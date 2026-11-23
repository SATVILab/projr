test_that("a local analysis can be read without Git, and older data can come from its archive", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  project <- file.path(base, "producer")
  fs::dir_create(file.path(project, "inputs"))
  writeLines(c(
    "directories:", "  raw-data: {path: inputs}", "remotes:",
    "  local:", "    shared: {path: history}"
  ), file.path(project, "_projr.yml"))
  writeLines("v0.2.0-1", file.path(project, "VERSION"))
  input <- file.path(project, "inputs", "x.txt")
  writeLines("new", input)
  archived <- file.path(project, "history", "raw-data", "v0.1.0")
  fs::dir_create(archived)
  writeLines("old", file.path(archived, "x.txt"))
  hashes <- unname(tools::md5sum(c(file.path(archived, "x.txt"), input)))
  rows <- data.frame(
    label = "raw-data", version = c("v0.1.0", "v0.1.1", "v0.2.0"),
    hash = hashes[c(1, 1, 2)], fn = "x.txt"
  )
  .manifest_store_write(rows, file.path(project, ".projr", "manifests"), legacy_path = NULL)
  .manifest_store_write(rows[1L, ], file.path(project, "history", "manifests"), legacy_path = NULL)
  writeLines(
    c(
      "#### v0.2.0: build", "### v0.1.1: earlier build", "**`projr` config**",
      "", "```yaml", "directories:", "  raw-data: {path: inputs}", "```"
    ),
    file.path(project, ".projr", "BUILDLOG.md")
  )

  withr::local_dir(base)
  dir <- projr_get("local", path = project, out = "download")
  expect_named(dir, "raw-data")
  dest <- file.path(dir[["raw-data"]], "x.txt")
  expect_identical(readLines(dest), "new")
  expect_error(projr_get_local(project, version = "0.1.1", out = "download"), "overwrite")
  projr_get_local(project, version = "0.1.1", out = "download", overwrite = TRUE)
  expect_identical(readLines(dest), "old")
  writeLines("my copy", dest)
  # Later uploads may serve an earlier version only when their published index
  # identifies the same snapshot, even if someone put matching bytes elsewhere.
  later <- file.path(dirname(archived), "v0.9.0")
  fs::file_move(archived, later)
  row <- rows[3L, ]
  row$version <- "v0.9.0"
  .manifest_store_write(row, file.path(project, "history", "manifests"), legacy_path = NULL)
  expect_error(
    projr_get_local(project, version = "0.1.1", out = "download", overwrite = TRUE),
    "No matching files"
  )
  row <- rows[1L, ]
  row$version <- "v0.9.1"
  .manifest_store_write(row, file.path(project, "history", "manifests"), legacy_path = NULL)
  fs::file_move(later, file.path(dirname(archived), "v0.9.1"))
  projr_get_local(project, version = "0.1.1", out = "download", overwrite = TRUE)
  expect_identical(readLines(dest), "old")
  writeLines("changed archived data", file.path(dirname(archived), "v0.9.1", "x.txt"))
  writeLines("my copy", dest)
  expect_error(
    projr_get_local(project, version = "0.1.1", out = "download", overwrite = TRUE),
    "No matching files"
  )
  expect_identical(readLines(dest), "my copy")
  expect_false(dir.exists(file.path(project, ".git")))
})

test_that("an unselected local remote serves its recorded build including a flat latest directory", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  fs::dir_create(file.path(base, "store"))
  writeLines(c(
    "settings: {hashing: {approach: legacy, cores: 2}}",
    "remotes:", "  local:", "    reports:",
    "      path: store", "      path-append-label: false",
    "      structure: latest"
  ), file.path(base, "_projr.yml"))
  source <- file.path(base, "store", "table.csv")
  writeLines("x,y", source)
  writeLines(c("Project: 2.0.0", "output: 2.0.0"), file.path(base, "store", "VERSION"))
  rows <- data.frame(
    label = "output", version = "v2.0.0", fn = "table.csv",
    hash = unname(tools::md5sum(source))
  )
  .manifest_store_write(rows, file.path(base, "store", "manifests"), legacy_path = NULL)
  settings <- NULL
  original_hash <- .hash_file
  testthat::local_mocked_bindings(.hash_file = function(fn, engine, cores) {
    settings <<- list(approach = engine, cores = cores)
    original_hash(fn, engine, cores)
  })
  usethis::with_project(base,
    {
      output <- projr_get_local(remote = "reports", out = file.path(base, "out"))
      expect_identical(list.files(output), "table.csv")
      expect_identical(settings, list(approach = "legacy", cores = 2L))
      expect_error(projr_get_local(path = base, remote = "reports"), "exactly one")
      expect_error(projr_get_local(remote = "typo"), "Unknown local remote")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("GitHub retrieval verifies staged files and protects all selected destinations", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  .test_local_github_releases("reports")
  base <- withr::local_tempdir()
  withr::local_dir(base)
  writeLines("old report", "report.txt")
  rows <- data.frame(
    label = c("raw-data", "output"), version = "v1.0.0", fn = "report.txt",
    hash = unname(tools::md5sum("report.txt"))
  )
  .remote_write_manifest("github", list(tag = "reports"), rows)
  rows$version <- "v2.0.0"
  .remote_write_manifest("github", list(tag = "reports"), rows)
  .remote_write_version_file("github", list(tag = "reports"), c("Project: 2.0.0", "output: 2.0.0"))
  with_dir(base, utils::zip("prev.zip", "report.txt", flags = "-q"))
  .gh_release_asset_upload_httr("fixture/project", "reports", "prev.zip", asset_name = "raw-data-v1.0.0.zip")
  .gh_release_asset_upload_httr("fixture/project", "reports", "prev.zip", asset_name = "output-v1.0.0.zip")
  result <- projr_get_github(repo = "fixture/project", tag = "reports", out = "out")
  expect_named(result, c("output", "raw-data"))
  expect_identical(readLines(file.path(result["output"], "report.txt")), "old report")
  writeLines("retain my work", file.path(result["output"], "report.txt"))
  expect_error(projr_get_github(repo = "fixture/project", tag = "reports", out = "out"), "overwrite")
  writeLines("damaged asset", "report.txt")
  utils::zip("bad.zip", "report.txt", flags = "-q")
  .gh_release_asset_upload_httr("fixture/project", "reports", "bad.zip", asset_name = "raw-data-v1.0.0.zip")
  unlink(file.path(result["raw-data"], "report.txt"))
  expect_error(projr_get_github(
    repo = "fixture/project", tag = "reports", out = "out",
    overwrite = TRUE
  ), "file hash")
  expect_identical(readLines(file.path(result["output"], "report.txt")), "retain my work")
  expect_false(file.exists(file.path(result["raw-data"], "report.txt")))
  expect_error(projr_get_github(repo = "../x"), "owner/repo")
  expect_error(projr_get_github(repo = "fixture/project", label = "../x"), "directory name")
})

test_that("read-only discovery preserves the producer's inputs and materializes empty snapshots", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  project <- file.path(base, "producer")
  fs::dir_create(file.path(project, "docs"))
  writeLines("keep this", file.path(project, "docs", "original"))
  writeLines(c(
    "directories:", "  raw-data: {path: docs}", "  docs: {path: reports}",
    "remotes:", "  local:", "    unused: {path: 'elsewhere-@uuid'}"
  ), file.path(project, "_projr.yml"))
  writeLines("v1.0.0", file.path(project, "VERSION"))
  .manifest_store_write(data.frame(label = "docs", fn = "", hash = "", version = "v1.0.0"),
    file.path(project, ".projr", "manifests"),
    legacy_path = NULL
  )
  for (out in c(project, file.path(project, "docs"))) {
    expect_error(projr_get_local(project, label = "docs", out = out, overwrite = TRUE), "overlaps")
  }
  expect_identical(list.files(file.path(project, "docs"), all.files = TRUE, no.. = TRUE), "original")
  expect_false(dir.exists(file.path(project, "reports")))
  downloaded <- projr_get_local(project, label = "docs", out = file.path(base, "retrieved"))
  expect_true(dir.exists(downloaded))
  expect_length(list.files(downloaded), 0L)
})

test_that("the producer branch and completed build select its recorded external release", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  title <- "reports-v1.0.0-feature_2fa-uuid"
  .test_local_github_releases(title, repo = "fixture/storage")
  base <- withr::local_tempdir()
  withr::local_dir(base)
  writeLines("result", "result.txt")
  rows <- data.frame(
    label = "output", fn = "result.txt", version = "v1.0.0",
    hash = unname(tools::md5sum("result.txt"))
  )
  .remote_write_manifest("github", list(tag = title), rows)
  utils::zip("output.zip", "result.txt", flags = "-q")
  .gh_release_asset_upload_httr("fixture/storage", title, "output.zip", asset_name = "output-v1.0.0.zip")
  record <- c(
    "### v1.0.0: build", "", "**`projr` config**", "", "```yaml", "remotes:",
    "  github:", "    reports:", "      repo: fixture/storage",
    "      tag: reports-@version-@branch-@uuid", "```",
    "**Resolved archive destinations**", "", "```yaml", "github:", "  reports:",
    paste0("    destination: ", title), "```"
  )
  sha <- paste(rep("c", 40L), collapse = "")
  requests <- character()
  previous <- httr::set_callback("request", function(req) {
    if (!grepl("/repos/fixture/producer/", req$url, fixed = TRUE)) {
      return(previous(req))
    }
    url <- httr::parse_url(req$url)
    requests <<- c(requests, req$url)
    if (startsWith(url$path, "repos/fixture/producer/commits/")) {
      expect_identical(utils::URLdecode(sub(".*/commits/", "", url$path)), "feature/a")
      return(.test_github_response(200, list(sha = sha)))
    }
    if (endsWith(url$path, "/releases/tags/archive")) {
      return(.test_github_response(404))
    }
    expect_identical(url$query$ref, sha)
    if (!endsWith(url$path, "/contents/.projr/BUILDLOG.md")) {
      return(.test_github_response(404))
    }
    response <- .test_github_response(200)
    response$content <- charToRaw(paste(record, collapse = "\n"))
    response
  })
  withr::defer(httr::set_callback("request", previous))
  withr::local_envvar(c(PROJR_BUILD_VERSION = "v9.9.9", PROJR_BUILD_HEAD = "ref: refs/heads/consumer"))
  destination <- projr_get_github(repo = "fixture/producer@feature/a", label = "output", out = "out")
  expect_identical(readLines(file.path(destination, "result.txt")), "result")
  expect_true(any(grepl("/contents/.projr/BUILDLOG.md", requests, fixed = TRUE)))
  consumer <- file.path(base, "consumer")
  fs::dir_create(consumer)
  yaml::write_yaml(
    list(remotes = list(github = list(reports = list(repo = "fixture/storage", tag = title)))),
    file.path(consumer, "_projr.yml")
  )
  usethis::with_project(consumer,
    {
      from_registry <- projr_get_github(
        remote = "reports", version = "1.0.0", label = "output",
        out = file.path(base, "registry")
      )
      expect_identical(readLines(file.path(from_registry, "result.txt")), "result")
    },
    quiet = TRUE,
    force = TRUE
  )
  expect_identical(
    .get_remote_target(
      "github", "reports", list(tag = "x-@version-@branch"),
      NULL, "v1.2.3", "ref: refs/heads/feature/a"
    ),
    "x-v1.2.3-feature_2fa"
  )
})
