test_that("replay compares individual output files and distinguishes empty, absent and unrecorded directories", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  yaml::write_yaml(
    list(
      directories = list(
        output = list(path = "result"),
        docs = list(path = "site"),
        `output-new` = list(path = "new"),
        `output-no-hashes` = list(path = "untracked"),
        `raw-data` = list(path = "inputs")
      )
    ),
    file.path(project, "_projr.yml")
  )
  fs::dir_create(file.path(
    project,
    c("result/sub", "site", "new", "untracked")
  ))
  for (file in c(
    "result/.same",
    "result/sub/changed",
    "result/extra",
    "new/new"
  )) {
    writeLines(file, file.path(project, file))
  }
  usethis::with_project(
    project,
    {
      hashes <- .restore_build_hashes(.path_get("result"))
      expected <- c(
        hashes[".same"],
        "sub/changed" = hashes[[1L]],
        missing = hashes[[1L]]
      )
      baseline <- list(
        output = expected,
        docs = character(),
        `output-gone` = character(),
        `output-no-hashes` = NULL
      )
      comparison <- .reproduce_compare(baseline)
      expect_false(comparison$exact)
      dirs <- comparison$directories
      rownames(dirs) <- dirs$label
      expect_true(dirs["docs", "exact"])
      expect_false(dirs["output-gone", "exact"])
      expect_identical(dirs["output-gone", "status"], "missing_directory")
      expect_identical(dirs["output-new", "status"], "extra_directory")
      expect_identical(dirs["output-no-hashes", "status"], "unrecorded")
      expect_true(is.na(dirs["output-no-hashes", "identical"]))
      expect_identical(
        unlist(
          dirs["output", c("identical", "changed", "missing", "extra")],
          use.names = FALSE
        ),
        rep(1L, 4)
      )
      expect_identical(
        comparison$files$path,
        c(".same", "extra", "missing", "sub/changed", "new")
      )
      expect_identical(
        comparison$files$status,
        c("identical", "extra", "missing", "changed", "extra")
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("replay reads the exact build's merged directories and recovers matching lists from metadata alone", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  writeLines(
    c("directories:", "  output-new: {path: added}"),
    file.path(project, "_projr.yml")
  )
  fs::dir_create(file.path(project, ".projr"))
  writeLines(
    c(
      "## v1.0.0: First",
      "**`projr` config**",
      "```yaml",
      "directories:",
      "  docs: {path: docs}",
      "  output: {path: results}",
      "  output-empty: {path: empty}",
      "  output-disabled: {path: disabled, hash: false}",
      "  raw-data: {path: raw}",
      "remotes:",
      "  local:",
      "    reports: {path: shared}",
      "```"
    ),
    file.path(project, ".projr", "BUILDLOG.md")
  )
  usethis::with_project(
    project,
    {
      rows <- data.frame(
        label = c("docs", "output", "output-empty"),
        fn = c("index.html", "report.txt", ""),
        hash = c(strrep("a", 32), strrep("b", 32), ""),
        version = "v1.0.0"
      )
      store <- .manifest_project_dir()
      .manifest_store_write(rows, store, legacy_path = NULL)
      # The archive first received the same output files during a later build.
      archived <- rows[2L, ]
      archived$version <- "v1.1.0"
      .manifest_store_write(
        archived,
        .path_get("shared", "manifests"),
        legacy_path = NULL
      )
      unlink(list.files(
        file.path(store, "output"),
        pattern = "^files-",
        full.names = TRUE
      ))
      baseline <- .reproduce_baseline("1.0.0", destination = "reports")
      expect_identical(
        names(baseline),
        c("docs", "output", "output-empty", "output-disabled")
      )
      expect_identical(baseline$output, c(report.txt = strrep("b", 32)))
      expect_identical(baseline$docs, c(index.html = strrep("a", 32)))
      expect_identical(baseline[["output-empty"]], character())
      expect_null(baseline[["output-disabled"]])
      archived$version <- "v1.0.0"
      archived$hash <- strrep("c", 32)
      .manifest_store_write(
        archived,
        .path_get("shared", "manifests"),
        legacy_path = NULL
      )
      expect_error(
        .reproduce_baseline("1.0.0", "reports"),
        "Project and archive disagree"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("legacy records compare verified files without using a remote", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  writeLines(
    c("directories:", "  output: {path: results}"),
    file.path(project, "_projr.yml")
  )
  fs::dir_create(file.path(project, "results"))
  writeLines("one", file.path(project, "results", "x"))
  usethis::with_project(
    project,
    {
      rows <- data.frame(
        label = "output",
        version = "v1.0.0",
        fn = "x",
        hash = unname(tools::md5sum(.path_get("results", "x")))
      )
      .manifest_write(rows, .path_get("manifest.csv"))
      testthat::local_mocked_bindings(.reproduce_archive_metadata = function(
        ...
      ) {
        stop("unexpected remote read")
      })
      result <- .reproduce_compare(.reproduce_baseline("1.0.0"))
      expect_true(result$exact)
      expect_identical(result$files$status, "identical")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("an explicit GitHub archive supplies only the requested build's manifest", {
  skip_if(.is_test_select())
  .test_local_github_releases(c("reports-v1.0.0", "archive"))
  project <- withr::local_tempdir()
  writeLines(
    c(
      "directories:",
      "  output: {path: results}",
      "remotes:",
      "  github:",
      '    reports: {repo: fixture/project, tag: "reports-@version"}'
    ),
    file.path(project, "_projr.yml")
  )
  usethis::with_project(
    project,
    {
      rows <- data.frame(
        label = "output",
        version = "v1.0.0",
        fn = "x",
        hash = strrep("d", 32)
      )
      .remote_write_manifest("github", list(tag = "reports-v1.0.0"), rows)
      rows$hash <- strrep("e", 32)
      .remote_write_manifest("github", list(tag = "archive"), rows)
      expect_identical(
        .reproduce_baseline("1.0.0", "reports")$output,
        c(x = strrep("d", 32))
      )
      expect_identical(
        .reproduce_baseline("2.0.0", "reports"),
        list(output = NULL)
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})
