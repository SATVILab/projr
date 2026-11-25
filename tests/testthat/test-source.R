# A local archive with two completed builds of raw-data:
# v0.1.0 has x.txt = "a", y.txt = "b"; v0.2.0 has x.txt = "a2".
.test_source_archive <- function(base) {
  archive <- file.path(base, "archive")
  files <- list(
    "v0.1.0" = c(x.txt = "a", y.txt = "b"),
    "v0.2.0" = c(x.txt = "a2")
  )
  rows <- list()
  for (version in names(files)) {
    dir <- file.path(archive, "raw-data", version)
    fs::dir_create(dir)
    for (fn in names(files[[version]])) {
      writeLines(files[[version]][[fn]], file.path(dir, fn))
    }
    rows[[version]] <- data.frame(
      label = "raw-data",
      version = version,
      hash = unname(tools::md5sum(file.path(dir, names(files[[version]])))),
      fn = names(files[[version]])
    )
  }
  .manifest_store_write(
    do.call(rbind, rows),
    file.path(archive, "manifests"),
    legacy_path = NULL
  )
  writeLines("v0.2.0", file.path(archive, "VERSION"))
  archive
}

.test_source_project <- function(base, archive, source) {
  project <- file.path(base, "consumer")
  fs::dir_create(file.path(project, "inputs"))
  yaml::write_yaml(
    list(
      directories = list(`raw-data` = list(path = "inputs")),
      remotes = list(local = list(upstream = list(path = archive))),
      build = list(source = source)
    ),
    file.path(project, "_projr.yml")
  )
  writeLines("v0.0.1-1", file.path(project, "VERSION"))
  project
}

.test_source_run <- function() {
  workspace <- withr::local_tempdir(.local_envir = parent.frame())
  .source_install(.source_plan(workspace))
}

test_that("build.source fetches, updates stale files and stops on local edits", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  archive <- .test_source_archive(base)
  project <- .test_source_project(base, archive, "upstream")
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    x <- file.path("inputs", "x.txt")

    # An empty folder fetches the latest completed build.
    .test_source_run()
    expect_identical(readLines(x), "a2")
    expect_false(file.exists(file.path("inputs", "y.txt")))
    record <- .source_record_read("raw-data")
    expect_identical(record$version, "v0.2.0")
    expect_identical(record$remote, "local:upstream")

    # With files present, if-empty leaves the folder alone without
    # contacting the remote, so it works offline.
    config <- .yml_get("default")
    config$remotes$local$upstream$path <- file.path(base, "missing")
    .yml_set(config)
    expect_identical(
      .source_plan(withr::local_tempdir())[[1]]$status,
      "present"
    )
    config$remotes$local$upstream$path <- archive
    .yml_set(config)

    # A pinned older build replaces files that still match the last fetch.
    config <- .yml_get("default")
    config$build$source <- list(
      upstream = list(version = "0.1.0", cue = "if-change")
    )
    .yml_set(config)
    .test_source_run()
    expect_identical(readLines(x), "a")
    expect_identical(readLines(file.path("inputs", "y.txt")), "b")
    expect_identical(.source_record_read("raw-data")$version, "v0.1.0")

    # The same pin with matching files needs no network access.
    config$remotes$local$upstream$path <- file.path(base, "missing")
    .yml_set(config)
    plan <- .source_plan(withr::local_tempdir())[["raw-data"]]
    expect_identical(plan$status, "pinned")
    config$remotes$local$upstream$path <- archive
    .yml_set(config)

    # A licence projr writes into the folder is not a local change.
    config$directories$`raw-data`$license <- "CC-BY"
    config$build$source$upstream$strategy <- "sync-diff"
    config$build$source$upstream$version <- "0.2.0"
    .yml_set(config)
    writeLines("licence", file.path("inputs", "LICENSE"))
    .test_source_run()
    expect_identical(readLines(x), "a2")
    expect_true(file.exists(file.path("inputs", "LICENSE")))
    config$directories$`raw-data`$license <- NULL
    config$build$source$upstream$version <- "0.1.0"
    .yml_set(config)
    unlink(file.path("inputs", "LICENSE"))
    .test_source_run()

    # A local edit is a conflict before anything changes.
    writeLines("mine", x)
    config$build$source$upstream$version <- "0"
    .yml_set(config)
    expect_error(.source_plan(withr::local_tempdir()), "x.txt")
    expect_identical(readLines(x), "mine")

    # conflict: local keeps the edit and applies the rest.
    config$build$source$upstream$conflict <- "local"
    .yml_set(config)
    .test_source_run()
    expect_identical(readLines(x), "mine")
    expect_false(file.exists(file.path("inputs", "y.txt")))
    expect_identical(.source_record_read("raw-data")$kept, "x.txt")

    # conflict: remote replaces it.
    config$build$source$upstream$conflict <- "remote"
    .yml_set(config)
    .test_source_run()
    expect_identical(readLines(x), "a2")
  })
})

test_that("build.source strategies, versions and selections", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  archive <- .test_source_archive(base)
  project <- .test_source_project(
    base,
    archive,
    list(upstream = list(cue = "always", strategy = "download-all"))
  )
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    # Without a fetch record, an extra local file blocks a sync but not
    # download-all, which never deletes.
    writeLines("extra", file.path("inputs", "z.txt"))
    .test_source_run()
    expect_identical(readLines(file.path("inputs", "z.txt")), "extra")
    expect_identical(readLines(file.path("inputs", "x.txt")), "a2")
    config <- .yml_get("default")
    config$build$source$upstream$strategy <- "sync-diff"
    .yml_set(config)
    expect_error(.source_plan(withr::local_tempdir()), "z.txt")

    # download-missing never overwrites.
    unlink("inputs/z.txt")
    writeLines("old", file.path("inputs", "x.txt"))
    config$build$source$upstream <- list(
      cue = "always",
      strategy = "download-missing",
      version = "0.1.0"
    )
    .yml_set(config)
    .test_source_run()
    expect_identical(readLines(file.path("inputs", "x.txt")), "old")
    expect_identical(readLines(file.path("inputs", "y.txt")), "b")

    # cue: never skips the remote; unknown versions and labels fail.
    config$build$source$upstream <- list(cue = "never")
    .yml_set(config)
    expect_length(.source_plan(withr::local_tempdir()), 0)
    config$build$source$upstream <- list(version = "1", cue = "if-change")
    .yml_set(config)
    expect_error(.source_plan(withr::local_tempdir()), "matching version 1")
    config$directories$`raw-extra` <- list(path = "extra")
    config$build$source$upstream <- list(content = list("raw-extra"))
    .yml_set(config)
    expect_error(.source_plan(withr::local_tempdir()), "no raw-extra")

    # Overlapping directories are rejected before any transfer.
    config$directories$`raw-extra` <- list(path = "inputs/sub")
    config$build$source$upstream <- list(
      content = list("raw-data", "raw-extra")
    )
    .yml_set(config)
    expect_error(.source_plan(withr::local_tempdir()), "overlapping")
  })
})

test_that("a build fetches inputs before hashing them and logs the source", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  base <- withr::local_tempdir()
  archive <- .test_source_archive(base)
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$remotes <- list(local = list(upstream = list(path = archive)))
    config$build$source <- list(
      upstream = list(content = list("raw-data"), cue = "if-change")
    )
    config$build$scripts <- list("copy.R")
    .yml_set(config)
    writeLines(
      c(
        "file.copy(",
        "  projr::projr_path_get('raw-data', 'x.txt'),",
        "  projr::projr_path_get('output', 'x.txt')",
        ")"
      ),
      "copy.R"
    )
    unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
    writeLines("stale", projr_path_get("raw-data", "x.txt", safe = FALSE))
    # The stale file has no fetch record, so it is a conflict.
    expect_error(
      projr_build_patch("fetch", project = project),
      "x.txt"
    )
    config$build$source$upstream$conflict <- "remote"
    .yml_set(config)
    projr_build_patch("fetch", project = project)
    expect_identical(
      readLines(projr_path_get("output", "x.txt", safe = FALSE)),
      "a2"
    )
    manifest <- .manifest_store_index(.manifest_project_dir(), "raw-data")
    expect_true(nrow(manifest) > 0)
    files <- .restore_build_snapshot(
      .manifest_project_dir(),
      NULL,
      "raw-data",
      utils::tail(manifest$version, 1L)
    )$files
    expect_identical(
      unname(files[["x.txt"]]),
      unname(tools::md5sum(file.path(archive, "raw-data", "v0.2.0", "x.txt")))
    )
    expect_true(any(grepl("Raw-input sources", .buildlog_read())))
  })
})

test_that("deleting a planned file never expands wildcards", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  project <- .test_source_project(base, base, "upstream")
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    writeLines("keep", file.path("inputs", "a1.csv"))
    writeLines("drop", file.path("inputs", "a[1].csv"))
    fs::dir_create(file.path("inputs", "sub", "deep"))
    writeLines("drop", file.path("inputs", "sub", "deep", "b.csv"))
    staging <- withr::local_tempdir()
    writeLines("file", file.path(staging, "sub"))
    .source_install(list(list(
      label = "raw-data",
      remote = "local:upstream",
      path = "inputs",
      version = "v0.1.0",
      status = "update",
      staging = staging,
      write = "sub",
      delete = c("a[1].csv", "sub/deep/b.csv"),
      files = character()
    )))
    expect_true(file.exists(file.path("inputs", "a1.csv")))
    expect_false(file.exists(file.path("inputs", "a[1].csv")))
    # Emptied folders go, so a remote file can replace a local folder.
    expect_identical(readLines(file.path("inputs", "sub")), "file")
  })
})

test_that("build.source fetches from a GitHub release", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  producer <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  .test_local_github_releases("published", repo = "fixture/destination")
  usethis::with_project(producer, {
    config <- .yml_get("default")
    config$build$scripts <- character()
    config$remotes <- list(
      github = list(
        share = list(repo = "fixture/destination", tag = "published")
      )
    )
    config$build$dest <- list(share = list(content = "raw-data"))
    .yml_set(config)
    projr_version_set("0.1.0-1")
    raw <- projr_path_get_dir("raw-data", safe = FALSE)
    unlink(raw, recursive = TRUE)
    writeLines("a", projr_path_get("raw-data", "x.txt", safe = FALSE))
    writeLines("b", projr_path_get("raw-data", "y.txt", safe = FALSE))
    projr_build_patch("first", project = producer)
    writeLines("a2", file.path(raw, "x.txt"))
    unlink(file.path(raw, "y.txt"))
    projr_build_patch("second", project = producer)
  })

  base <- withr::local_tempdir()
  consumer <- file.path(base, "consumer")
  fs::dir_create(file.path(consumer, "inputs"))
  yaml::write_yaml(
    list(
      directories = list(`raw-data` = list(path = "inputs")),
      remotes = list(
        github = list(
          upstream = list(repo = "fixture/destination", tag = "published")
        )
      ),
      build = list(source = "upstream")
    ),
    file.path(consumer, "_projr.yml")
  )
  writeLines("0.0.1-1", file.path(consumer, "VERSION"))
  usethis::with_project(consumer, quiet = TRUE, force = TRUE, {
    x <- file.path("inputs", "x.txt")
    .test_source_run()
    expect_identical(readLines(x), "a2")
    expect_false(file.exists(file.path("inputs", "y.txt")))
    expect_identical(.source_record_read("raw-data")$version, "v0.1.2")

    config <- .yml_get("default")
    config$build$source <- list(
      upstream = list(version = "0.1.1", cue = "if-change")
    )
    .yml_set(config)
    .test_source_run()
    expect_identical(readLines(x), "a")
    expect_identical(readLines(file.path("inputs", "y.txt")), "b")
    expect_identical(.source_plan(withr::local_tempdir())[[1]]$status, "pinned")
  })
})

test_that("projr_source fetches, discovers remotes and resolves conflicts", {
  skip_if(.is_test_select())
  base <- withr::local_tempdir()
  archive <- .test_source_archive(base)
  project <- .test_source_project(base, archive, NULL)
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    x <- file.path("inputs", "x.txt")
    config <- .yml_get("default")
    config$build$source <- NULL
    .yml_set(config)

    # Without build.source, the one remote holding raw-data supplies it.
    projr_source()
    expect_identical(readLines(x), "a2")

    # Arguments override settings; an explicit call updates a non-empty folder.
    projr_source(version = "0.1.0")
    expect_identical(readLines(x), "a")

    # A local edit errors without a choice, and asks interactively.
    writeLines("mine", x)
    expect_error(projr_source(), class = "projr_source_conflict")
    expect_identical(readLines(x), "mine")
    local_mocked_bindings(
      .is_interactive_and_not_test = function() TRUE,
      .source_conflict_choice = function(message) "local"
    )
    projr_source()
    expect_identical(readLines(x), "mine")
    expect_false(file.exists(file.path("inputs", "y.txt")))
    local_mocked_bindings(.source_conflict_choice = function(message) NULL)
    writeLines("again", x)
    config$build$source <- list(upstream = list(conflict = "error"))
    .yml_set(config)
    expect_error(projr_source(version = "0.2.0"), "Cancelled")
    expect_identical(readLines(x), "again")
    projr_source(conflict = "remote")
    expect_identical(readLines(x), "a2")

    # Two registered remotes holding the same label need a choice.
    config$build$source <- NULL
    config$remotes$local$mirror <- list(path = archive)
    .yml_set(config)
    expect_error(projr_source(), "local:upstream and local:mirror")
    projr_source(remote = "mirror", version = "0.1.0", conflict = "remote")
    expect_identical(readLines(x), "a")
    expect_identical(.source_record_read("raw-data")$remote, "local:mirror")
    expect_error(projr_source(remote = "missing"), "not declared")
    expect_error(projr_source(version = "next"), "version must be")

    # version = "current" restores the checkout's recorded build.
    called <- NULL
    local_mocked_bindings(projr_restore = function(...) {
      called <<- list(...)
      invisible(TRUE)
    })
    expect_error(
      projr_source(version = "current", remote = "mirror"),
      "GitHub release"
    )
    projr_source("raw-data", version = "current", conflict = "remote")
    expect_identical(
      called,
      list(label = "raw-data", destination = "archive", clear = TRUE)
    )
  })
})
