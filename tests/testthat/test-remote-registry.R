test_that("registered remotes resolve independently of the selected publications", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$build$local <- NULL
    config$build$github <- NULL
    config$remotes <- list(
      local = list(backup = list(path = "archives/@version")),
      github = list(
        shared = list(repo = "other/archive", tag = "snapshot-@version"),
        reader = list()
      )
    )
    .yml_set(config)
    projr_version_set("1.2.3")
    expect_true(projr_yml_check())
    expect_length(.dest_send_get_type(FALSE, FALSE), 0)
    expect_length(.dest_github_targets(FALSE, TRUE, NULL), 0)
    remote <- .yml_dest_get_title_complete(
      "shared",
      "github",
      NULL,
      FALSE,
      TRUE
    )
    expect_identical(remote$id, "snapshot-v1.2.3")
    expect_identical(remote$repo, "other/archive")
    expect_null(.yml_remote_definition("github", "missing"))
    expect_identical(
      .yml_dest_get_title_complete("reader", "github", NULL, FALSE, TRUE)$id,
      "reader"
    )
    expect_identical(.buildlog_get_destinations(), character())
    before <- readLines("_projr.yml")
    expect_error(
      projr_yml_dest_add_local("backup", "docs", "new-path", overwrite = FALSE),
      "already exists"
    )
    expect_identical(readLines("_projr.yml"), before)

    config$build$local <- list(backup = list(content = "raw-data"))
    config$build$github <- list(shared = NULL)
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_identical(
      .dest_send_title_get_content("backup", "local", FALSE),
      "raw-data"
    )
    expect_setequal(
      .dest_send_title_get_content("shared", "github", FALSE),
      .dest_send_title_get_content_auto()
    )
    expect_false(
      "cache" %in% .dest_send_title_get_content("shared", "github", FALSE)
    )
    expect_true(any(grepl(
      "snapshot-v1.2.3",
      .buildlog_get_destinations(),
      fixed = TRUE
    )))
    expect_true(any(grepl(
      "archives/v1.2.3",
      .buildlog_get_destinations(),
      fixed = TRUE
    )))
    config$build$github$shared <- list(content = character())
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_length(.dest_github_tags_needed(FALSE, TRUE), 0)
    expect_length(.remote_ls_dest(), 0)
    expect_false(any(grepl(
      "snapshot-v1.2.3",
      .buildlog_get_destinations(),
      fixed = TRUE
    )))
    config$build$github["shared"] <- list(NULL)
    config$build$github["reports"] <- list(NULL)
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_setequal(
      .dest_send_title_get_content("reports", "github", FALSE),
      .dest_send_title_get_content_auto()
    )
    expect_identical(
      .yml_dest_get_title_complete("reports", "github", NULL, FALSE, TRUE)$id,
      "reports"
    )
    config$build$local["no-path"] <- list(NULL)
    .yml_set(config)
    expect_error(projr_yml_check(), "path")
    config$build$local[["no-path"]] <- NULL
    config$build$github[["reports"]] <- NULL
    .yml_set(config)
    writeLines(
      c(
        "remotes:",
        "  github:",
        "    shared:",
        "      tag: later",
        "build:",
        "  local:",
        "    backup: {}"
      ),
      "_projr-local.yml"
    )
    expect_true(projr_yml_check())
    expect_identical(.dest_github_tags_needed(FALSE, TRUE), "later")
    expect_identical(
      .yml_remote_definition("github", "shared")$repo,
      "other/archive"
    )
    expect_setequal(
      .dest_send_title_get_content("backup", "local", FALSE),
      .dest_send_title_get_content_auto()
    )
    writeLines(c("build:", "  local:", "    backup:"), "_projr-local.yml")
    expect_setequal(
      .dest_send_title_get_content("backup", "local", FALSE),
      .dest_send_title_get_content_auto()
    )

    unlink("_projr-local.yml")
    config$build$github$shared <- list(tag = "inline")
    .yml_set(config)
    expect_error(projr_yml_check(), "remotes.github.shared")
    config$build$github$shared <- list(content = "cache")
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_identical(
      .dest_send_title_get_content("shared", "github", FALSE),
      "cache"
    )
    config$build$github$shared <- list(content = "absent")
    .yml_set(config)
    expect_error(projr_yml_check(), "absent")
    projr_yml_dest_add_github("shared", "docs", send_strategy = "upload-all")
    expect_true(projr_yml_check())
    rewritten <- .yml_get("default")
    expect_identical(rewritten$build$github$shared, list(content = "docs"))
    expect_identical(rewritten$remotes$github$shared$repo, "other/archive")
    expect_identical(rewritten$remotes$github$shared$tag, "snapshot-@version")
    expect_identical(
      rewritten$remotes$github$shared$send$strategy,
      "upload-all"
    )
    config$build$github$shared <- list(content = "raw-data")
    config$remotes$github$shared$repo <- "../../foo"
    .yml_set(config)
    expect_error(projr_yml_check(), "owner/repository")
  })
})

test_that("GitHub preparation and asset transfer use the registered destination repository", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$remotes$github <- list(
      share = list(repo = "org/first", tag = "results", branch = "analysis"),
      copy = list(repo = "org/second", tag = "results"),
      archive = list(repo = "org/one-off", tag = "saved-@version"),
      hidden = list(repo = "org/reader", tag = "results")
    )
    config$build$github <- list(
      share = list(content = "raw-data"),
      copy = list(content = "docs")
    )
    .yml_set(config)
    projr_version_set("1.2.3")
    expect_true(projr_yml_check())
    created <- sent <- character()
    local_mocked_bindings(
      .remote_check_exists = function(type, id) FALSE,
      .remote_create = function(type, id, branch = NULL) {
        created <<- c(
          created,
          paste(.gh_repo_get(), id, branch %||% "", sep = "/")
        )
      },
      .dest_send_label = function(label, title, ...) {
        sent <<- c(sent, paste(.gh_repo_get(), title, label, sep = "/"))
      }
    )
    .dest_prepare_github_releases("patch", "raw-data", FALSE, TRUE, FALSE)
    expect_setequal(
      created,
      c(
        "org/first/results/analysis",
        "org/second/results/",
        "org/one-off/saved-v1.2.3/"
      )
    )
    .dest_send("patch", "raw-data", FALSE, TRUE)
    expect_setequal(
      sent,
      c(
        "org/first/share/raw-data",
        "org/second/copy/docs",
        "org/one-off/archive/raw-data"
      )
    )
    expect_null(getOption("projr.github.repo"))
    expect_true(any(grepl(
      "saved-v1.2.3",
      .buildlog_get_destinations("raw-data", FALSE),
      fixed = TRUE
    )))
    expect_error(.with_github_repo("other/repo", stop("test")), "test")
    expect_null(getOption("projr.github.repo"))
  })
})

test_that("a selected registered local remote publishes the chosen content", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$build$scripts <- character()
    config$build$github <- NULL
    config$build$local <- list(send = list(content = "raw-data"))
    config$remotes$local <- list(
      send = list(
        path = "backup",
        structure = "archive",
        send = list(strategy = "upload-all")
      ),
      shared = list(path = "read-only")
    )
    .yml_set(config)
    projr_version_set("1.2.2-1")
    dir.create(config$directories[["raw-data"]]$path, showWarnings = FALSE)
    writeLines(
      "sample",
      file.path(config$directories[["raw-data"]]$path, "sample.txt")
    )
    projr_build_patch("check local publication", project = project)
    contents <- list.files("backup", recursive = TRUE)
    sample <- grep("sample.txt$", contents, value = TRUE)
    expect_length(sample, 1L)
    expect_identical(readLines(file.path("backup", sample)), "sample")
    expect_false(any(grepl("^docs/", contents)))
    expect_false(dir.exists("read-only"))
    projr_ignore_auto()
    rules <- readLines(".gitignore")
    expect_true("/backup/**" %in% rules)
    expect_false("/read-only/**" %in% rules)
  })
})


test_that("registered publishers send assets and metadata to the named repository", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  .test_local_github_releases(
    "published",
    repo = "fixture/destination",
    checkout_repo = "fixture/source"
  )
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$build$scripts <- character()
    config$build$local <- NULL
    config$build$github <- list(share = list(content = "raw-data"))
    config$remotes$github <- list(
      share = list(repo = "fixture/destination", tag = "published")
    )
    .yml_set(config)
    projr_version_set("1.2.2-1")
    dir.create(config$directories[["raw-data"]]$path, showWarnings = FALSE)
    writeLines(
      "sample",
      file.path(config$directories[["raw-data"]]$path, "sample.txt")
    )
    projr_build_patch("test external repository", project = project)
    expect_identical(.gh_repo_get(), "fixture/source")
    assets <- .remote_ls_final_github_httr("fixture/destination", "published")
    expect_setequal(
      assets,
      c("raw-data-v1.2.3.zip", "manifest.zip", "VERSION.zip", "CHANGELOG.zip")
    )
  })
})


test_that("creating an empty release asset preserves the selected target branch", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$remotes$github <- list(
      archive = list(repo = "org/one-off", branch = "private", tag = "saved")
    )
    .yml_set(config)
    projr_version_set("1.2.3")
    created <- list()
    local_mocked_bindings(
      .remote_final_check_exists_direct = function(...) FALSE,
      .remote_check_exists = function(...) FALSE,
      .remote_create = function(type, id, branch = NULL, ...) {
        created <<- list(repo = .gh_repo_get(), tag = id, branch = branch)
      },
      .remote_file_add_github = function(...) TRUE
    )
    target <- .yml_dest_get_title_complete(
      "archive",
      "github",
      NULL,
      TRUE,
      TRUE
    )
    .with_github_repo(target$repo, branch = target$branch, {
      .remote_final_empty_get(
        "github",
        target$id,
        "raw-data",
        "archive",
        version = "1.2.3"
      )
    })
    expect_identical(
      created,
      list(repo = "org/one-off", tag = "saved", branch = "private")
    )
  })
})
