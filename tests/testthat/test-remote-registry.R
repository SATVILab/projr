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

    config$build$dest <- list(
      backup = list(content = "raw-data"),
      shared = NULL
    )
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
    config$build$dest$shared <- list(content = character())
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_length(.dest_github_tags_needed(FALSE, TRUE), 0)
    expect_length(.remote_ls_dest(), 0)
    expect_false(any(grepl(
      "snapshot-v1.2.3",
      .buildlog_get_destinations(),
      fixed = TRUE
    )))
    config$build$dest["shared"] <- list(NULL)
    config$remotes$github$reports <- list()
    config$build$dest["reports"] <- list(NULL)
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
    config$remotes$local["no-path"] <- list(list())
    config$build$dest["no-path"] <- list(NULL)
    .yml_set(config)
    expect_error(projr_yml_check(), "path")
    config$remotes$local[["no-path"]] <- NULL
    config$build$dest[["no-path"]] <- NULL
    config$remotes$github[["reports"]] <- NULL
    config$build$dest[["reports"]] <- NULL
    .yml_set(config)
    writeLines(
      c(
        "remotes:",
        "  github:",
        "    shared:",
        "      tag: later",
        "build:",
        "  dest:",
        "    backup: {}",
        "    shared:"
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
    writeLines(c("build:", "  dest:", "    backup:"), "_projr-local.yml")
    expect_setequal(
      .dest_send_title_get_content("backup", "local", FALSE),
      .dest_send_title_get_content_auto()
    )

    unlink("_projr-local.yml")
    config$build$dest$shared <- list(tag = "inline")
    .yml_set(config)
    expect_error(projr_yml_check(), "build.dest.shared")
    config$build$dest$shared <- list(content = "cache")
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_identical(
      .dest_send_title_get_content("shared", "github", FALSE),
      "cache"
    )
    config$build$dest$shared <- list(content = "absent")
    .yml_set(config)
    expect_error(projr_yml_check(), "absent")
    projr_yml_dest_add_github("shared", "docs", send_strategy = "upload-all")
    expect_true(projr_yml_check())
    rewritten <- .yml_get("default")
    expect_identical(rewritten$build$dest$shared, list(content = "docs"))
    expect_identical(rewritten$remotes$github$shared$repo, "other/archive")
    expect_identical(rewritten$remotes$github$shared$tag, "snapshot-@version")
    expect_identical(
      rewritten$remotes$github$shared$send$strategy,
      "upload-all"
    )
    config$build$dest$shared <- list(content = "raw-data")
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
    config$build$dest <- list(
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
    config$build$dest <- list(send = list(content = "raw-data"))
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
    config$build$dest <- list(share = list(content = "raw-data"))
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

test_that("build.dest selects registered remotes with overrides", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$build$local <- NULL
    config$build$github <- NULL
    config$remotes <- list(
      local = list(
        backup = list(path = "archives", send = list(cue = "if-change")),
        shared = list(path = "shared")
      ),
      github = list(shared = list(), reports = list())
    )
    config$build$dest <- "backup"
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_named(.yml_get(NULL)$build$local, "backup")
    expect_null(.yml_get(NULL)$build$dest)
    expect_setequal(
      .dest_send_title_get_content("backup", "local", FALSE),
      .dest_send_title_get_content_auto()
    )

    config$build$dest <- list(
      cue = "always",
      content = list("raw-data"),
      backup = NULL,
      "github:shared" = list(cue = "if-change", strategy = "upload-all")
    )
    .yml_set(config)
    expect_true(projr_yml_check())
    expect_identical(
      .dest_send_title_get_content("backup", "local", FALSE),
      "raw-data"
    )
    send <- .yml_dest_get_title_complete(
      "backup",
      "local",
      NULL,
      FALSE,
      FALSE
    )$send
    expect_identical(send$cue, "always")
    send <- .yml_dest_get_title_complete(
      "shared",
      "github",
      NULL,
      FALSE,
      FALSE
    )$send
    expect_identical(send$cue, "if-change")
    expect_identical(send$strategy, "upload-all")

    config$build$dest <- list("reports", list(backup = list(cue = "always")))
    .yml_set(config)
    # An empty selection in a higher layer turns uploads off.
    writeLines(c("build:", "  dest: []"), "_projr-local.yml")
    expect_null(.yml_get(NULL)$build$github)
    expect_null(.yml_get(NULL)$build$local)
    unlink("_projr-local.yml")
    expect_named(.yml_get(NULL)$build$github, "reports")
    expect_identical(.yml_get(NULL)$build$local$backup$cue, "always")

    config$build$dest <- "shared"
    .yml_set(config)
    expect_error(.yml_get(NULL), "github:shared")
    config$build$dest <- "missing"
    .yml_set(config)
    expect_error(.yml_get(NULL), "not declared under remotes")
    config$build$dest <- list("backup", "cue")
    .yml_set(config)
    expect_error(.yml_get(NULL), "belong in a mapping")
    config$build$dest <- list(backup = list(path = "x"))
    .yml_set(config)
    expect_error(.yml_get(NULL), "build.dest.backup")
    config$build$dest <- "backup"
    config$build$local <- list(backup = NULL)
    expect_error(.yml_set(config), "build.local is no longer supported")
    yaml::write_yaml(config, "_projr.yml")
    expect_error(.yml_get(NULL), "build.local is no longer supported")
    expect_error(
      projr_yml_check("default"),
      "build.local is no longer supported"
    )

    config$build$local <- NULL
    config$build$dest <- list(backup = list(cue = "sometimes"))
    .yml_set(config)
    expect_error(projr_yml_check(), "cue")
    config$build$dest <- NULL
    config$remotes$local$cue <- list(path = "x")
    .yml_set(config)
    expect_error(projr_yml_check(), "reserved")
  })
})

test_that("build.source selects registered remotes with fetch settings", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$build$local <- NULL
    config$build$github <- NULL
    config$directories$`raw-codebook` <- list(path = "_raw_data/codebook")
    config$remotes <- list(
      local = list(archive = list(path = "archives")),
      github = list(
        archive = list(),
        survey = list(get = list(cue = "if-change", version = "0.6.3"))
      )
    )
    .yml_set(config)
    expect_identical(.yml_build_source_get(), list())

    config$build$source <- "survey"
    .yml_set(config)
    expect_true(projr_yml_check())
    source <- .yml_build_source_get()
    expect_named(source, "github:survey")
    expect_identical(source[[1]]$type, "github")
    expect_setequal(source[[1]]$content, c("raw-data", "raw-codebook"))
    expect_identical(source[[1]]$cue, "if-change")
    expect_identical(source[[1]]$version, "0.6.3")
    expect_identical(source[[1]]$inspect, "manifest")
    expect_identical(source[[1]]$strategy, "sync-diff")
    expect_identical(source[[1]]$conflict, "error")

    config$build$source <- list(
      "survey",
      list(
        "local:archive" = list(
          version = "1.2.3"
        )
      )
    )
    .yml_set(config)
    expect_true(projr_yml_check())
    source <- .yml_build_source_get()
    expect_named(source, c("github:survey", "local:archive"))
    expect_identical(source[["local:archive"]]$version, "1.2.3")
    expect_identical(source[["local:archive"]]$cue, "if-empty")

    config$build$source <- list(
      cue = "always",
      content = list("raw-codebook"),
      survey = NULL,
      "local:archive" = list(cue = "never", content = list("raw-data"))
    )
    .yml_set(config)
    expect_true(projr_yml_check())
    source <- .yml_build_source_get()
    expect_identical(source[["github:survey"]]$cue, "always")
    expect_identical(source[["github:survey"]]$content, "raw-codebook")
    expect_identical(source[["local:archive"]]$cue, "never")
    expect_identical(source[["local:archive"]]$content, "raw-data")
    expect_null(.yml_get(NULL)$build$github)

    config$build$source <- "archive"
    .yml_set(config)
    expect_error(.yml_build_source_get(), "github:archive")
    config$build$source <- "missing"
    .yml_set(config)
    expect_error(projr_yml_check(), "not declared under remotes")
    config$build$source <- list(survey = list(content = list("output")))
    .yml_set(config)
    expect_error(projr_yml_check(), "build.source.github:survey.content")
    config$build$source <- list(survey = list(strategy = "upload-all"))
    .yml_set(config)
    expect_error(projr_yml_check(), "strategy")
    for (version in c("1", "1.2", "1.2.3", "latest")) {
      config$build$source <- list(survey = list(version = version))
      .yml_set(config)
      expect_true(projr_yml_check())
    }
    config$build$source <- list(survey = list(version = "next"))
    .yml_set(config)
    expect_error(projr_yml_check(), "x.y.z")
    config$build$source <- "survey"
    config$build$dest <- "survey"
    .yml_set(config)
    # One remote can supply inputs and receive outputs.
    expect_true(projr_yml_check())
    config$build$dest <- NULL
    config$remotes$github$survey$get$conflict <- "overwrite"
    .yml_set(config)
    expect_error(projr_yml_check(), "remotes.github.survey.get.conflict")
  })
})

test_that("one-off archives are registered under remotes", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$build$local <- NULL
    config$build$github <- NULL
    config$remotes <- list(github = list(archive = list(tag = "kept")))
    .yml_set(config)
    expect_false(.build_pre_register_archives(FALSE, TRUE, TRUE))
    expect_null(.yml_get("default")$remotes$local)
    expect_true(.build_pre_register_archives(TRUE, TRUE, "raw-data"))
    remotes <- .yml_get("default")$remotes
    expect_identical(remotes$local$archive, list(path = "_archive"))
    expect_identical(remotes$github$archive, list(tag = "kept"))
    expect_true(projr_yml_check())
    expect_false(.build_pre_register_archives(TRUE, TRUE, TRUE))

    config$remotes <- NULL
    .yml_set(config)
    expect_true(.build_pre_register_archives(TRUE, TRUE, FALSE))
    expect_length(.yml_get("default")$remotes$github$archive, 0)
    expect_true(any(grepl("archive: \\{\\}", readLines("_projr.yml"))))
    expect_null(.yml_get("default")$remotes$local)
  })
})

test_that("directory and remote source settings are no longer accepted", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$directories$`raw-data`$source <- list(github = "archive")
    .yml_set(config)
    expect_error(projr_yml_check(), "directories.raw-data.source")
    config$directories$`raw-data`$source <- NULL
    config$remotes <- list(github = list(archive = list(source = TRUE)))
    .yml_set(config)
    expect_error(projr_yml_check(), "remotes.github.archive.source")
  })
})
