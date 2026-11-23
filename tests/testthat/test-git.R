test_that(".git_ functions work", { # setup
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .git_system_setup_gert()

      # initialisation
      # ---------------------
      expect_false(.git_repo_check_exists())
      .git_init_git()
      expect_true(.git_repo_check_exists())
      .git_repo_rm()
      expect_false(.git_repo_check_exists())
      .git_init_gert()
      expect_true(.git_repo_check_exists())
      .git_repo_rm()
      expect_false(.git_repo_check_exists())
      .git_init()
      expect_true(.git_repo_check_exists())

      # config
      # ---------------------
      .test_setup_project_git_config()
      expect_identical(.git_config_get_name_git(), "DarthVader")

      # adding and committing individual fules
      # ---------------------
      status_tbl <- gert::git_status()
      .git_commit_file_git(".Rbuildignore", msg = "Commit R build ignore")
      expect_identical(
        nrow(gert::git_status()), nrow(status_tbl) - 1L
      )
      .git_commit_file_gert(".gitignore", msg = "Commit Git ignore")
      expect_identical(nrow(gert::git_status()), nrow(status_tbl) - 2L)
      # getting modified files
      # ---------------------
      expect_identical(.git_modified_get_git(), character())
      expect_identical(.git_modified_get_gert(), character())
      expect_identical(.git_modified_get(), character())
      cat("abc", file = ".Rbuildignore")
      expect_identical(.git_modified_get_git(), ".Rbuildignore")
      expect_identical(.git_modified_get_gert(), ".Rbuildignore")
      expect_identical(.git_modified_get(), ".Rbuildignore")
      # getting untracked files
      # ---------------------
      status_tbl <- gert::git_status()
      new_vec <- status_tbl[["file"]][status_tbl[["status"]] == "new"] |>
        sort()
      expect_identical(.git_new_get_git() |> sort(), new_vec)
      expect_identical(.git_new_get_gert() |> sort(), new_vec)
      expect_identical(.git_new_get() |> sort(), new_vec)
      # check there's a remote
      # ---------------------
      expect_false(.git_remote_check_exists_git())
      expect_false(.git_remote_check_exists_gert())
      expect_false(.git_remote_check_exists())
      # check there's an upstream remote
      # ---------------------
      expect_false(suppressWarnings(.git_remote_check_upstream_git()))
      expect_false(suppressWarnings(.git_remote_check_upstream()))
    }
  )
})


test_that("Git and gert clone, push and fetch from a local bare upstream", {
  skip_if(.is_test_select())
  skip_if_not_installed("gert")
  skip_if(!.git_system_check_git())
  dir_test <- .test_setup_project(set_env_var = FALSE)
  dir_remote <- withr::local_tempdir()
  gert::git_init(dir_remote, bare = TRUE)
  repo <- "fixture/project"
  url <- paste0("https://github.com/", repo, ".git")
  config <- withr::local_tempfile()
  withr::local_envvar(c(GIT_CONFIG_GLOBAL = config, GIT_ALLOW_PROTOCOL = "file"))
  expect_equal(system2("git", c(
    "config", "--file", shQuote(config),
    shQuote(paste0("url.", dir_remote, ".insteadOf")), url
  )), 0L)
  # libgit2 can cache the user's config directory before the test starts.
  # Redirect the URL at its clone boundary and use the real local transport.
  clone <- gert::git_clone
  testthat::local_mocked_bindings(
    git_clone = function(url, ...) {
      expect_identical(url, "https://github.com/fixture/project.git")
      clone(dir_remote, ...)
    }, .package = "gert"
  )

  usethis::with_project(dir_test, {
    for (backend in c("git", "gert")) {
      withr::local_envvar(c(PROJR_GIT_USE_GERT = as.character(backend == "gert")))
      expect_false(.git_check_behind())
    }
    gert::git_remote_add(dir_remote, "origin")
    branch <- gert::git_branch()
    expect_equal(system2("git", c("push", "-u", "origin", branch)), 0L)
    expect_equal(system2("git", c(
      "-C", shQuote(dir_remote), "symbolic-ref", "HEAD", paste0("refs/heads/", branch)
    )), 0L)

    for (backend in c("git", "gert")) {
      withr::local_envvar(c(PROJR_GIT_USE_GERT = as.character(backend == "gert")))
      expect_identical(.git_system_get(), backend)
      expect_true(.git_remote_check_exists())
      expect_true(.git_remote_check_upstream())
      path_clone <- paste0("clone-", backend)
      .git_clone(repo, path_clone)
      expect_identical(gert::git_log(repo = path_clone)$commit[[1]], gert::git_log()$commit[[1]])
      gert::git_remote_set_url(dir_remote, remote = "origin", repo = path_clone)

      writeLines(backend, "change.txt")
      .git_commit_file("change.txt", paste("Push with", backend))
      expect_true(.git_push())
      pushed <- gert::git_log()$commit[[1]]
      expect_identical(gert::git_log(repo = dir_remote)$commit[[1]], pushed)
      expect_false(.git_check_behind())
      withr::with_dir(path_clone, {
        .git_fetch()
        expect_equal(gert::git_ahead_behind()$behind, 1L)
        expect_true(.git_check_behind())
        expect_identical(gert::git_log(ref = paste0("origin/", branch))$commit[[1]], pushed)
      })
    }
  })
})

for (backend in c("git", "gert")) {
  test_that(paste(backend, "reports repository identity and the latest commit"), {
    skip_if(.is_test_select())
    skip_if_not_installed("gert")
    skip_if(backend == "git" && !.git_system_check_git())
    withr::local_envvar(PROJR_GIT_USE_GERT = as.character(backend == "gert"))
    expect_identical(.git_system_get(), backend)
    expect_identical(.git_system_check_gert(), requireNamespace("gert", quietly = TRUE))
    expect_identical(.git_system_setup(), backend == "gert")
    dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
    usethis::with_project(dir_test, {
      expect_false(.git_repo_is_worktree())
      expect_null(.git_branch_get())
      expect_null(.git_last_commit_get())

      .git_init()
      .test_setup_project_git_config()
      expect_false(.git_repo_is_worktree())
      expect_null(suppressWarnings(.git_last_commit_get()))
      expect_identical(.git_config_get_name(), "DarthVader")
      expect_identical(.git_config_get_email(), "number_one_fan@tellytubbies.com")
      expect_false(.git_commit_file(character(), "empty"))
      expect_false(.git_commit_file(NULL, "empty"))

      writeLines("version one", "report.txt")
      .git_commit_file("report.txt", "First report")
      expect_identical(.git_branch_get(), gert::git_branch())
      commit <- .git_last_commit_get()
      expect_identical(commit$message, "First report")
      expect_true(nzchar(commit$sha) && startsWith(gert::git_log()$commit[1], commit$sha))
    })
  })

  test_that(paste(backend, "distinguishes modified, ignored and untracked files"), {
    skip_if(.is_test_select())
    skip_if_not_installed("gert")
    skip_if(backend == "git" && !.git_system_check_git())
    withr::local_envvar(PROJR_GIT_USE_GERT = as.character(backend == "gert"))
    dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
    usethis::with_project(dir_test, {
      expect_length(.git_changed_filter("tracked.txt"), 0L)
      expect_identical(.git_untracked_not_ignored_get(), character())
      .git_init()
      .test_setup_project_git_config()
      writeLines("one", "tracked.txt")
      expect_true("tracked.txt" %in% .git_untracked_not_ignored_get())
      .git_commit_file("tracked.txt", "Track one file")
      expect_false("tracked.txt" %in% .git_untracked_not_ignored_get())
      expect_length(.git_changed_filter("tracked.txt"), 0L)

      writeLines("ignored.txt", ".gitignore")
      writeLines("ignored", "ignored.txt")
      writeLines("two", "tracked.txt")
      writeLines("new", "untracked.txt")
      expect_false("ignored.txt" %in% .git_untracked_not_ignored_get())
      expect_true(".gitignore" %in% .git_untracked_not_ignored_get())
      expect_length(.git_changed_filter(character()), 0L)
      expect_setequal(
        .git_changed_filter(c("tracked.txt", "untracked.txt", "missing.txt")),
        c("tracked.txt", "untracked.txt")
      )

      .git_commit_all("Changed files only", add_untracked = FALSE)
      expect_true("untracked.txt" %in% .git_untracked_not_ignored_get())
      expect_length(.git_changed_filter("tracked.txt"), 0L)
      .git_commit_all("Include new files", add_untracked = TRUE)
      expect_identical(nrow(gert::git_status()), 0L)
    })
  })
}

test_that(".git_add_file_git works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Create a file
      writeLines("test content", "test.txt")

      # Add the file using git CLI
      result <- .git_add_file_git("test.txt")

      # Check file is staged
      status <- gert::git_status()
      expect_true("test.txt" %in% status$file)
      expect_true("new" %in% status$status)
    }
  )
})

test_that("Git identities are written only to the fixture repository", {
  skip_if(.is_test_select())
  skip_if_not_installed("gert")
  config <- withr::local_tempfile()
  writeLines(c("[user]", "  name = Original User", "  email = original@example.test"), config)
  bytes <- function(path) readBin(path, "raw", file.size(path))
  original <- bytes(config)
  withr::local_envvar(c(GIT_CONFIG_GLOBAL = config))
  testthat::local_mocked_bindings(
    git_config_global_set = function(...) stop("Unexpected global Git config write"),
    .package = "gert"
  )
  for (backend in c("git", "gert")) {
    if (backend == "git" && !.git_system_check_git()) next
    withr::local_envvar(c(PROJR_GIT_USE_GERT = as.character(backend == "gert")))
    project <- .test_setup_project()
    usethis::with_project(project, {
      .init_git_config_ind_set("user.name", "Fixture Author")
      .init_git_config_ind_set("user.email", "fixture@example.test")
      cfg <- gert::git_config(repo = project)
      local <- cfg[cfg$level == "local", ]
      expect_equal(local$value[local$name == "user.name"], "Fixture Author")
      expect_equal(local$value[local$name == "user.email"], "fixture@example.test")
      nested <- file.path(project, "child")
      dir.create(nested)
      expect_error(.test_setup_project_git_config(nested), "temporary fixture")
    })
    expect_identical(bytes(config), original)
  }
})

test_that("Git CLI reports rejected writes and can retry without an empty commit", {
  skip_if(.is_test_select())
  skip_if_not_installed("gert")
  skip_if(!.git_system_check_git())
  withr::local_envvar(c(PROJR_GIT_USE_GERT = "FALSE", PROJR_OUTPUT_LEVEL = "none"))
  withr::local_dir(withr::local_tempdir())
  gert::git_init()
  .test_setup_project_git_config()
  writeLines("first", "my results.txt")
  expect_error(.git_add_file_git("missing-file"), "git add failed")
  .git_commit_all("First build")
  expect_false(.git_commit_all())
  expect_false(.git_commit_file_git("my results.txt", "No new changes"))
  upstream <- withr::local_tempdir()
  gert::git_init(upstream, bare = TRUE)
  gert::git_remote_add(upstream)
  gert::git_push("origin", set_upstream = TRUE)
  first <- gert::git_info()$commit

  writeLines("second", "my results.txt")
  hook <- file.path(".git", "hooks", "pre-commit")
  writeLines(c("#!/bin/sh", "exit 1"), hook)
  Sys.chmod(hook, "0755")
  expect_error(.git_commit_all("Second build"), "git commit failed")
  expect_identical(gert::git_info()$commit, first)
  unlink(hook)
  .git_commit_all("Second build")
  reject <- file.path(upstream, "hooks", "pre-receive")
  writeLines(c("#!/bin/sh", "echo 'https://fixture:sample-token@example.invalid' >&2", "exit 1"), reject)
  Sys.chmod(reject, "0755")
  error <- expect_error(.git_push_git(stdout = TRUE, stderr = TRUE), "git push failed")
  expect_match(conditionMessage(error), "https://example.invalid", fixed = TRUE)
  expect_false(grepl("sample-token", conditionMessage(error), fixed = TRUE))
  expect_identical(gert::git_log(repo = upstream)$commit[1], first)
  unlink(reject)
  expect_true(.git_push_git(stdout = TRUE, stderr = TRUE))
  expect_identical(gert::git_log(repo = upstream)$commit[1], gert::git_info()$commit)

  for (level in c("std", "debug")) {
    withr::local_envvar(c(PROJR_OUTPUT_LEVEL = level))
    files <- sprintf("results-%s-%d.txt", level, seq_len(32))
    file.create(files)
    .git_add_file_git(files)
    lines <- .git_commit_staged_git("Add results", stdout = TRUE, stderr = TRUE)
    expect_length(lines, 0L)
    expect_true(all(files %in% gert::git_ls()$path))
  }
})
