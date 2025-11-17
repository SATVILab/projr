test_that("projr_yml_git_ functions work", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_git_set_default()
      expect_identical(
        .yml_git_get("default"),
        NULL
      )

      # set one to FALSE
      projr_yml_git_set(push = FALSE, simplify_identical = FALSE, simplify_default = FALSE)
      expect_identical(
        .yml_git_get("default"),
        list(push = FALSE)
      )
      # set two to FALSE (with simplify_identical = TRUE, should simplify to FALSE)
      projr_yml_git_set(commit = FALSE, push = FALSE, simplify_identical = TRUE, simplify_default = FALSE)
      expect_identical(
        .yml_git_get("default"),
        FALSE
      )
      # set three to FALSE, no simplify identical
      projr_yml_git_set(commit = FALSE, add_untracked = FALSE, push = FALSE, simplify_identical = FALSE, simplify_default = FALSE)
      expect_identical(
        .yml_git_get("default"),
        list(commit = FALSE, `add-untracked` = FALSE, push = FALSE)
      )
      # set three to FALSE, simplify identical
      projr_yml_git_set(commit = FALSE, add_untracked = FALSE, push = FALSE, simplify_identical = TRUE, simplify_default = FALSE)
      expect_identical(
        .yml_git_get("default"),
        FALSE
      )
      # set three to TRUE, no simplify default
      projr_yml_git_set(all = TRUE, simplify_default = FALSE)
      expect_identical(
        .yml_git_get("default"),
        TRUE
      )
      # set three to TRUE, simplify default
      projr_yml_git_set(all = TRUE)
      expect_identical(
        .yml_git_get("default"),
        NULL
      )
      # use meaningful default
      projr_yml_git_set(commit = FALSE)
      projr_yml_git_set_default()
      expect_identical(
        .yml_git_get("default"),
        NULL
      )
    }
  )
})

test_that(".git_ functions work", { # setup
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
      path_err <- file.path(tempdir(), "err1")
      commit_out <- .git_commit_file_git(
        ".Rbuildignore",
        msg = "Have fun", stderr = path_err
      )
      print("first commit out")
      print(commit_out)
      print("first error message")
      print(readLines(path_err))
      expect_identical(
        nrow(gert::git_status()), nrow(status_tbl) - 1L
      )
      commit_out <- .git_commit_file_gert(
        ".gitignore",
        msg = "Have fun"
      )
      print("second commit out")
      print(commit_out)
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


test_that(".git_ functions work with GitHub", { # setup
  skip_if(.is_test_select())
  skip_if(!nzchar(.auth_get_github_pat_find()))

  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  debug <- FALSE

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      if (debug) {
        print("setting config")
      }
      .test_setup_project_git_config()
      if (debug) {
        print("done setting config")
      }
      # check there's a remote
      # ---------------------
      if (debug) {
        print("checking remotes exist")
      }
      expect_true(.git_remote_check_exists_git())
      if (debug) {
        print("git remote identification")
        print(system2("git", args = c("remote", "-v"), stdout = TRUE))
        print("done with git remote identification")
      }
      expect_true(.git_remote_check_exists_gert())
      expect_true(.git_remote_check_exists())
      if (debug) {
        print("gert remote identification")
        print(gert::git_remote_ls())
        print("done checking remotes exist")
      }
      # check there's an upstream remote
      # ---------------------
      if (debug) {
        print("check upstream")
      }
      expect_true(suppressWarnings(.git_remote_check_upstream_git()))
      expect_true(suppressWarnings(.git_remote_check_upstream()))
      if (debug) {
        print("done checking upstream")
      }
      # push
      # -----------------------
      invisible(file.create("abc.txt"))

      if (debug) {
        print("get gert status table")
      }
      status_tbl <- gert::git_status()
      if (debug) {
        print("done get gert status table")
        print("commit a file with git")
      }
      pathout <- file.path(tempdir(), "pushout")
      file.create(pathout)
      errout <- file.path(tempdir(), "errout")
      file.create(errout)
      .git_commit_file_git(
        "abc.txt",
        msg = "abc", timeout = 20, stdout = pathout, stderr = errout
      )
      if (debug) {
        print("pathout")
        print(readLines(pathout))
        print("errout")
        print(readLines(errout))
        print("done commit a file with git")
        print("Use plain-text credential store")
      }

      system2("git", args = c("config", "--local", "credential.helper", "store"))
      .dep_install_only("gh")
      username <- tryCatch({
        gh::gh_whoami()[["login"]]
      }, error = function(e) {
        NULL
      })
      if (!.is_string(username)) {
        skip("GitHub user not found")
      }
      PAT <- .auth_get_github_pat_find()

      # Create a credential string
      credential_string <- paste0("protocol=https\nhost=github.com\nusername=", username, "\npassword=", PAT)

      # Write the credential string to a temporary file
      temp_file <- tempfile()
      writeLines(credential_string, temp_file)

      # Use the temporary file as the input for 'git credential approve'
      system(paste0("git credential approve < ", shQuote(temp_file)))

      # Delete the temporary file
      file.remove(temp_file)

      if (debug) {
        print("push a file with git")
      }
      pathout <- file.path(tempdir(), "pushout")
      .file_rm(pathout)
      file.create(pathout)
      errout <- file.path(tempdir(), "errout")
      .file_rm(errout)
      file.create(errout)
      expect_true(
        .git_push_git(
          timeout = 20, stdout = pathout, stderr = errout
        )
      )
      if (debug) {
        print("pathout")
        print("no change")
        print(readLines(pathout))
        print("errout")
        print(readLines(errout))
        print("done pushing a file with git")
      }
      invisible(file.create("def.txt"))
      status_tbl <- gert::git_status()
      if (debug) {
        print("commit a file with gert")
      }
      .git_commit_file_gert("def.txt", msg = "def")
      if (debug) {
        print("done committing a file with gert")
      }
      if (!Sys.getenv("GITHUB_ACTIONS") == "true") {
        expect_true(.git_push_gert())
      }
      if (debug) {
        print("gert::git_log()")
      }
      gert::git_log()[["message"]] |> print()
      gert::git_log()[["files"]] |> print()
      gert::git_log()[["author"]] |> print()
      if (debug) {
        print("gert::git_info()")
        print(gert::git_info())
      }
      # Packages should be installed via Suggests
      .dep_install_only("gitcreds")
      .dep_install_only("credentials")
      if (debug) {
        print("gitcreds::gitcreds_get")
      }
      gitcreds::gitcreds_get() |> print()
      # print("usethis::gh_token_help")
      # usethis::gh_token_help() |> print()
    }
  )
})

# Comprehensive tests for git functions
# =======================================

test_that(".git_repo_is_worktree works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No repo yet
      expect_false(.git_repo_is_worktree())

      # Initialize repo - should not be a worktree
      .git_init()
      expect_false(.git_repo_is_worktree())
    }
  )
})

test_that(".git_changed_filter works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Without git repo
      expect_length(.git_changed_filter("test.txt"), 0)

      # Initialize git and setup
      .git_init()
      .test_setup_project_git_config()

      # Create and commit a file
      writeLines("test", "test.txt")
      .git_commit_file("test.txt", "initial commit")

      # File not changed
      expect_length(.git_changed_filter("test.txt"), 0)

      # Modify file
      writeLines("test modified", "test.txt")
      result <- .git_changed_filter("test.txt")
      expect_length(result, 1)
      expect_true("test.txt" %in% as.character(result))

      # New file
      writeLines("new", "new.txt")
      result <- .git_changed_filter("new.txt")
      expect_length(result, 1)
      expect_true("new.txt" %in% as.character(result))

      # Non-existent file
      expect_length(.git_changed_filter("nonexistent.txt"), 0)

      # Empty input
      expect_length(.git_changed_filter(character(0)), 0)
    }
  )
})

test_that(".git_commit_all works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Create some files
      writeLines("file1", "file1.txt")
      writeLines("file2", "file2.txt")
      writeLines("file3", "file3.txt")

      # Initial commit of file1
      .git_commit_file("file1.txt", "first commit")

      # Modify file1 and add file2, file3
      writeLines("file1 modified", "file1.txt")

      # Commit all (should commit modified and untracked)
      .git_commit_all("commit all", add_untracked = TRUE)

      # Check nothing is staged
      status <- gert::git_status()
      expect_identical(nrow(status), 0L)

      # Create another file and commit without untracked
      writeLines("file4", "file4.txt")
      writeLines("file1 modified again", "file1.txt")
      .git_commit_file("file1.txt", "commit file1 only")

      # file4 should still be untracked
      status <- gert::git_status()
      expect_true("file4.txt" %in% status$file)
    }
  )
})

test_that(".git_branch_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No repo
      expect_null(.git_branch_get())

      # Initialize repo
      .git_init()
      .test_setup_project_git_config()

      # Need at least one commit for branch to exist
      writeLines("test", "test.txt")
      .git_commit_file("test.txt", "initial commit")

      # Should return branch name (usually master or main)
      branch <- .git_branch_get()
      expect_true(is.character(branch) && length(branch) == 1)
      expect_true(nzchar(branch))
    }
  )
})

test_that(".git_last_commit_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No repo
      expect_null(.git_last_commit_get())

      # Initialize repo
      .git_init()
      .test_setup_project_git_config()

      # No commits yet
      expect_null(suppressWarnings(.git_last_commit_get()))

      # Make a commit
      writeLines("test", "test.txt")
      .git_commit_file("test.txt", "test commit message")

      # Should return commit info
      commit_info <- .git_last_commit_get()
      expect_type(commit_info, "list")
      expect_true("sha" %in% names(commit_info))
      expect_true("message" %in% names(commit_info))
      expect_identical(commit_info$message, "test commit message")
      expect_true(nzchar(commit_info$sha))
    }
  )
})

test_that(".git_untracked_not_ignored_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No repo
      expect_identical(.git_untracked_not_ignored_get(), character(0))

      # Initialize repo
      .git_init()
      .test_setup_project_git_config()

      # No untracked files initially (all existing files are ignored or committed)
      # Create a new file
      writeLines("test", "test.txt")
      untracked <- .git_untracked_not_ignored_get()
      expect_true("test.txt" %in% untracked)

      # Commit the file
      .git_commit_file("test.txt", "commit test")

      # Should be empty now
      untracked <- .git_untracked_not_ignored_get()
      expect_false("test.txt" %in% untracked)

      # Add to gitignore
      writeLines("ignored.txt", "ignored.txt")
      writeLines("ignored.txt", ".gitignore")

      # ignored.txt should not appear in untracked
      untracked <- .git_untracked_not_ignored_get()
      expect_false("ignored.txt" %in% untracked)
      # .gitignore itself should be untracked
      expect_true(".gitignore" %in% untracked)
    }
  )
})

test_that(".git_config_get_name and .git_config_get_email work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Check name
      name <- .git_config_get_name()
      expect_identical(name, "DarthVader")

      # Check email
      email <- .git_config_get_email()
      expect_identical(email, "number_one_fan@tellytubbies.com")

      # Test git system functions
      name_git <- .git_config_get_name_git()
      expect_identical(name_git, "DarthVader")

      email_git <- .git_config_get_email_git()
      expect_identical(email_git, "number_one_fan@tellytubbies.com")

      # Test gert functions
      name_gert <- .git_config_get_name_gert()
      expect_identical(name_gert, "DarthVader")

      email_gert <- .git_config_get_email_gert()
      expect_identical(email_gert, "number_one_fan@tellytubbies.com")
    }
  )
})

test_that(".git_system_get and .git_system_check_git work", {
  skip_if(.is_test_select())

  # Check if git system is available
  git_available <- .git_system_check_git()
  expect_type(git_available, "logical")

  # Get the git system being used
  system <- .git_system_get()
  expect_true(system %in% c("git", "gert"))

  # If git CLI is available, it should be preferred
  if (git_available) {
    expect_identical(system, "git")
  }
})

test_that(".git_clone works", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())
  skip("Skipping git clone test - requires network and GitHub authentication")

  # This test is skipped by default as it requires network access
  # and GitHub authentication. Uncomment to run manually if needed.

  # dir_test <- tempfile()
  # dir.create(dir_test)
  #
  # withr::with_dir(dir_test, {
  #   # Would need a public repo to test
  #   # .git_clone("user/repo", "cloned_repo")
  #   # expect_true(dir.exists("cloned_repo"))
  # })
})

test_that(".git_fetch works with remote", {
  skip_if(.is_test_select())
  skip("Skipping fetch test - requires remote repository")

  # This test requires a remote repository setup
  # Would test .git_fetch(), .git_fetch_git(), .git_fetch_gert()
})

test_that(".git_check_behind works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # No remote - should return FALSE
      expect_false(.git_check_behind())

      # Note: Full testing would require remote setup
    }
  )
})
