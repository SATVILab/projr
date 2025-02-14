test_that(".yml_git_ functions work", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
     .yml_git_set_default()
      expect_identical(
        .yml_git_get("default"),
        NULL
      )

      # set one to FALSE
     .yml_git_set(push = FALSE)
      expect_identical(
        .yml_git_get("default"),
        list(push = FALSE)
      )
      # set two to FALSE
     .yml_git_set(commit = FALSE)
      expect_identical(
        .yml_git_get("default"),
        list(commit = FALSE, push = FALSE)
      )
      # set three to FALSE, no simplify identical
     .yml_git_set(add_untracked = FALSE, simplify_identical = FALSE)
      expect_identical(
        .yml_git_get("default"),
        list(commit = FALSE, `add-untracked` = FALSE, push = FALSE)
      )
      # set three to FALSE, simplify identical
     .yml_git_set(add_untracked = FALSE)
      expect_identical(
        .yml_git_get("default"),
        FALSE
      )
      # set three to TRUE, no simplify default
     .yml_git_set(all = TRUE, simplify_default = FALSE)
      expect_identical(
        .yml_git_get("default"),
        TRUE
      )
      # set three to TRUE, simplify default
     .yml_git_set(all = TRUE)
      expect_identical(
        .yml_git_get("default"),
        NULL
      )
      # use meaningful default
     .yml_git_set(commit = FALSE)
     .yml_git_set_default()
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


test_that(".git_ functions work", { # setup
  skip_if(.is_test_select())
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
      username <- gh::gh_whoami()[["login"]]
      PAT <- Sys.getenv("GITHUB_PAT")

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
      if (!requireNamespace("gitcreds", quietly = TRUE)) {
        utils::install.packages("gitcreds")
      }
      if (!requireNamespace("credentials", quietly = TRUE)) {
        utils::install.packages("credentials")
      }
      if (debug) {
        print("gitcreds::gitcreds_get")
      }
      gitcreds::gitcreds_get() |> print()
      # print("usethis::gh_token_help")
      # usethis::gh_token_help() |> print()
    }
  )
})
