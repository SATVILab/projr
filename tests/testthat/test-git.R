test_that(".projr_yml_git_ functions work", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_git_set_default()
      expect_identical(
        .projr_yml_git_get("default"),
        NULL
      )

      # set one to FALSE
      projr_yml_git_set(push = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        list(push = FALSE)
      )
      # set two to FALSE
      projr_yml_git_set(commit = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        list(commit = FALSE, push = FALSE)
      )
      # set three to FALSE, no simplify identical
      projr_yml_git_set(add_untracked = FALSE, simplify_identical = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        list(commit = FALSE, `add-untracked` = FALSE, push = FALSE)
      )
      # set three to FALSE, simplify identical
      projr_yml_git_set(add_untracked = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        FALSE
      )
      # set three to TRUE, no simplify default
      projr_yml_git_set(all = TRUE, simplify_default = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        TRUE
      )
      # set three to TRUE, simplify default
      projr_yml_git_set(all = TRUE)
      expect_identical(
        .projr_yml_git_get("default"),
        NULL
      )
      # use meaningful default
      projr_yml_git_set(commit = FALSE)
      projr_yml_git_set_default()
      expect_identical(
        .projr_yml_git_get("default"),
        NULL
      )
    }
  )
})

test_that(".projr_git_ functions work", { # setup
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_git_system_setup_gert()

      # initialisation
      # ---------------------
      expect_false(.projr_git_repo_check_exists())
      .projr_git_init_git()
      expect_true(.projr_git_repo_check_exists())
      .projr_git_repo_rm()
      .projr_git_init_gert()
      expect_true(.projr_git_repo_check_exists())
      .projr_git_repo_rm()
      .projr_git_init()
      expect_true(.projr_git_repo_check_exists())
      # config
      # ---------------------
      gert::git_config_set("user.name", "Darth Vader")
      expect_identical(.projr_git_config_get_name_git(), "Darth Vader")
      expect_identical(.projr_git_config_get_name_gert(), "Darth Vader")

      # adding and committing individual fules
      # ---------------------
      status_tbl <- gert::git_status()
      .projr_git_commit_file_git(".Rbuildignore", msg = "Have fun")
      expect_identical(nrow(gert::git_status()), nrow(status_tbl) - 1L)
      .projr_git_commit_file_gert(".gitignore", msg = "Have fun")
      expect_identical(nrow(gert::git_status()), nrow(status_tbl) - 2L)
      # getting modified files
      # ---------------------
      expect_identical(.projr_git_modified_get_git(), character())
      expect_identical(.projr_git_modified_get_gert(), character())
      expect_identical(.projr_git_modified_get(), character())
      cat("abc", file = ".Rbuildignore")
      expect_identical(.projr_git_modified_get_git(), ".Rbuildignore")
      expect_identical(.projr_git_modified_get_gert(), ".Rbuildignore")
      expect_identical(.projr_git_modified_get(), ".Rbuildignore")
      # getting untracked files
      # ---------------------
      status_tbl <- gert::git_status()
      new_vec <- status_tbl[["file"]][status_tbl[["status"]] == "new"] |>
        sort()
      expect_identical(.projr_git_new_get_git() |> sort(), new_vec)
      expect_identical(.projr_git_new_get_gert() |> sort(), new_vec)
      expect_identical(.projr_git_new_get() |> sort(), new_vec)
      # check there's a remote
      # ---------------------
      expect_false(.projr_git_remote_check_exists_git())
      expect_false(.projr_git_remote_check_exists_gert())
      expect_false(.projr_git_remote_check_exists())
      # check there's an upstream remote
      # ---------------------
      expect_false(suppressWarnings(.projr_git_remote_check_upstream_git()))
      expect_false(suppressWarnings(.projr_git_remote_check_upstream()))
    }
  )
})


test_that(".projr_git_ functions work", { # setup
  # skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  # skip()

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # check there's a remote
      # ---------------------
      expect_true(.projr_git_remote_check_exists_git())
      expect_true(.projr_git_remote_check_exists_gert())
      expect_true(.projr_git_remote_check_exists())
      # check there's an upstream remote
      # ---------------------
      expect_true(suppressWarnings(.projr_git_remote_check_upstream_git()))
      expect_true(suppressWarnings(.projr_git_remote_check_upstream()))
      # push
      # -----------------------
      invisible(file.create("abc.txt"))

      status_tbl <- gert::git_status()
      .projr_git_commit_file("abc.txt", msg = "abc")
      expect_true(.projr_git_push_git())
      invisible(file.create("def.txt"))
      status_tbl <- gert::git_status()
      .projr_git_commit_file("def.txt", msg = "def")
      expect_true(.projr_git_push_gert())
    }
  )
})
