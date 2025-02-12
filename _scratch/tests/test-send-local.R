test_that("projr_yml_dest_add* functions work", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive"
      )
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )
      # add two
      projr_yml_dest_add_local(
        title = "archive second",
        content = "output",
        path = "_archive/second"
      )
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          ),
          "archive second" = list(
            content = "output",
            path = "_archive/second"
          )
        )
      )
      # remove just one
      .projr_yml_dest_rm_title("archive second", "local", "default")
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )
    }
  )
})

# --------------------------
# actually sending
# --------------------------

test_that(".projr_remote_create works - local", {
  # skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      file.create(projr::projr_path_get("raw-data", "data.csv"))
      projr_init_git()
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr::projr_build_dev()
      # remove github remote
      yml_projr <- projr_yml_get()
      yml_projr[["build"]] <- yml_projr[["build"]][
        -which(names(yml_projr[["build"]]) == "github")
      ]
      .projr_yml_set(yml_projr)
      browser()
      browser()
      projr::projr_build_patch()
      # add a local destination, that is never sent to
      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      browser()

      # always add  it
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        overwrite = TRUE
      )

      projr::projr_build_patch()
    }
  )
})
