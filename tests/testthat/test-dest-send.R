test_that("projr_yml_dest_add* functions work", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive"
      )
      expect_identical(
        .yml_dest_get_type("local", "default"),
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
        .yml_dest_get_type("local", "default"),
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
      .yml_dest_rm_title("archive second", "local", "default")
      expect_identical(
        .yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )

      # ============
      # github
      # ============

      # test maniup
      .yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_github(
        title = "archive",
        content = "raw-data"
      )
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
      # add two
      projr_yml_dest_add_github(
        title = "archive second",
        content = "output"
      )
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          ),
          "archive-second" = list(
            content = "output"
          )
        )
      )
      # remove just one
      .yml_dest_rm_title("archive-second", "github", "default")
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
    }
  )
})
