test_that("projr_yml_dest_add* functions work", {
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
        content = "data-raw",
        path = "_archive"
      )
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "data-raw",
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
            content = "data-raw",
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
            content = "data-raw",
            path = "_archive"
          )
        )
      )
    }
  )
})


test_that(".projr_dest_send works", {
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # no runs
      expect_false(.projr_dest_send("dev"))
      .projr_yml_dest_rm_type_all("default")
      expect_false(.projr_dest_send("major"))
      # run, but do nothing
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive"
      )
      expect_true(
        .projr_state_len_nz(
          .projr_yml_dest_get_title("archive", "local", "default")
        )
      )
      projr_version_set("0.0.1")
      .projr_build_manifest_post(TRUE) |> invisible()
      .projr_dest_send("major")
      expect_true(.projr_dir_ls("_archive") |> .projr_state_len_z())
      # run and do something
      .projr_test_setup_content("output", safe = FALSE)
      .projr_version_bump("patch")
      .projr_build_manifest_post(TRUE) |> invisible()
      .projr_dest_send("major")
      expect_true(dir.exists("_archive"))
      expect_identical(
        .projr_dir_ls("_archive/output/v0.0.2"), .projr_dir_ls("_output")
      )
    }
  )
})
