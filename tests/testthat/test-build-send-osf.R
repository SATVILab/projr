test_that("projr_build_output works - osf - latest", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true")
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      expect_error(projr_yml_dest_add_osf(
        title = "Raw data",
        content = "data-raw",
        structure = "latest",
        category = "data"
      ))
      id_parent <- .projr_test_osf_create_project("Test")
      projr_yml_dest_add_osf(
        title = "Raw data",
        content = "data-raw",
        structure = "latest",
        category = "data",
        id_parent = id_parent
      )
      id <- .projr_yml_dest_get_title("Raw data", "osf", NULL)[["id"]]
      expect_true(.is_string(id))
      expect_true(.assert_nchar(id, 5L))

      # handle nothing to send
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")
      osf_tbl <- .projr_remote_get_final_osf(
        id = id,
        path = NULL,
        path_append_label = TRUE,
        label = "data-raw",
        structure = "latest"
      )
      fn_vec <- .projr_remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_true(.is_len_0(fn_vec))

      # handle something to upload
      # ---------------------

      browser()

      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Ze data")
      fn_vec <- .projr_remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        fn_vec |> sort(),
        .file_ls("_data_raw") |> sort()
      )

      # expect no change
      # ----------------------
      projr_build_patch(msg = "I love zis data")
      fn_vec <- .projr_remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        .file_ls("_data_raw") |> sort(),
        fn_vec |> sort()
      )

      # add something
      # ----------------------
      file.create("_data_raw/add.txt")
      projr_build_patch(msg = "More data")
      fn_vec <- .projr_remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        .file_ls("_data_raw") |> sort(),
        fn_vec |> sort()
      )


      # do nothing again
      # ----------------------
      projr_build_patch(msg = "I love zis data")
      fn_vec <- .projr_remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        .file_ls("_data_raw") |> sort(),
        fn_vec |> sort()
      )

      # remove something
      # ----------------------
      file.remove("_data_raw/add.txt")
      projr_build_patch(msg = "Less data")
      fn_vec <- .projr_remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        fn_vec |> sort(),
        .file_ls("_data_raw") |> sort()
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
