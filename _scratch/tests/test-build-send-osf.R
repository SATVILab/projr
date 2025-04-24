test_that("projr_build_output works - osf - latest", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  skip_if(Sys.getenv("GITHUB_ACTIONS") == "true")
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     projr_build_output("patch", msg = "test")
     .version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
      expect_error(projr_yml_dest_add_osf(
        title = "Raw data",
        content = "raw-data",
        structure = "latest",
        category = "data"
      ))
      id_parent <- .test_osf_create_project("Test")
     projr_yml_dest_add_osf(
        title = "Raw data",
        content = "raw-data",
        structure = "latest",
        category = "data",
        id_parent = id_parent
      )
      id <- .yml_dest_get_title("Raw data", "osf", NULL)[["id"]]
      expect_true(.is_string(id))
      expect_true(.assert_nchar(id, 5L))

      # handle nothing to send
      # ---------------------
     projr_build_patch(msg = "Vat are you vinking about")
      osf_tbl <- .remote_get_final_osf(
        id = id,
        path = NULL,
        path_append_label = TRUE,
        label = "raw-data",
        structure = "latest"
      )
      fn_vec <- .remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_true(.is_len_0(fn_vec))

      # handle something to upload
      # ---------------------

      .test_setup_content("raw-data")
     projr_build_patch(msg = "Ze data")
      fn_vec <- .remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        fn_vec |> sort(),
        .file_ls("_raw_data") |> sort()
      )

      # expect no change
      # ----------------------
     projr_build_patch(msg = "I love zis data")
      fn_vec <- .remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        .file_ls("_raw_data") |> sort(),
        fn_vec |> sort()
      )

      # add something
      # ----------------------
      file.create("_raw_data/add.txt")
     projr_build_patch(msg = "More data")
      fn_vec <- .remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        .file_ls("_raw_data") |> sort(),
        fn_vec |> sort()
      )


      # do nothing again
      # ----------------------
     projr_build_patch(msg = "I love zis data")
      fn_vec <- .remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        .file_ls("_raw_data") |> sort(),
        fn_vec |> sort()
      )

      # remove something
      # ----------------------
      file.remove("_raw_data/add.txt")
     projr_build_patch(msg = "Less data")
      fn_vec <- .remote_file_ls(
        "osf",
        remote = osf_tbl
      )
      expect_identical(
        fn_vec |> sort(),
        .file_ls("_raw_data") |> sort()
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
