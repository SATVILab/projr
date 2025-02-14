test_that("projr_build_output works - github - latest", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
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
     .build_output("patch", msg = "test")
     projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_github(
        title = "Raw data",
        content = "raw-data",
        structure = "latest"
      )

      # handle nothing to send
      # ---------------------
     .build_patch(msg = "Vat are you vinking about")
      release_tbl <- .pb_release_tbl_get()
      expect_true(nrow(release_tbl) == 0L)

      # handle something to upload
      # ---------------------

      .test_setup_content("raw-data")
     .build_patch(msg = "Ze data")
      release_tbl <- .pb_release_tbl_get()
      expect_true(nrow(release_tbl) == 1L)
      fn_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = "Raw-data", fn = "raw-data.zip")
      )
      expect_true("subdir1/subdir2/ghi.txt" %in% fn_vec)
      expect_true("abc.txt" %in% fn_vec)


      # expect no change
      # ----------------------
     .build_patch(msg = "I love zis data")
      release_tbl <- .pb_release_tbl_get()
      expect_true(nrow(release_tbl) == 1L)
      fn_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = "Raw-data", fn = "raw-data.zip")
      )
      expect_true("subdir1/subdir2/ghi.txt" %in% fn_vec)

      # add something
      # ----------------------
      file.create("_raw_data/add.txt")
     .build_patch(msg = "More data")
      fn_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = "Raw-data", fn = "raw-data.zip")
      )
      expect_true("add.txt" %in% fn_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% fn_vec)


      # do nothing again
      # ----------------------
     .build_patch(msg = "I love zis data")
      fn_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = "Raw-data", fn = "raw-data.zip")
      )
      expect_true("add.txt" %in% fn_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% fn_vec)

      # remove something
      # ----------------------
      file.remove("_raw_data/add.txt")
     .build_patch(msg = "Less data")
      fn_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = "Raw-data", fn = "raw-data.zip")
      )
      expect_true(!"add.txt" %in% fn_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% fn_vec)
    },
    quiet = TRUE,
    force = TRUE
  )
})
