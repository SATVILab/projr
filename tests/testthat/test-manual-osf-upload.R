test_that(".projr_osf_upload_dir works", {
  # skips
  skip_if_offline()
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # create previous upload
      # ---------------------------

      yml_projr_orig <- projr_yml_get_unchecked()

      # create files
      .projr_test_setup_content("output")

      # create manifest
      manifest <- .projr_test_manifest_create()

      # create project
      osf_tbl_proj <- .projr_osf_create_project("ProjectUpload")
      .projr_osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      # upload to OSF
      .projr_osf_upload_dir(osf_tbl = osf_tbl_proj, path_dir = "_output")

      # check that files are present on OSF
      path_dir_dnld <- file.path(tempdir(), "osf_dnld")
      if (!dir.exists(path_dir_dnld)) {
        dir.create(path_dir_dnld, recursive = TRUE)
      }
      withr::defer(unlink(path_dir_dnld, recursive = TRUE))
      osf_tbl_dir <- osf_tbl_proj |> osfr::osf_ls_files()
      osfr_tbl_dir_output <- osf_tbl_dir[osf_tbl_dir[["name"]] == "_output", ]
      osfr::osf_download(
        x = osfr_tbl_dir_output, path = path_dir_dnld, recurse = TRUE
      )
      expect_identical(list.files(path_dir_dnld), "_output")
      expect_identical(
        list.files(file.path(path_dir_dnld, "_output")),
        c("abc.txt", "subdir1")
      )
      expect_identical(
        list.files(file.path(path_dir_dnld, "_output", "subdir1")),
        c("def.txt", "subdir2")
      )
      expect_identical(
        list.files(file.path(path_dir_dnld, "_output", "subdir1", "subdir2")),
        c("ghi.txt")
      )
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
