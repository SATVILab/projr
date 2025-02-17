test_that(".osf_send_fn works", {
  # skips
  skip_if_offline()
  skip_if(FALSE)

  # setup
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # setup
      # ----------------

      yml_projr_orig <- projr_yml_get()

      # create files
      .test_setup_content("output")

      # create manifest
      manifest <- .test_manifest_create()

      # create project
      osf_tbl_proj <- .test_osf_create_project("ProjectUploadFn")
      .osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      path_dir_fn <-projr_path_get_dir("output", safe = FALSE)
      fn_vec <- list.files(path_dir_fn, recursive = TRUE)

      # upload to node
      # --------------------

      # upload
      .osf_send_fn(
        fn_rel = fn_vec, path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_proj, conflict = "overwrite"
      )
      fn_vec_osf <- .osf_ls_files(osf_tbl = osf_tbl_proj)
      expect_identical(fn_vec, fn_vec_osf)
    }
  )
})
