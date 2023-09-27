test_that(".projr_osf_download_node_manifest", {
  # skips
  skip_if_offline()
  skip_if(TRUE)

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
      file.create(
        projr_path_get("output", "abc", output_safe = FALSE)
      )
      file.create(
        projr_path_get("output", "subdir", "def.txt", output_safe = FALSE)
      )
      file.create(
        projr_path_get(
          "output", "subdir", "subdir2", "ghi.txt",
          output_safe = FALSE
        )
      )

      # create manifest
      manifest <- .projr_build_manifest_hash_post(TRUE)
      .projr_manifest_write(
        manifest,
        output_run = TRUE
      )
      # upload
      osf_tbl <- .projr_osf_get_node(
        title = "Test",
        yml_param = list(public = FALSE, category = NULL),
        parent_id = "q26c9"
      )
      osfr::osf_upload(
        x = osf_tbl, path = projr_path_get("project", "manifest.csv")
      )
      expect_identical(
        .projr_osf_download_node_manifest(osf_tbl),
        manifest
      )

      # next steps:
      # 1. do some files
      # 2. make changes, check that they happen as expected
    },
    quiet = TRUE,
    force = TRUE
  )

  # teardown
  tryCatch({
    osf_tbl <- .projr_osf_get_node(
      "Test",
      yml_param = list(), parent_id = "q26c9"
    )
    osfr::osf_rm(osf_tbl, check = FALSE, recurse = TRUE)
  })
})
