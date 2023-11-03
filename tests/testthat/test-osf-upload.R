test_that(".projr_osf_send_fn works", {
  # skips
  skip_if_offline()
  skip_if(TRUE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # setup
      # ----------------

      yml_projr_orig <- projr_yml_get_unchecked()

      # create files
      .projr_test_setup_content("output")

      # create manifest
      manifest <- .projr_test_manifest_create()

      # create project
      osf_tbl_proj <- .projr_test_osf_create_project("ProjectUploadFn")
      .projr_osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      path_dir_fn <- projr_dir_get("output", output_safe = FALSE)
      fn_vec <- list.files(path_dir_fn, recursive = TRUE)

      # upload to node
      # --------------------

      # upload
      .projr_osf_send_fn(
        fn_rel = fn_vec, path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_proj, conflict = "overwrite"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "node")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_proj, path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )

      # upload to sub-directory of node
      # ----------------------
      osf_tbl_file <- osfr::osf_mkdir(
        x = osf_tbl_proj, path = "output/v1.0.0"
      )
      # upload
      .projr_osf_send_fn(
        fn_rel = fn_vec, path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_file, conflict = "overwrite"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "dir")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file, path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )
    }
  )
})

test_that(".projr_osf_send_dir and _missing work", {
  # skips
  skip_if_offline()
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # setup
      # ----------------

      yml_projr_orig <- projr_yml_get_unchecked()

      # create files
      .projr_test_setup_content("data-raw")

      # create manifest
      manifest <- .projr_test_manifest_create()

      # create project
      osf_tbl_proj <- .projr_test_osf_create_project("ProjectUploadFn")
      .projr_osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      path_dir_fn <- projr_dir_get("data-raw", output_safe = FALSE)
      fn_vec <- list.files(path_dir_fn, recursive = TRUE)

      # upload to node
      # --------------------

      # upload
      .projr_osf_send_dir(
        path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_proj, conflict = "overwrite"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "node")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_proj, path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )

      # upload to sub-directory of node
      # ----------------------
      osf_tbl_file <- osfr::osf_mkdir(
        x = osf_tbl_proj, path = "output/test_dir"
      )
      # upload
      .projr_osf_send_dir(
        path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_file, conflict = "overwrite"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "dir")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file, path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )

      # upload only missing files - version_source - manifest
      # ----------------------

      # create extra files
      path_file_extra_init <- projr_path_get(
        "data-raw", "extra.txt"
      )
      path_file_extra_sub <- projr_path_get(
        "data-raw", "altdir1", "extra2.txt"
      )
      path_file_extra_sub_sub <- projr_path_get(
        "data-raw", "altdir1", "altdir2", "extra3.txt"
      )

      path_file_extra <- c(
        path_file_extra_init, path_file_extra_sub, path_file_extra_sub_sub
      )
      for (x in path_file_extra) file.create(path_file_extra)
      fn_vec <- list.files(projr_path_get("data-raw"), recursive = TRUE)
      projr_version_set("0.0.1")
      .projr_test_manifest_create()

      # upload
      .projr_osf_send_missing(
        path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_file,
        conflict = "overwrite",
        label = "data-raw",
        version_source = "manifest"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "missing_manifest")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file,
        path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )


      # upload only missing files - version_source - osf
      # ----------------------

      # create files
      browser()
      .projr_test_setup_content("output")
      path_dir_fn_output <- projr_dir_get("output", output_safe = FALSE)
      fn_vec <- list.files(path_dir_fn_output, recursive = TRUE)
      osf_tbl_file_output <- osfr::osf_mkdir(osf_tbl_proj, "output")
      osfr::osf_rm(osf_tbl_file_output, check = FALSE, recurse = TRUE)
      osf_tbl_file_output <- osfr::osf_mkdir(osf_tbl_proj, "output")

      # upload
      .projr_osf_send_missing(
        path_dir_local = path_dir_fn_output,
        osf_tbl = osf_tbl_file_output,
        conflict = "overwrite",
        label = "output",
        version_source = "osf"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "missing_osf")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file_output,
        path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        list.files(path_dir_save_local, recursive = TRUE),
        fn_vec
      )
      unlink(path_dir_fn_output, recursive = TRUE)

      # synchronise based on versions - version
      # ----------------------

      browser()
      manifest <- .projr_test_manifest_create()

      # will upload to a different directory as latest two versions
      # on record different, so expect a new versioned directory
      # to appear

      # get path to upload to (will be needed)
      path_dir_osf <- .projr_osf_path_get(
        osf_tbl = osf_tbl_proj,
        path = "abc",
        path_append_label = TRUE,
        label = "data-raw",
        remote_structure = "version",
        version = NULL
      )
      osf_tbl_file <- osf_tbl_proj |> osfr::osf_mkdir(path_dir_osf)
      path_dir_local <- projr_dir_get("data-raw")
      # upload
      .projr_osf_send_version(
        path_dir_local = path_dir_local,
        label = "data-raw",
        osf_tbl_upload = osf_tbl_file,
        version_source = "manifest"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "version")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file,
        path_dir_save_local = path_dir_save_local
      )
      fn_vec <- list.files(projr_dir_get("data-raw"), recursive = TRUE)
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )

      # bump version, but expect no upload as nothing changed
      projr_version_set("0.0.2")
      .projr_test_manifest_create()

      path_dir_local <- projr_dir_get("data-raw")
      # upload
      .projr_osf_send_version(
        path_dir_local = path_dir_local,
        label = "data-raw",
        osf_tbl_upload = osf_tbl_file,
        version_source = "manifest"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "version_unchanged")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file,
        path_dir_save_local = path_dir_save_local
      )
      fn_vec <- list.files(projr_dir_get("data-raw"), recursive = TRUE)
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )
      osf_tbl_dir <- osf_tbl_proj |> osfr::osf_mkdir(dirname(path_dir_osf))
      expect_identical(nrow(osf_tbl_dir |> osfr::osf_ls_files()), 1L)
      expect_identical(osfr::osf_ls_files(osf_tbl_dir)[["name"]], "v0.0.1")

      # now remove a file, add a file and change a file
      # RESTART HERE - SHOULD WORK
      file.remove(projr_path_get("data-raw", "extra.txt"))
      unlink(projr_dir_get("data-raw", "altdir1"), recursive = TRUE)

      # add a file
      file.create(projr_path_get("data-raw", "add.txt"))

      # change a file
      writeLines(c("change"), projr_path_get("data-raw", "abc.txt"))

      projr_version_set("v0.0.3")
      manifest <- .projr_test_manifest_create()
      # upload
      .projr_osf_send_version(
        path_dir_local = path_dir_local,
        label = "data-raw",
        osf_tbl_upload = osf_tbl_file,
        version_source = "manifest"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "version_changed")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file,
        path_dir_save_local = path_dir_save_local
      )
      fn_vec <- list.files(projr_dir_get("data-raw"), recursive = TRUE)
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )
      osf_tbl_dir <- osf_tbl_proj |> osfr::osf_mkdir(dirname(path_dir_osf))
      expect_identical(nrow(osf_tbl_dir |> osfr::osf_ls_files()), 1L)
      expect_identical(
        osfr::osf_ls_files(osf_tbl_dir)[["name"]], c("v0.0.1", "v0.0.3")
      )




      # synchronise based on versions - latest
      # ----------------------

      # remove all extra files
      file.remove(projr_path_get("data-raw", "extra.txt"))
      unlink(projr_dir_get("data-raw", "altdir1"), recursive = TRUE)

      # add a file
      file.create(projr_path_get("data-raw", "add.txt"))

      # change a file
      writeLines(c("change"), projr_path_get("data-raw", "abc.txt"))



      projr_manifest_compare(
        manifest_
      )

      # upload
      .projr_osf_send_missing(
        path_dir_local = path_dir_fn,
        osf_tbl = osf_tbl_file,
        conflict = "overwrite"
      )

      # check
      path_dir_save_local <- projr_dir_get("cache", "missing")
      .projr_osf_download_osf_tbl(
        osf_tbl = osf_tbl_file,
        path_dir_save_local = path_dir_save_local
      )
      expect_identical(
        fn_vec, list.files(path_dir_save_local, recursive = TRUE)
      )
      browser()
      browser()
      browser()
    }
  )
})

test_that(".projr_osf_send_yml_label works", {
  # skips
  skip_if_offline()
  skip_if(TRUE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # setup
      # ----------------

      yml_projr_orig <- projr_yml_get_unchecked()

      # create files
      .projr_test_setup_content("output")

      # create manifest
      manifest <- .projr_test_manifest_create()

      # create project
      osf_tbl_proj <- .projr_test_osf_create_project("ProjectUpload")
      .projr_osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      # test
      osf_tbl_file <- osf_tbl_projr |> osfr::osf_ls_files(n_max = Inf)
      path_osf <- NULL
      path_local <- projr_dir_get("output", output_safe = FALSE)

      .projr_osf_send_yml_label(
        osf_tbl = osf_tbl,
        osf_tbl_file = osf_tbl_file,
        path_osf = path_osf,
        path_local = path_local,
        conflict = "overwrite"
      )
    }
  )
})
