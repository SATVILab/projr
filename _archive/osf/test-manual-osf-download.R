test_that(".projr_checkout_osf works", {
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

      # create files
      .projr_test_setup_content("raw-data")

      # create manifest
      manifest <- .projr_test_manifest_create()

      # create project
      osf_tbl_proj <- .projr_test_osf_create_project("ProjectDownloadManifest")
      .projr_osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      # upload to OSF
      osf_tbl_upload <- osfr::osf_mkdir(x = osf_tbl_proj, path = "raw-data")
      osf_tbl_dir <- with_dir(
        "_data_raw",
        {
          .projr_osf_upload_dir(
            osf_tbl = osf_tbl_upload, path_dir = "."
          )
        }
      )
      # add to YAML config
      projr_osf_source_add(
        label = "raw-data", id = osf_tbl_proj[["id"]]
      )

      # remove to test restore
      unlink("_data_raw", recursive = TRUE)
      dir.create("_data_raw")

      # restore from a `latest` folder
      # ------------------------
      .projr_checkout_osf("raw-data")
      # check files there
      expect_identical(
        list.files(file.path("_data_raw")), c("abc.txt", "subdir1")
      )
      expect_identical(
        list.files(file.path("_data_raw", "subdir1")), c("def.txt", "subdir2")
      )
      expect_identical(
        list.files(file.path("_data_raw", "subdir1", "subdir2")), c("ghi.txt")
      )

      # restore from a versioned folder
      # ------------------------
      osfr::osf_rm(osf_tbl_upload, recurse = TRUE, check = FALSE)
      osf_tbl_upload <- osfr::osf_mkdir(
        x = osf_tbl_proj, path = "raw-data/v1.0.1"
      )
      osf_tbl_dir <- with_dir(
        "_data_raw",
        .projr_osf_upload_dir(osf_tbl = osf_tbl_upload, path_dir = ".")
      )
      # remove downloaded files
      unlink("_data_raw", recursive = TRUE)
      dir.create("_data_raw")

      # add to YAML config
      projr_osf_source_add(
        label = "raw-data", id = osf_tbl_proj[["id"]],
        remote_structure = "version", overwrite = TRUE
      )

      # remove to test restore
      unlink("_data_raw", recursive = TRUE)
      dir.create("_data_raw")
      .projr_checkout_osf("raw-data", )
      # check files there
      expect_identical(
        list.files(file.path("_data_raw")), c("abc.txt", "subdir1")
      )
      expect_identical(
        list.files(file.path("_data_raw", "subdir1")), c("def.txt", "subdir2")
      )
      expect_identical(
        list.files(file.path("_data_raw", "subdir1", "subdir2")), c("ghi.txt")
      )

      # remove downloaded files
      if (dir.exists("_data_raw")) unlink("_data_raw", recursive = TRUE)
      dir.create("_data_raw")

      # restore from a versioned folder but not the latest
      # -----------------------------

      # create files
      .projr_test_setup_content("raw-data")
      file.create("_data_raw/add.txt")

      # add a new version
      osf_tbl_upload <- osfr::osf_mkdir(
        x = osf_tbl_proj, path = "raw-data/v2.0.1"
      )
      osf_tbl_dir <- with_dir(
        "_data_raw",
        .projr_osf_upload_dir(osf_tbl = osf_tbl_upload, path_dir = ".")
      )

      # remove downloaded files
      unlink("_data_raw", recursive = TRUE)
      dir.create("_data_raw")
      # checkout
      .projr_checkout_osf("raw-data", version = "1.5")
      # check files there
      expect_identical(
        list.files(file.path("_data_raw")), c("abc.txt", "subdir1")
      )
      expect_identical(
        list.files(file.path("_data_raw", "subdir1")), c("def.txt", "subdir2")
      )
      expect_identical(
        list.files(file.path("_data_raw", "subdir1", "subdir2")), c("ghi.txt")
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".projr_osf_download_node_label works", {
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

      # create files
      .projr_test_setup_content("raw-data")

      # create manifest
      manifest <- .projr_test_manifest_create()

      # upload
      osf_tbl_proj <- .projr_test_osf_create_project("ProjectDownloadManifest")
      .projr_osf_rm_node_id_defer(osf_tbl_proj[["id"]])

      # add source directory
      id_source <- projr_osf_source_add(
        label = "raw-data",
        parent_id = osf_tbl_proj[["id"]],
        path_append_label = FALSE
      )

      # get source node
      osf_tbl_source <- .projr_osf_get_node_id(id_source)

      # upload manifest
      osfr::osf_upload(x = osf_tbl_proj, path = "manifest.csv")

      # add source
      osf_tbl_source <- .projr_osf_create_node(
        title = "raw-data",
        parent_id = osf_tbl_proj[["id"]]
      )

      # upload files
      osfr::osf_upload(
        x = osf_tbl_proj,
        path = "raw-data",
        conflicts = "overwrite"
      )

      # next steps:
      # 1. do some files
      # 2. make changes, check that they happen as expected
    },
    quiet = TRUE,
    force = TRUE
  )
})
