# Dedicated OSF Remote Tests
#
# This file is the ONLY testthat file that:
# - Creates or deletes OSF nodes via projr's wrappers
# - Calls OSF-specific remote helpers (.remote_file_add_osf(), etc.)
# - Tests OSF node functionality
#
# All tests skip in CRAN, LITE, and FAST modes, and require OSF credentials.

# =============================================================================
# Basic OSF Remote Creation and Existence
# =============================================================================

test_that(".remote_create works for OSF", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create OSF project
      id_parent <- .test_osf_create_project("ProjectParent")
      expect_true(.remote_check_exists("osf", id_parent))
      env <- environment()
      .osf_rm_node_id_defer(id_parent, env)

      # Create OSF component
      id_comp <- try(.remote_create(
        type = "osf", name = "CreateComp", id_parent = id_parent,
        category = "data"
      ))
      expect_true(.remote_check_exists("osf", id_comp))
      .osf_rm_node_id_defer(id_comp, env)
    }
  )
})

test_that(".remote_get works for OSF", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test OSF project
      id <- .test_osf_create_project("TestProject")
      env <- environment()
      .osf_rm_node_id_defer(id, env)

      # Test remote_get
      remote <- .remote_get("osf", id)
      expect_identical(remote[["id"]], id)
    }
  )
})

test_that(".remote_get_final works for OSF", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test OSF project
      id <- .test_osf_create_project("TestProject")
      env <- environment()
      .osf_rm_node_id_defer(id, env)

      # Test without sub-directory
      expect_error(
        .remote_final_get(
          "osf",
          id = id, path_append_label = TRUE
        )[["id"]]
      )
      expect_identical(
        .remote_final_get(
          "osf",
          id = id, path_append_label = FALSE,
          structure = "latest"
        )[["id"]],
        id
      )

      # Test with sub-directory
      path_rel <- "a/raw-data/v0.0.0-1"
      osf_tbl <- .osf_mkdir(.remote_get("osf", id), path_rel)
      expect_identical(
        .remote_final_get_osf(
          id = id,
          path = "a",
          path_append_label = TRUE,
          label = "raw-data",
          structure = "archive"
        ),
        osf_tbl
      )
    }
  )
})

# =============================================================================
# OSF File Operations
# =============================================================================

test_that("adding, listing and removing files works for OSF", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create OSF node
      id <- .test_osf_create_project("TestProject")
      env <- environment()
      .osf_rm_node_id_defer(id, env)
      osf_tbl <- .remote_get("osf", id)

      # Test when empty
      expect_identical(
        .remote_file_ls("osf", remote = osf_tbl),
        character()
      )

      # Add content
      path_dir_source <- .test_content_setup_dir()
      fn_vec_source <- .remote_file_ls("local", path_dir_source)
      .remote_file_add(
        "osf",
        fn = fn_vec_source,
        path_dir_local = path_dir_source,
        remote = osf_tbl
      )

      # Poll for upload
      start_time <- proc.time()[3]
      max_wait <- 60
      file_list <- .remote_file_ls("osf", osf_tbl)
      while (length(file_list) == 0L && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(3)
        file_list <- .remote_file_ls("osf", osf_tbl)
      }

      # Verify upload
      expect_identical(file_list, fn_vec_source)

      # Remove some content
      fn_vec_orig_osf <- .remote_file_ls("osf", osf_tbl)
      fn_vec_rm <- c("abc.txt", "subdir1/def.txt")
      expect_true(
        .remote_file_rm("osf", fn = fn_vec_rm, remote = osf_tbl)
      )

      # Poll for removal
      start_time <- proc.time()[3]
      max_wait <- 30
      file_list <- .remote_file_ls("osf", remote = osf_tbl)
      expected_list <- setdiff(fn_vec_orig_osf, fn_vec_rm)
      while (!identical(file_list, expected_list) && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(3)
        file_list <- .remote_file_ls("osf", remote = osf_tbl)
      }

      # Verify removal
      expect_identical(file_list, expected_list)

      # Cleanup
      unlink(path_dir_source, recursive = TRUE)
    }
  )
})

test_that(".remote_file_rm_all works for OSF", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create OSF node
      id <- .test_osf_create_project("TestProject")
      env <- environment()
      .osf_rm_node_id_defer(id, env)
      osf_tbl <- .remote_get("osf", id)

      # Test when empty
      expect_false(
        .remote_final_empty("osf", remote = osf_tbl)
      )

      # Add content
      osf_tbl_sub_a <- .osf_mkdir(osf_tbl, path = "a")
      osf_tbl_sub_b <- .osf_mkdir(osf_tbl, path = "a/b")
      path_tmp_file <- file.path(tempdir(), "test-file.txt")
      file.create(path_tmp_file)
      writeLines("test content", path_tmp_file)

      .osf_upload(x = osf_tbl, path = path_tmp_file, conflicts = "overwrite")
      .osf_upload(x = osf_tbl_sub_a, path = path_tmp_file, conflicts = "overwrite")
      .osf_upload(x = osf_tbl_sub_b, path = path_tmp_file, conflicts = "overwrite")

      # Poll for uploads
      start_time <- proc.time()[3]
      max_wait <- 60
      file_count <- nrow(.osf_ls_files(osf_tbl))
      while (file_count == 0L && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(3)
        file_count <- nrow(.osf_ls_files(osf_tbl))
      }

      # Remove all content
      expect_true(
        .remote_final_empty("osf", remote = osf_tbl)
      )

      # Poll for removal (reuse existing pattern from later in file)
      start_time <- proc.time()[3]
      max_wait <- 60
      is_zero <- (nrow(.osf_ls_files(osf_tbl)) == 0L)
      while (!is_zero && (proc.time()[3] - start_time) < max_wait) {
        Sys.sleep(3)
        is_zero <- (nrow(.osf_ls_files(osf_tbl)) == 0L)
      }

      # Verify removal
      expect_true(is_zero)
    }
  )
})

# =============================================================================
# OSF Empty Remote Removal
# =============================================================================

test_that(".remote_final_rm_if_empty works for OSF", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create OSF node
      id <- .test_osf_create_project("TestProject")
      env <- environment()
      .osf_rm_node_id_defer(id, env)
      osf_tbl <- .remote_get("osf", id)

      # Test with node (not sub-dir) - should return FALSE
      expect_false(
        .remote_final_rm_if_empty("osf", remote = osf_tbl)
      )

      # Create sub-directory
      osf_tbl_file <- .remote_final_get(
        "osf",
        id = id, path_append_label = FALSE, structure = "archive"
      )

      # Remove empty sub-directory - should return TRUE
      expect_true(
        .remote_final_rm_if_empty("osf", remote = osf_tbl_file)
      )

      # Wait for removal
      start_time <- proc.time()[3]
      is_zero <- (nrow(.osf_ls_files(osf_tbl)) == 0L)
      while (!is_zero && (proc.time()[3] - start_time) < 15) {
        Sys.sleep(3)
        is_zero <- (nrow(.osf_ls_files(osf_tbl)) == 0L)
      }
      expect_true(is_zero)

      # Create sub-directory with content
      osf_tbl_file <- .remote_final_get(
        "osf",
        id = id, path_append_label = FALSE, structure = "archive"
      )
      path_tmp_file <- file.path(tempdir(), "test.txt")
      file.create(path_tmp_file)
      .osf_upload(x = osf_tbl_file, path = path_tmp_file)

      # Wait for upload
      Sys.sleep(5)

      # Try to remove non-empty sub-directory - should return FALSE
      expect_false(
        .remote_final_rm_if_empty("osf", remote = osf_tbl_file)
      )
      expect_identical(nrow(.osf_ls_files(osf_tbl)), 1L)
    }
  )
})
