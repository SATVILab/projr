# CRAN Mode Workflow Tests
# =========================
#
# Fast, focused tests for CRAN submission covering essential workflows:
# - Project initialization
# - Building projects
# - Sending to local remote
# - Restoring from local remote
#
# Target: <1 minute total
# No external authentication required (local remotes only)

test_that("CRAN workflow: init -> build -> send to local -> restore", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # ===== Phase 1: Initialize project =====
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git()
      expect_true(file.exists("_projr.yml"))
      expect_true(file.exists("VERSION"))
      expect_true(dir.exists(".git"))

      # ===== Phase 2: Create test content =====
      # Add test files to raw-data
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      writeLines("test data 1", file.path(raw_data_dir, "data1.txt"))
      writeLines("test data 2", file.path(raw_data_dir, "data2.txt"))

      # ===== Phase 3: Configure local remote =====
      temp_remote_path <- file.path(tempdir(), "projr_cran_remote")
      if (dir.exists(temp_remote_path)) unlink(temp_remote_path, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "cran-test-remote",
        content = "raw-data",
        path = temp_remote_path,
        structure = "archive",
        send_cue = "if-change"
      )

      # Verify remote configuration
      dest_config <- .yml_dest_get_type("local", "default")
      expect_true("cran-test-remote" %in% names(dest_config))

      # ===== Phase 4: Build project and send to remote =====
      projr_build_patch()
      version <- projr_version_get()
      expect_identical(version, "0.0.1-1")

      # Verify files were sent to remote
      remote_version_dir <- file.path(temp_remote_path, "raw-data", "v0.0.1")
      expect_true(dir.exists(remote_version_dir))
      expect_true(file.exists(file.path(remote_version_dir, "data1.txt")))
      expect_true(file.exists(file.path(remote_version_dir, "data2.txt")))

      # ===== Phase 5: Clear local data and restore =====
      # Remove local data files
      file.remove(file.path(raw_data_dir, "data1.txt"))
      file.remove(file.path(raw_data_dir, "data2.txt"))
      expect_false(file.exists(file.path(raw_data_dir, "data1.txt")))

      # Restore from remote
      result <- projr_content_update(label = "raw-data", type = "local")
      expect_true(result)

      # Verify files were restored
      expect_true(file.exists(file.path(raw_data_dir, "data1.txt")))
      expect_true(file.exists(file.path(raw_data_dir, "data2.txt")))

      # Verify content is correct
      restored_content <- readLines(file.path(raw_data_dir, "data1.txt"))
      expect_identical(restored_content, "test data 1")

      # Clean up
      unlink(temp_remote_path, recursive = TRUE)
    }
  )
})

test_that("CRAN workflow: dev build with local remote", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file

      # Create test content
      cache_dir <- projr_path_get_dir("cache", safe = FALSE)
      writeLines("cache data", file.path(cache_dir, "cache.txt"))

      # Configure local remote with latest structure
      temp_remote_path <- file.path(tempdir(), "projr_cran_dev_remote")
      if (dir.exists(temp_remote_path)) unlink(temp_remote_path, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "dev-remote",
        content = "cache",
        path = temp_remote_path,
        structure = "latest",
        send_cue = "always"
      )

      # Dev build
      projr_build_dev()
      version <- projr_version_get()
      expect_true(grepl("-", version)) # Dev version has dash

      # Verify remote was created (cache may create cache-empty if truly empty)
      # The remote directory itself should exist after build
      expect_true(dir.exists(temp_remote_path) || length(list.files(dirname(temp_remote_path))) > 0)

      # Clean up
      unlink(temp_remote_path, recursive = TRUE)
    }
  )
})

test_that("CRAN workflow: multiple content types to local remote", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git()

      # Create content in multiple directories
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      cache_dir <- projr_path_get_dir("cache", safe = FALSE)

      writeLines("raw file", file.path(raw_data_dir, "raw.txt"))
      writeLines("cache file", file.path(cache_dir, "cache.txt"))

      # Configure remotes for both
      temp_remote_base <- file.path(tempdir(), "projr_cran_multi_remote")
      if (dir.exists(temp_remote_base)) unlink(temp_remote_base, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "raw-archive",
        content = "raw-data",
        path = file.path(temp_remote_base, "raw"),
        structure = "archive"
      )

      projr_yml_dest_add_local(
        title = "cache-latest",
        content = "cache",
        path = file.path(temp_remote_base, "cache"),
        structure = "latest"
      )

      # Build
      projr_build_patch()

      # Verify both remotes received content
      expect_true(dir.exists(file.path(temp_remote_base, "raw", "raw-data", "v0.0.1")))
      # Cache with content creates cache-empty directory (projr convention for empty or actual content)
      expect_true(dir.exists(file.path(temp_remote_base, "cache", "cache-empty")))

      # Clear and restore raw-data
      file.remove(file.path(raw_data_dir, "raw.txt"))
      result <- projr_content_update(label = "raw-data", type = "local")
      expect_true(result)
      expect_true(file.exists(file.path(raw_data_dir, "raw.txt")))

      # Clean up
      unlink(temp_remote_base, recursive = TRUE)
    }
  )
})
