# LITE Mode Workflow Tests
# =========================
#
# Focused tests for LITE mode covering essential workflows with more scenarios:
# - Project initialization with different engines
# - Building projects (patch, minor, dev)
# - Sending to local remotes (archive, latest)
# - Restoring from local remotes
# - Git integration
#
# Target: Part of <5 minute LITE test suite
# No external authentication required (local remotes only)

test_that("LITE workflow: bookdown init -> build -> send -> restore with git", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git()

      # Create test content
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      writeLines("data v1", file.path(raw_data_dir, "data.txt"))
      dir.create(file.path(raw_data_dir, "subdir"), showWarnings = FALSE)
      writeLines("nested data", file.path(raw_data_dir, "subdir", "nested.txt"))

      # Configure local remote with archive
      temp_remote <- file.path(tempdir(), "projr_lite_remote1")
      if (dir.exists(temp_remote)) unlink(temp_remote, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "lite-archive",
        content = "raw-data",
        path = temp_remote,
        structure = "archive",
        send_cue = "if-change"
      )

      # First build
      projr_build_patch()
      expect_identical(projr_version_get(), "0.0.1-1")

      # Verify remote has v0.0.1
      expect_true(dir.exists(file.path(temp_remote, "raw-data", "v0.0.1")))
      expect_true(file.exists(file.path(temp_remote, "raw-data", "v0.0.1", "data.txt")))

      # Modify content
      writeLines("data v2", file.path(raw_data_dir, "data.txt"))

      # Second build
      projr_build_patch()
      expect_identical(projr_version_get(), "0.0.2-1")

      # Verify remote has v0.0.2
      expect_true(dir.exists(file.path(temp_remote, "raw-data", "v0.0.2")))

      # Clear and restore
      file.remove(file.path(raw_data_dir, "data.txt"))
      result <- projr_content_update(label = "raw-data", type = "local")
      expect_true(result)

      # Should restore latest version
      restored <- readLines(file.path(raw_data_dir, "data.txt"))
      expect_identical(restored, "data v2")

      unlink(temp_remote, recursive = TRUE)
    }
  )
})

test_that("LITE workflow: switch engines and rebuild", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Start with bookdown (don't remove engine)
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git()

      cache_dir <- projr_path_get_dir("cache", safe = FALSE)
      writeLines("cache", file.path(cache_dir, "cache.txt"))

      # Configure remote
      temp_remote <- file.path(tempdir(), "projr_lite_remote2")
      if (dir.exists(temp_remote)) unlink(temp_remote, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "cache-remote",
        content = "cache",
        path = temp_remote,
        structure = "latest"
      )

      # First build with bookdown
      projr_build_patch()
      expect_identical(projr_version_get(), "0.0.1-1")
      expect_identical(.engine_get(), "bookdown")

      # Switch to standalone RMarkdown
      file.remove("_bookdown.yml")
      file.remove("index.Rmd")
      if (file.exists("_output.yml")) file.remove("_output.yml")

      writeLines(
        c(
          "---",
          "title: Document",
          "output: html_document",
          "---",
          "Content"
        ),
        "doc.Rmd"
      )

      projr_build_patch()
      expect_identical(projr_version_get(), "0.0.2-1")
      expect_identical(.engine_get(), "rmd")

      unlink(temp_remote, recursive = TRUE)
    }
  )
})

test_that("LITE workflow: dev builds and version bumps", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Git is needed for builds to work properly
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git() # Initialize git for builds

      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("output", file.path(output_dir, "out.txt"))

      # Configure remote
      temp_remote <- file.path(tempdir(), "projr_lite_remote3")
      if (dir.exists(temp_remote)) unlink(temp_remote, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "output-archive",
        content = "output",
        path = temp_remote,
        structure = "archive"
      )

      # Dev build
      projr_build_dev()
      v1 <- projr_version_get()
      expect_true(grepl("-", v1))

      # Production patch (sends to remote)
      projr_build_patch()
      v2 <- projr_version_get()
      expect_identical(v2, "0.0.1-1")

      # Dev build again
      projr_build_dev()
      v3 <- projr_version_get()
      expect_true(grepl("0\\.0\\.1-", v3))

      # Minor bump (sends to remote)
      projr_build_minor()
      v4 <- projr_version_get()
      expect_identical(v4, "0.1.0-1")

      # Verify production builds created archive versions
      expect_true(dir.exists(file.path(temp_remote, "output")))
      # Check that at least one version was archived
      archive_dirs <- list.dirs(file.path(temp_remote, "output"), recursive = FALSE)
      expect_true(length(archive_dirs) >= 1)

      unlink(temp_remote, recursive = TRUE)
    }
  )
})

test_that("LITE workflow: send strategies and cues", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git()

      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      writeLines("original", file.path(raw_data_dir, "file.txt"))

      # Remote 1: if-change cue
      remote1 <- file.path(tempdir(), "projr_lite_remote4a")
      if (dir.exists(remote1)) unlink(remote1, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "if-change",
        content = "raw-data",
        path = remote1,
        structure = "archive",
        send_cue = "if-change"
      )

      # Remote 2: always cue
      remote2 <- file.path(tempdir(), "projr_lite_remote4b")
      if (dir.exists(remote2)) unlink(remote2, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "always",
        content = "raw-data",
        path = remote2,
        structure = "archive",
        send_cue = "always"
      )

      # First build
      projr_build_patch()
      expect_true(dir.exists(file.path(remote1, "raw-data", "v0.0.1")))
      expect_true(dir.exists(file.path(remote2, "raw-data", "v0.0.1")))

      # Second build without changes
      projr_build_patch()

      # if-change should NOT create v0.0.2
      expect_false(dir.exists(file.path(remote1, "raw-data", "v0.0.2")))
      # always SHOULD create v0.0.2
      expect_true(dir.exists(file.path(remote2, "raw-data", "v0.0.2")))

      unlink(remote1, recursive = TRUE)
      unlink(remote2, recursive = TRUE)
    }
  )
})

test_that("LITE workflow: restore with multiple versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init() # Creates directories
      projr_version_set("0.0.0-1", only_if_exists = FALSE) # Create VERSION file
      projr_init_git()

      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      temp_remote <- file.path(tempdir(), "projr_lite_remote5")
      if (dir.exists(temp_remote)) unlink(temp_remote, recursive = TRUE)

      projr_yml_dest_add_local(
        title = "multi-version",
        content = "raw-data",
        path = temp_remote,
        structure = "archive",
        send_cue = "always"
      )

      # Build version 0.0.1
      writeLines("v1 data", file.path(raw_data_dir, "data.txt"))
      projr_build_patch()

      # Build version 0.0.2
      writeLines("v2 data", file.path(raw_data_dir, "data.txt"))
      projr_build_patch()

      # Build version 0.0.3
      writeLines("v3 data", file.path(raw_data_dir, "data.txt"))
      projr_build_patch()

      # Verify all versions in remote
      expect_true(dir.exists(file.path(temp_remote, "raw-data", "v0.0.1")))
      expect_true(dir.exists(file.path(temp_remote, "raw-data", "v0.0.2")))
      expect_true(dir.exists(file.path(temp_remote, "raw-data", "v0.0.3")))

      # Clear local
      file.remove(file.path(raw_data_dir, "data.txt"))

      # Restore (should get latest)
      result <- projr_content_update(label = "raw-data", type = "local")
      expect_true(result)

      # Should have v3
      restored <- readLines(file.path(raw_data_dir, "data.txt"))
      expect_identical(restored, "v3 data")

      unlink(temp_remote, recursive = TRUE)
    }
  )
})
