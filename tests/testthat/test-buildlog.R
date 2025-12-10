# Test buildlog system resources functionality

test_that(".buildlog_get_system_resources works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Get system resources
  sys_resources <- .buildlog_get_system_resources()

  # Check that it returns a character vector
  expect_true(is.character(sys_resources))

  # Check that it contains expected sections
  expect_true(any(grepl("\\*\\*System Resources\\*\\*", sys_resources)))
  expect_true(any(grepl("- OS:", sys_resources)))
  expect_true(any(grepl("- CPU Cores:", sys_resources)))
  expect_true(any(grepl("- Total RAM:", sys_resources)))
  expect_true(any(grepl("- Disk Space:", sys_resources)))
  expect_true(any(grepl("- Architecture:", sys_resources)))
  expect_true(any(grepl("- Platform:", sys_resources)))

  # Check that CPU cores is a positive number
  cpu_line <- sys_resources[grepl("- CPU Cores:", sys_resources)]
  cpu_cores <- as.numeric(gsub(".*: ", "", cpu_line))
  expect_true(cpu_cores > 0)
})

test_that(".buildlog_get_memory_info works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Get memory info
  mem_info <- .buildlog_get_memory_info()

  # Check that it returns a character string
  expect_true(is.character(mem_info))
  expect_true(length(mem_info) == 1)

  # Check that it contains RAM information
  expect_true(grepl("- Total RAM:", mem_info))
})

test_that(".buildlog_get_disk_info works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Get disk info
  disk_info <- .buildlog_get_disk_info()

  # Check that it returns a character string
  expect_true(is.character(disk_info))
  expect_true(length(disk_info) == 1)

  # Check that it contains disk space information
  expect_true(grepl("- Disk Space:", disk_info))
})

test_that(".buildlog_get_add includes system resources", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create sample inputs
  msg <- "Test build message"
  bump_component <- "patch"
  version_run_on_list <- list(
    "desc" = list("success" = "0.0.1")
  )
  total_time <- as.difftime(120, units = "secs")

  # Get the buildlog addition
  add_txt <- .buildlog_get_add(msg, bump_component, version_run_on_list, total_time)

  # Check that it includes system resources section
  expect_true(any(grepl("\\*\\*System Resources\\*\\*", add_txt)))
  expect_true(any(grepl("- OS:", add_txt)))
  expect_true(any(grepl("- CPU Cores:", add_txt)))
})

test_that(".buildlog_read works with empty file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Should return default header if file doesn't exist
      result <- .buildlog_read()
      expect_identical(result, c("# BUILDLOG", ""))
    }
  )
})

test_that(".buildlog_write creates file correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      test_content <- c("# BUILDLOG", "", "## v0.0.1", "Test content")
      .buildlog_write(test_content)

      # Verify file exists
      path_buildlog <- .path_get("BUILDLOG.md")
      expect_true(file.exists(path_buildlog))

      # Verify content
      result <- readLines(path_buildlog, warn = FALSE)
      expect_identical(result, test_content)
    }
  )
})

test_that(".buildlog_add creates entries correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add a buildlog entry
      .buildlog_add(
        msg = "Test build",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.1")),
        total_time = as.difftime(60, units = "secs")
      )

      content <- .buildlog_read()

      # Check header
      expect_true(any(grepl("# BUILDLOG", content)))

      # Check version
      expect_true(any(grepl("v0.0.1", content)))

      # Check message
      expect_true(any(grepl("Test build", content)))
    }
  )
})

test_that(".buildlog_get_metadata_time formats correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test various durations
  expect_match(.buildlog_get_metadata_time(as.difftime(30, units = "secs")), "30s")
  expect_match(.buildlog_get_metadata_time(as.difftime(90, units = "secs")), "1min 30s")
  expect_match(.buildlog_get_metadata_time(as.difftime(3661, units = "secs")), "1hr 1min 1s")
  expect_match(.buildlog_get_metadata_time(as.difftime(86460, units = "secs")), "1d 0hr 1min 0s")
})

test_that(".buildlog_get_version formats correctly for different bump types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      version_list <- list(desc = list(success = "1.2.3"))

      # Patch: 4 hashes
      patch_ver <- .buildlog_get_version(version_list, "patch")
      expect_match(patch_ver, "^#### v1.2.3$")

      # Minor: 3 hashes
      minor_ver <- .buildlog_get_version(version_list, "minor")
      expect_match(minor_ver, "^### v1.2.3$")

      # Major: 2 hashes
      major_ver <- .buildlog_get_version(version_list, "major")
      expect_match(major_ver, "^## v1.2.3$")
    }
  )
})

test_that(".buildlog_get_projr_yml includes yaml content", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      result <- .buildlog_get_projr_yml()

      expect_true(is.character(result))
      expect_true(any(grepl("\\*\\*`projr` config\\*\\*", result)))
      expect_true(any(grepl("```yaml", result)))
    }
  )
})

test_that(".buildlog_get_session_info includes session information", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  result <- .buildlog_get_session_info()

  expect_true(is.character(result))
  expect_true(any(grepl("\\*\\*Session info\\*\\*", result)))
  expect_true(any(grepl("R version", result)))
})

test_that("buildlog preserves existing entries", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add first entry
      .buildlog_add(
        msg = "First build",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.1")),
        total_time = as.difftime(30, units = "secs")
      )

      # Add second entry
      .buildlog_add(
        msg = "Second build",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.2")),
        total_time = as.difftime(45, units = "secs")
      )

      content <- .buildlog_read()

      # Both entries should be present
      expect_true(any(grepl("First build", content)))
      expect_true(any(grepl("Second build", content)))

      # Second entry should come before first (newest first)
      first_pos <- which(grepl("First build", content))[1]
      second_pos <- which(grepl("Second build", content))[1]
      expect_true(second_pos < first_pos)
    }
  )
})
