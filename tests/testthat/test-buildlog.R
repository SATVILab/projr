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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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

test_that(".build_buildlog_check returns correct output_run status", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with dev build
  expect_false(.build_buildlog_check("dev"))
  expect_false(.build_buildlog_check(NULL))

  # Test with output builds
  expect_true(.build_buildlog_check("patch"))
  expect_true(.build_buildlog_check("minor"))
  expect_true(.build_buildlog_check("major"))
})

test_that(".buildlog_get_header creates correct header format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  version_list <- list(desc = list(success = "1.2.3"))

  # Test patch
  header <- .buildlog_get_header(version_list, "patch")
  expect_true(is.character(header))
  expect_true(any(grepl("#### v1.2.3:", header)))

  # Test minor
  header <- .buildlog_get_header(version_list, "minor")
  expect_true(any(grepl("### v1.2.3:", header)))

  # Test major
  header <- .buildlog_get_header(version_list, "major")
  expect_true(any(grepl("## v1.2.3:", header)))

  # All should have timestamp
  expect_true(any(grepl("\\d{4}-\\d{2}-\\d{2}", header)))
})

test_that(".buildlog_get_desc formats description correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  msg <- "This is a test message"
  desc <- .buildlog_get_desc(msg)

  expect_true(is.character(desc))
  expect_true(any(grepl("\\*\\*Description\\*\\*", desc)))
  expect_true(any(grepl("This is a test message", desc)))

  # Should have proper structure with empty lines
  expect_true(length(desc) >= 3)
})

test_that(".buildlog_get_metadata includes time and profile", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      total_time <- as.difftime(125, units = "secs")
      metadata <- .buildlog_get_metadata(total_time)

      expect_true(is.character(metadata))
      expect_true(any(grepl("\\*\\*Metadata\\*\\*", metadata)))
      expect_true(any(grepl("Total time:", metadata)))
      expect_true(any(grepl("2min 5s", metadata)))
      expect_true(any(grepl("projr.*profile:", metadata)))
    }
  )
})

test_that(".buildlog_get_metadata_time handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Just seconds
  expect_match(.buildlog_get_metadata_time(as.difftime(5, units = "secs")), "^5s$")

  # Exactly 1 minute
  expect_match(.buildlog_get_metadata_time(as.difftime(60, units = "secs")), "^1min 0s$")

  # Exactly 1 hour
  expect_match(.buildlog_get_metadata_time(as.difftime(3600, units = "secs")), "^1hr 0min 0s$")

  # Exactly 1 day
  expect_match(.buildlog_get_metadata_time(as.difftime(86400, units = "secs")), "^1d 0hr 0min 0s$")

  # Test weeks (1 week + 2 days + 3 hours + 4 minutes + 5 seconds)
  week_seconds <- 7 * 24 * 3600 + 2 * 24 * 3600 + 3 * 3600 + 4 * 60 + 5
  result <- .buildlog_get_metadata_time(as.difftime(week_seconds, units = "secs"))
  expect_match(result, "1w 2d 3hr 4min 5s")

  # Multiple weeks
  multi_week <- 3 * 7 * 24 * 3600 + 1 * 24 * 3600
  result <- .buildlog_get_metadata_time(as.difftime(multi_week, units = "secs"))
  expect_match(result, "3w 1d 0hr 0min 0s")

  # Large duration with all components
  large <- 2 * 7 * 24 * 3600 + 3 * 24 * 3600 + 5 * 3600 + 30 * 60 + 45
  result <- .buildlog_get_metadata_time(as.difftime(large, units = "secs"))
  expect_match(result, "2w 3d 5hr 30min 45s")
})

test_that(".buildlog_get_change_summary returns NULL for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Dev builds should not have change summaries
  result <- .buildlog_get_change_summary("dev")
  expect_identical(result, character(0))

  result <- .buildlog_get_change_summary(NULL)
  expect_identical(result, character(0))
})

test_that(".buildlog_get_add combines all sections correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      msg <- "Complete test build"
      bump_component <- "minor"
      version_run_on_list <- list(desc = list(success = "2.0.0"))
      total_time <- as.difftime(150, units = "secs")

      add_txt <- .buildlog_get_add(msg, bump_component, version_run_on_list, total_time)

      # Check all expected sections are present
      expect_true(any(grepl("### v2.0.0:", add_txt))) # Header (minor = 3 hashes)
      expect_true(any(grepl("\\*\\*Description\\*\\*", add_txt))) # Description
      expect_true(any(grepl("Complete test build", add_txt))) # Message
      expect_true(any(grepl("\\*\\*Metadata\\*\\*", add_txt))) # Metadata
      expect_true(any(grepl("Total time:", add_txt))) # Time
      expect_true(any(grepl("projr.*profile:", add_txt))) # Profile
      expect_true(any(grepl("\\*\\*System Resources\\*\\*", add_txt))) # System
      expect_true(any(grepl("\\*\\*`projr` config\\*\\*", add_txt))) # Config
      expect_true(any(grepl("\\*\\*Session info\\*\\*", add_txt))) # Session
      expect_true(any(grepl("----", add_txt))) # Separator

      # Verify structure
      expect_true(is.character(add_txt))
      expect_true(length(add_txt) > 20) # Should be substantial
    }
  )
})

test_that(".build_buildlog_add works correctly for output builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with output build - function doesn't return a value we can capture
      .build_buildlog_add(
        msg = "Test build entry",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.1.0")),
        total_time = as.difftime(90, units = "secs")
      )

      # Verify file was created
      path_buildlog <- .path_get("BUILDLOG.md")
      expect_true(file.exists(path_buildlog))

      # Verify content
      content <- .buildlog_read()
      expect_true(any(grepl("Test build entry", content)))
      expect_true(any(grepl("v0.1.0", content)))
    }
  )
})

test_that(".build_buildlog_add returns FALSE for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with dev build
      result <- .build_buildlog_add(
        msg = "Dev build",
        bump_component = "dev",
        version_run_on_list = list(desc = list(success = "0.1.0-1")),
        total_time = as.difftime(30, units = "secs")
      )

      # Should return FALSE
      expect_false(result)

      # Verify no buildlog file was created
      path_buildlog <- .path_get("BUILDLOG.md")
      if (file.exists(path_buildlog)) {
        content <- .buildlog_read()
        # Should not contain dev build message
        expect_false(any(grepl("Dev build", content)))
      }
    }
  )
})
