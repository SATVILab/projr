# Test detailed projr logging system (cache/projr/log/)

test_that(".log_dir_get_base returns correct path", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      log_base <- .log_dir_get_base()

      # Should return a valid path
      expect_true(is.character(log_base))
      expect_equal(length(log_base), 1)
      expect_true(nzchar(log_base))

      # Path should end with 'log'
      expect_true(basename(log_base) == "log")
    }
  )
})

test_that(".log_dir_get_type creates directories correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test output type
      output_dir <- .log_dir_get_type("output", create = TRUE)
      expect_true(dir.exists(output_dir))
      expect_true(grepl("output$", output_dir))

      # Test dev type
      dev_dir <- .log_dir_get_type("dev", create = TRUE)
      expect_true(dir.exists(dev_dir))
      expect_true(grepl("dev$", dev_dir))
    }
  )
})

test_that(".log_dir_get_type without create doesn't create directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove log directory if it exists
      log_base <- .log_dir_get_base()
      if (dir.exists(log_base)) {
        unlink(log_base, recursive = TRUE)
      }

      # Getting path without create should not create directory
      output_dir <- .log_dir_get_type("output", create = FALSE)
      expect_false(dir.exists(output_dir))
    }
  )
})

test_that(".log_file_get_history returns correct path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Get history file paths
      output_history <- .log_file_get_history("output")
      dev_history <- .log_file_get_history("dev")

      # Should end with history/builds.md
      expect_true(grepl("history.*builds\\.md$", output_history))
      expect_true(grepl("history.*builds\\.md$", dev_history))

      # Should be different paths
      expect_false(output_history == dev_history)

      # Directories should be created
      expect_true(dir.exists(dirname(output_history)))
      expect_true(dir.exists(dirname(dev_history)))
    }
  )
})

test_that(".log_dir_get_output_date creates date directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with default date (today)
      output_dir <- .log_dir_get_output_date("output")
      expect_true(dir.exists(output_dir))

      # Should contain today's date in format YYYY-MMM-DD
      today <- format(Sys.Date(), "%Y-%b-%d")
      expect_true(grepl(today, output_dir))

      # Test with specific date
      specific_date <- "2024-Jan-15"
      specific_dir <- .log_dir_get_output_date("output", date = specific_date)
      expect_true(dir.exists(specific_dir))
      expect_true(grepl(specific_date, specific_dir))
    }
  )
})

test_that(".log_file_get_output returns correct file path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with default timestamp
      log_file <- .log_file_get_output("output")

      # Should end with .qmd
      expect_true(grepl("\\.qmd$", log_file))

      # Should contain time format HH-MM-SS
      expect_true(grepl("\\d{2}-\\d{2}-\\d{2}\\.qmd$", log_file))

      # Test with specific timestamp
      timestamp <- "12-30-45"
      specific_file <- .log_file_get_output("dev", timestamp = timestamp)
      expect_true(grepl(timestamp, specific_file))
    }
  )
})

test_that(".log_enabled checks environment variable correctly", {
  skip_if(.is_test_select())

  # Save original value
  original <- Sys.getenv("PROJR_LOG_DETAILED", unset = NA)

  # Test TRUE values
  Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
  expect_true(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "true")
  expect_true(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "1")
  expect_true(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "YES")
  expect_true(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "Y")
  expect_true(.log_enabled())

  # Test FALSE values
  Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
  expect_false(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "false")
  expect_false(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "0")
  expect_false(.log_enabled())

  Sys.setenv(PROJR_LOG_DETAILED = "NO")
  expect_false(.log_enabled())

  # Restore original
  if (is.na(original)) {
    Sys.unsetenv("PROJR_LOG_DETAILED")
  } else {
    Sys.setenv(PROJR_LOG_DETAILED = original)
  }
})

test_that(".log_build_init creates log file correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure logging is enabled
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Initialize log
      log_info <- .log_build_init(
        build_type = "output",
        bump_component = "patch",
        msg = "Test build",
        output_level = "std"
      )

      # Should return list with log_file and timestamp
      expect_true(is.list(log_info))
      expect_true("log_file" %in% names(log_info))
      expect_true("timestamp" %in% names(log_info))

      # Log file should exist
      expect_true(file.exists(log_info$log_file))

      # Read content
      content <- readLines(log_info$log_file, warn = FALSE)

      # Should have YAML frontmatter
      expect_true(content[1] == "---")
      expect_true(any(grepl("title:", content)))
      expect_true(any(grepl("date:", content)))
      expect_true(any(grepl("format:", content)))

      # Should have build information
      expect_true(any(grepl("# Build Information", content)))
      expect_true(any(grepl("Build Type", content)))
      expect_true(any(grepl("Version Bump", content)))
      expect_true(any(grepl("Test build", content)))
    }
  )
})

test_that(".log_build_init returns NULL when logging disabled", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Disable logging
      Sys.setenv(PROJR_LOG_DETAILED = "FALSE")

      # Initialize log should return NULL
      log_info <- .log_build_init(
        build_type = "output",
        bump_component = "patch",
        msg = "Test build"
      )

      expect_null(log_info)

      # Re-enable for other tests
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
    }
  )
})

test_that(".log_build_append adds messages to log", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log file
      log_info <- .log_build_init("dev", msg = "Test")
      log_file <- log_info$log_file

      # Append messages
      .log_build_append("Test message 1", "info")
      .log_build_append("Test message 2", "debug")
      .log_build_append("Test message 3", "success")

      # Read content
      content <- readLines(log_file, warn = FALSE)

      # Should contain all messages
      expect_true(any(grepl("Test message 1", content)))
      expect_true(any(grepl("Test message 2", content)))
      expect_true(any(grepl("Test message 3", content)))

      # Should have level indicators
      expect_true(any(grepl("INFO", content)))
      expect_true(any(grepl("DEBUG", content)))
      expect_true(any(grepl("SUCCESS", content)))
    }
  )
})

test_that(".log_build_section adds section headers", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log file
      log_info <- .log_build_init("dev", msg = "Test")
      log_file <- log_info$log_file

      # Add section
      .log_build_section("Test Section")

      # Read content
      content <- readLines(log_file, warn = FALSE)

      # Should contain section header
      expect_true(any(grepl("## Test Section", content)))
    }
  )
})

test_that(".log_build_finalize adds summary", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log file
      log_info <- .log_build_init("dev", msg = "Test")
      log_file <- log_info$log_file

      # Finalize with success
      start_time <- Sys.time() - 10
      .log_build_finalize(success = TRUE, start_time = start_time)

      # Read content
      content <- readLines(log_file, warn = FALSE)

      # Should contain summary section
      expect_true(any(grepl("# Build Summary", content)))
      expect_true(any(grepl("Status.*SUCCESS", content)))
      expect_true(any(grepl("Duration", content)))
      expect_true(any(grepl("Completed", content)))
    }
  )
})

test_that(".log_build_finalize marks failures", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log file
      log_info <- .log_build_init("dev", msg = "Test")
      log_file <- log_info$log_file

      # Finalize with failure
      .log_build_finalize(success = FALSE)

      # Read content
      content <- readLines(log_file, warn = FALSE)

      # Should mark as failed
      expect_true(any(grepl("Status.*FAILED", content)))
    }
  )
})

test_that(".log_history_add creates history file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add to history
      .log_history_add(
        build_type = "output",
        bump_component = "patch",
        msg = "Test build",
        success = TRUE
      )

      # History file should exist
      history_file <- .log_file_get_history("output")
      expect_true(file.exists(history_file))

      # Read content
      content <- readLines(history_file, warn = FALSE)

      # Should have header
      expect_true(any(grepl("# Build History", content)))

      # Should have entry
      expect_true(any(grepl("Test build", content)))
      expect_true(any(grepl("output", content)))
      expect_true(any(grepl("patch", content)))
      expect_true(any(grepl("\\[OK\\]", content)))
    }
  )
})

test_that(".log_history_add handles multiple entries", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add first entry
      .log_history_add(
        build_type = "dev",
        bump_component = "patch",
        msg = "First build",
        success = TRUE
      )

      # Add second entry
      .log_history_add(
        build_type = "dev",
        bump_component = "minor",
        msg = "Second build",
        success = FALSE
      )

      # Read content
      history_file <- .log_file_get_history("dev")
      content <- readLines(history_file, warn = FALSE)

      # Both entries should be present
      expect_true(any(grepl("First build", content)))
      expect_true(any(grepl("Second build", content)))

      # Should have success and failure markers
      expect_true(any(grepl("\\[OK\\]", content)))
      expect_true(any(grepl("\\[X\\]", content)))

      # Second entry should come before first (newest first)
      first_pos <- which(grepl("First build", content))[1]
      second_pos <- which(grepl("Second build", content))[1]
      expect_true(second_pos < first_pos)
    }
  )
})

test_that("projr_log_clear clears all logs", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create some logs
      log_info1 <- .log_build_init("output", msg = "Test 1")
      .log_history_add("output", msg = "Test 1", success = TRUE)

      log_info2 <- .log_build_init("dev", msg = "Test 2")
      .log_history_add("dev", msg = "Test 2", success = TRUE)

      # Verify files exist
      expect_true(file.exists(log_info1$log_file))
      expect_true(file.exists(log_info2$log_file))
      expect_true(file.exists(.log_file_get_history("output")))
      expect_true(file.exists(.log_file_get_history("dev")))

      # Clear all logs
      projr_log_clear(build_type = "all", history = TRUE, output = TRUE)

      # History files should be deleted
      expect_false(file.exists(.log_file_get_history("output")))
      expect_false(file.exists(.log_file_get_history("dev")))

      # Output directories should be deleted
      expect_false(file.exists(log_info1$log_file))
      expect_false(file.exists(log_info2$log_file))
    }
  )
})

test_that("projr_log_clear clears only output logs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create logs
      log_info1 <- .log_build_init("output", msg = "Test 1")
      .log_history_add("output", msg = "Test 1", success = TRUE)

      log_info2 <- .log_build_init("dev", msg = "Test 2")
      .log_history_add("dev", msg = "Test 2", success = TRUE)

      # Clear only output
      projr_log_clear(build_type = "output", history = TRUE, output = TRUE)

      # Output logs should be deleted
      expect_false(file.exists(.log_file_get_history("output")))
      expect_false(file.exists(log_info1$log_file))

      # Dev logs should remain
      expect_true(file.exists(.log_file_get_history("dev")))
      expect_true(file.exists(log_info2$log_file))
    }
  )
})

test_that("projr_log_clear can preserve history", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create logs
      log_info <- .log_build_init("output", msg = "Test")
      .log_history_add("output", msg = "Test", success = TRUE)

      # Clear only output files, keep history
      projr_log_clear(build_type = "output", history = FALSE, output = TRUE)

      # Output file should be deleted
      expect_false(file.exists(log_info$log_file))

      # History should remain
      expect_true(file.exists(.log_file_get_history("output")))
    }
  )
})

test_that("projr_log_clear with before_date filters correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create old log
      old_date <- "2024-Jan-01"
      old_dir <- .log_dir_get_output_date("output", date = old_date)
      old_file <- file.path(old_dir, "10-00-00.qmd")
      writeLines("test", old_file)

      # Create new log
      new_date <- format(Sys.Date(), "%Y-%b-%d")
      new_dir <- .log_dir_get_output_date("output", date = new_date)
      new_file <- file.path(new_dir, "11-00-00.qmd")
      writeLines("test", new_file)

      # Clear logs before 2024-06-01
      projr_log_clear(
        build_type = "output",
        history = FALSE,
        output = TRUE,
        before_date = "2024-06-01"
      )

      # Old log should be deleted
      expect_false(file.exists(old_file))

      # New log should remain
      expect_true(file.exists(new_file))
    }
  )
})

test_that("log system integrates with build process", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Simulate a build workflow
      log_info <- .log_build_init(
        build_type = "output",
        bump_component = "patch",
        msg = "Integration test build",
        output_level = "std"
      )

      log_file <- log_info$log_file

      # Add sections and messages
      .log_build_section("Initialization")
      .log_build_append("Starting build", "info")

      .log_build_section("Compilation")
      .log_build_append("Compiling files", "info")
      .log_build_append("Compilation successful", "success")

      .log_build_section("Testing")
      .log_build_append("Running tests", "info")
      .log_build_append("All tests passed", "success")

      # Finalize
      start_time <- Sys.time() - 30
      .log_build_finalize(success = TRUE, start_time = start_time)

      # Add to history
      .log_history_add(
        build_type = "output",
        bump_component = "patch",
        msg = "Integration test build",
        success = TRUE
      )

      # Verify detailed log
      content <- readLines(log_file, warn = FALSE)
      expect_true(any(grepl("Initialization", content)))
      expect_true(any(grepl("Compilation", content)))
      expect_true(any(grepl("Testing", content)))
      expect_true(any(grepl("Starting build", content)))
      expect_true(any(grepl("All tests passed", content)))
      expect_true(any(grepl("SUCCESS", content)))

      # Verify history
      history_file <- .log_file_get_history("output")
      history_content <- readLines(history_file, warn = FALSE)
      expect_true(any(grepl("Integration test build", history_content)))
      expect_true(any(grepl("\\[OK\\]", history_content)))
    }
  )
})

test_that("PROJR_LOG_DETAILED handles case variations", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  original <- Sys.getenv("PROJR_LOG_DETAILED", unset = NA)

  # Test various TRUE representations
  for (val in c("TRUE", "true", "True", "1", "YES", "yes", "Y", "y")) {
    Sys.setenv(PROJR_LOG_DETAILED = val)
    expect_true(.log_enabled(), info = paste("Failed for value:", val))
  }

  # Test various FALSE representations
  for (val in c("FALSE", "false", "False", "0", "NO", "no", "N", "n")) {
    Sys.setenv(PROJR_LOG_DETAILED = val)
    expect_false(.log_enabled(), info = paste("Failed for value:", val))
  }

  # Test invalid values (should default to TRUE)
  Sys.setenv(PROJR_LOG_DETAILED = "maybe")
  expect_false(.log_enabled())

  # Restore original
  if (is.na(original)) {
    Sys.unsetenv("PROJR_LOG_DETAILED")
  } else {
    Sys.setenv(PROJR_LOG_DETAILED = original)
  }
})

test_that("PROJR_LOG_DETAILED default is TRUE when unset", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  original <- Sys.getenv("PROJR_LOG_DETAILED", unset = NA)

  # Unset the variable
  Sys.unsetenv("PROJR_LOG_DETAILED")

  # Should default to TRUE
  expect_true(.log_enabled())

  # Restore original
  if (!is.na(original)) {
    Sys.setenv(PROJR_LOG_DETAILED = original)
  }
})

test_that("PROJR_LOG_DETAILED controls log file creation", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # With logging enabled
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
      log_info <- .log_build_init("output", msg = "Test")
      expect_true(is.list(log_info))
      expect_true(file.exists(log_info$log_file))

      # With logging disabled
      Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
      log_info2 <- .log_build_init("dev", msg = "Test2")
      expect_null(log_info2)

      # Re-enable for other tests
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
    }
  )
})

test_that("PROJR_LOG_DETAILED doesn't affect history tracking", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Disable detailed logging
      Sys.setenv(PROJR_LOG_DETAILED = "FALSE")

      # History should still be updated
      .log_history_add(
        build_type = "output",
        msg = "Test with logging disabled",
        success = TRUE
      )

      history_file <- .log_file_get_history("output")
      expect_true(file.exists(history_file))

      content <- readLines(history_file, warn = FALSE)
      expect_true(any(grepl("Test with logging disabled", content)))

      # Re-enable for other tests
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
    }
  )
})

test_that("Log file paths use correct separators", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      log_base <- .log_dir_get_base()
      log_output <- .log_dir_get_type("output")
      log_dev <- .log_dir_get_type("dev")

      # All paths should be valid
      expect_true(file.exists(log_base) || dir.exists(log_base) || !is.na(log_base))
      expect_true(dir.exists(log_output))
      expect_true(dir.exists(log_dev))

      # Paths should use correct separator
      expect_true(grepl(.Platform$file.sep, log_output, fixed = TRUE))
      expect_true(grepl(.Platform$file.sep, log_dev, fixed = TRUE))
    }
  )
})
