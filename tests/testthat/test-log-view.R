# Test projr_log_view function

test_that("projr_log_view displays most recent log by default", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create output log
      log_info1 <- .log_build_init("output", msg = "Output build")
      .log_build_append("Output message", "info")
      .log_build_finalize(TRUE, Sys.time() - 5)

      Sys.sleep(1)

      # Create dev log (more recent)
      log_info2 <- .log_build_init("dev", msg = "Dev build")
      .log_build_append("Dev message", "info")

      # Should return dev log (most recent)
      result <- projr_log_view(n_lines = NULL, show_header = FALSE)

      # Verify it shows content
      expect_true(is.character(result))
      expect_true(length(result) > 0)
      expect_true(any(grepl("Dev message", result)))
    }
  )
})

test_that("projr_log_view can filter by build_type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create output log
      log_info1 <- .log_build_init("output", msg = "Output build")
      .log_build_append("Output specific message", "info")
      .log_build_finalize(TRUE, Sys.time() - 5)

      # Create dev log (more recent)
      log_info2 <- .log_build_init("dev", msg = "Dev build")
      .log_build_append("Dev specific message", "info")

      # Get output log specifically
      result_output <- projr_log_view(build_type = "output", n_lines = NULL, show_header = FALSE)
      expect_true(any(grepl("Output specific message", result_output)))

      # Get dev log specifically
      result_dev <- projr_log_view(build_type = "dev", n_lines = NULL, show_header = FALSE)
      expect_true(any(grepl("Dev specific message", result_dev)))
    }
  )
})

test_that("projr_log_view handles n_lines parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log with multiple lines
      log_info <- .log_build_init("output", msg = "Test")
      for (i in 1:20) {
        .log_build_append(paste("Message", i), "info")
      }

      # Get last 5 lines
      result_5 <- projr_log_view(n_lines = 5, show_header = FALSE)
      expect_equal(length(result_5), 5)

      # Get last 10 lines
      result_10 <- projr_log_view(n_lines = 10, show_header = FALSE)
      expect_equal(length(result_10), 10)

      # Get all lines (n_lines = NULL)
      result_all <- projr_log_view(n_lines = NULL, show_header = FALSE)
      expect_true(length(result_all) > 10)
    }
  )
})

test_that("projr_log_view works with explicit log_file path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create multiple logs
      log_info1 <- .log_build_init("output", msg = "First")
      .log_build_append("First message", "info")

      Sys.sleep(1)

      log_info2 <- .log_build_init("dev", msg = "Second")
      .log_build_append("Second message", "info")

      # View specific log file
      result <- projr_log_view(log_file = log_info1$log_file, n_lines = 5, show_header = FALSE)
      expect_true(any(grepl("First message", result)))
      expect_false(any(grepl("Second message", result)))
    }
  )
})

test_that("projr_log_view handles missing log gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No logs created
      result <- projr_log_view()
      expect_null(result)
    }
  )
})

test_that("projr_log_view shows header by default", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log
      log_info <- .log_build_init("output", msg = "Test")
      .log_build_append("Test message", "info")

      # Capture output with header
      output <- capture.output(projr_log_view(n_lines = 2))

      # Should contain log file path and modified time references
      expect_true(any(grepl("Log file:", output, fixed = TRUE)))
    }
  )
})

test_that(".log_file_get_most_recent returns most recent across types", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create output log first
      log_info1 <- .log_build_init("output", msg = "Output")
      output_path <- log_info1$log_file

      Sys.sleep(1)

      # Create dev log (more recent)
      log_info2 <- .log_build_init("dev", msg = "Dev")
      dev_path <- log_info2$log_file

      # Should return dev path (more recent)
      most_recent <- .log_file_get_most_recent()
      expect_equal(most_recent, dev_path)

      # Modify output log to be more recent by appending to it directly
      Sys.sleep(1)
      cat("Updated\n", file = output_path, append = TRUE)

      # Should now return output path
      most_recent2 <- .log_file_get_most_recent()
      expect_equal(most_recent2, output_path)
    }
  )
})

test_that("CLI functions write to most recent log automatically", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log
      log_info <- .log_build_init("output", msg = "Test")
      log_file <- log_info$log_file

      # Call CLI functions without passing log_file
      .cli_info("Info message test", output_level = "std")
      .cli_debug("Debug message test", output_level = "std")
      .cli_success("Success message test", output_level = "std")

      # Read log file
      content <- readLines(log_file, warn = FALSE)

      # All messages should be in the log
      expect_true(any(grepl("Info message test", content)))
      expect_true(any(grepl("Debug message test", content)))
      expect_true(any(grepl("Success message test", content)))
    }
  )
})

test_that("Debug messages always logged regardless of output_level", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

      # Create log
      log_info <- .log_build_init("output", msg = "Test")
      log_file <- log_info$log_file

      # Call debug with output_level = "none" (should still log)
      .cli_debug("Debug with none level", output_level = "none")
      .cli_debug("Debug with std level", output_level = "std")

      # Read log file
      content <- readLines(log_file, warn = FALSE)

      # Both debug messages should be in log
      expect_true(any(grepl("Debug with none level", content)))
      expect_true(any(grepl("Debug with std level", content)))
    }
  )
})
