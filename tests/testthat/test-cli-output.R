# Tests for CLI output functionality
# ===================================

test_that(".cli_output_level_get works with explicit parameter", {
  skip_if(.is_test_select())
  
  # Test explicit levels
  expect_identical(.cli_output_level_get("none", FALSE), "none")
  expect_identical(.cli_output_level_get("std", FALSE), "std")
  expect_identical(.cli_output_level_get("debug", FALSE), "debug")
  
  # Test with output_run
  expect_identical(.cli_output_level_get("none", TRUE), "none")
  expect_identical(.cli_output_level_get("std", TRUE), "std")
})

test_that(".cli_output_level_get works with environment variable", {
  skip_if(.is_test_select())
  
  # Save original env var
  old_val <- Sys.getenv("PROJR_OUTPUT_LEVEL", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_OUTPUT_LEVEL = old_val) else Sys.unsetenv("PROJR_OUTPUT_LEVEL"))
  
  # Test env var setting
  Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
  expect_identical(.cli_output_level_get(NULL, FALSE), "debug")
  
  Sys.setenv(PROJR_OUTPUT_LEVEL = "std")
  expect_identical(.cli_output_level_get(NULL, TRUE), "std")
  
  # Unset and test defaults
  Sys.unsetenv("PROJR_OUTPUT_LEVEL")
  expect_identical(.cli_output_level_get(NULL, FALSE), "none")  # dev default
  expect_identical(.cli_output_level_get(NULL, TRUE), "std")    # output default
})

test_that(".cli_should_show works correctly", {
  skip_if(.is_test_select())
  
  # Test "none" level
  expect_false(.cli_should_show("std", "none"))
  expect_false(.cli_should_show("debug", "none"))
  
  # Test "std" level
  expect_true(.cli_should_show("std", "std"))
  expect_false(.cli_should_show("debug", "std"))
  
  # Test "debug" level
  expect_true(.cli_should_show("std", "debug"))
  expect_true(.cli_should_show("debug", "debug"))
})

test_that(".cli_info respects output level", {
  skip_if(.is_test_select())
  
  # At "none" level, should not produce output
  expect_silent(.cli_info("test message", output_level = "none"))
  
  # At "std" level, should produce output (we can't easily test the output itself)
  # Just verify it doesn't error
  expect_error(.cli_info("test message", output_level = "std"), NA)
  
  # At "debug" level, should also work
  expect_error(.cli_info("test message", output_level = "debug"), NA)
})

test_that(".cli_debug respects output level", {
  skip_if(.is_test_select())
  
  # At "none" and "std" levels, should not produce output
  expect_silent(.cli_debug("test debug message", output_level = "none"))
  expect_silent(.cli_debug("test debug message", output_level = "std"))
  
  # At "debug" level, should produce output
  expect_error(.cli_debug("test debug message", output_level = "debug"), NA)
})

test_that(".cli_stage_header respects output level", {
  skip_if(.is_test_select())
  
  # At "none" level, should not produce output
  expect_silent(.cli_stage_header("Test Stage", "output", "none"))
  
  # At "std" level, should produce output
  expect_error(.cli_stage_header("Test Stage", "output", "std"), NA)
  expect_error(.cli_stage_header("Test Stage", "dev", "std"), NA)
})

test_that("Build functions accept output_level parameter", {
  skip_if(.is_test_select())
  
  # Verify the parameter exists in the function signatures
  expect_true("output_level" %in% names(formals(projr_build)))
  expect_true("output_level" %in% names(formals(projr_build_dev)))
  expect_true("output_level" %in% names(formals(projr_build_major)))
  expect_true("output_level" %in% names(formals(projr_build_minor)))
  expect_true("output_level" %in% names(formals(projr_build_patch)))
  
  # Verify default value is NULL
  expect_null(formals(projr_build)$output_level)
  expect_null(formals(projr_build_dev)$output_level)
})

test_that("CLI output works in actual build (integration test)", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test dev build with output_level = "none" (should be quiet)
      # We can't easily capture the CLI output, but we can verify the build works
      expect_error(projr_build_dev(output_level = "none"), NA)
      
      # Test dev build with output_level = "std"
      expect_error(projr_build_dev(output_level = "std"), NA)
      
      # Verify the build actually produced output
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/docs") || dir.exists("_tmp/projr/v0.0.0-2/docs"))
    },
    quiet = TRUE,
    force = TRUE
  )
})
