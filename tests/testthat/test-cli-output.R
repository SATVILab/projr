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

test_that("PROJR_OUTPUT_LEVEL validates input correctly", {
  skip_if(.is_test_select())
  
  old_val <- Sys.getenv("PROJR_OUTPUT_LEVEL", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_OUTPUT_LEVEL = old_val) else Sys.unsetenv("PROJR_OUTPUT_LEVEL"))
  
  # Valid values should work
  Sys.setenv(PROJR_OUTPUT_LEVEL = "none")
  expect_identical(.cli_output_level_get(NULL, FALSE), "none")
  
  Sys.setenv(PROJR_OUTPUT_LEVEL = "std")
  expect_identical(.cli_output_level_get(NULL, FALSE), "std")
  
  Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
  expect_identical(.cli_output_level_get(NULL, FALSE), "debug")
  
  # Invalid value should error
  Sys.setenv(PROJR_OUTPUT_LEVEL = "invalid")
  expect_error(.cli_output_level_get(NULL, FALSE))
  
  # Case sensitivity check
  Sys.setenv(PROJR_OUTPUT_LEVEL = "DEBUG")
  expect_error(.cli_output_level_get(NULL, FALSE))
})

test_that("PROJR_OUTPUT_LEVEL explicit parameter overrides env var", {
  skip_if(.is_test_select())
  
  old_val <- Sys.getenv("PROJR_OUTPUT_LEVEL", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_OUTPUT_LEVEL = old_val) else Sys.unsetenv("PROJR_OUTPUT_LEVEL"))
  
  # Set env var to one value
  Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
  
  # Explicit parameter should override
  expect_identical(.cli_output_level_get("none", FALSE), "none")
  expect_identical(.cli_output_level_get("std", TRUE), "std")
})

test_that("PROJR_OUTPUT_LEVEL defaults work correctly", {
  skip_if(.is_test_select())
  
  old_val <- Sys.getenv("PROJR_OUTPUT_LEVEL", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_OUTPUT_LEVEL = old_val) else Sys.unsetenv("PROJR_OUTPUT_LEVEL"))
  
  # Unset env var should use defaults
  Sys.unsetenv("PROJR_OUTPUT_LEVEL")
  
  # Dev build default is "none"
  expect_identical(.cli_output_level_get(NULL, FALSE), "none")
  
  # Output build default is "std"
  expect_identical(.cli_output_level_get(NULL, TRUE), "std")
})

test_that("CLI functions handle NULL log_file parameter", {
  skip_if(.is_test_select())
  
  # All CLI functions should handle NULL log_file gracefully
  expect_silent(.cli_info("test", output_level = "none", log_file = NULL))
  expect_silent(.cli_success("test", output_level = "none", log_file = NULL))
  expect_silent(.cli_debug("test", output_level = "none", log_file = NULL))
  expect_silent(.cli_step("test", output_level = "none", log_file = NULL))
  expect_silent(.cli_stage_header("test", "dev", "none", log_file = NULL))
})

test_that("CLI debug messages only show at debug level", {
  skip_if(.is_test_select())
  
  # At "none" level, debug should be silent
  expect_silent(.cli_debug("debug msg", output_level = "none"))
  
  # At "std" level, debug should be silent
  expect_silent(.cli_debug("debug msg", output_level = "std"))
  
  # At "debug" level, debug should produce output
  expect_error(.cli_debug("debug msg", output_level = "debug"), NA)
})

test_that("CLI message hierarchy works correctly", {
  skip_if(.is_test_select())
  
  # none level - nothing shows
  expect_silent(.cli_info("info", output_level = "none"))
  expect_silent(.cli_success("success", output_level = "none"))
  expect_silent(.cli_debug("debug", output_level = "none"))
  
  # std level - info and success show, debug doesn't
  expect_error(.cli_info("info", output_level = "std"), NA)
  expect_error(.cli_success("success", output_level = "std"), NA)
  expect_silent(.cli_debug("debug", output_level = "std"))
  
  # debug level - all show
  expect_error(.cli_info("info", output_level = "debug"), NA)
  expect_error(.cli_success("success", output_level = "debug"), NA)
  expect_error(.cli_debug("debug", output_level = "debug"), NA)
})
