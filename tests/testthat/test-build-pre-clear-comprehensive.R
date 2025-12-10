# Comprehensive tests for build-pre-clear.R
# Tests all combinations of clear_output parameter and directory types

# Helper to create test content in directories
.create_clear_test_content <- function(label_vec, safe = TRUE) {
  for (label in label_vec) {
    path_dir <- projr_path_get_dir(label, safe = safe)
    file.create(file.path(path_dir, "test1.txt"))
    file.create(file.path(path_dir, "test2.txt"))
    dir.create(file.path(path_dir, "subdir"), showWarnings = FALSE)
    file.create(file.path(path_dir, "subdir", "nested.txt"))
  }
  invisible(TRUE)
}

# Helper to check if directory is cleared
.check_dir_cleared <- function(label, safe = TRUE) {
  path_dir <- projr_path_get_dir(label, safe = safe, create = FALSE)
  if (!dir.exists(path_dir)) {
    return(TRUE) # Cleared (doesn't exist)
  }
  # Directory exists, check if it's empty
  files <- list.files(path_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  length(files) == 0
}

# =============================================================================
# Test .build_clear_pre_output with different clear_output values
# =============================================================================

test_that(".build_clear_pre_output clears nothing when clear_output='never'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in output directory
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # Call with clear_output = "never"
      result <- .build_clear_pre_output(clear_output = "never")

      # Should return FALSE and not clear anything
      expect_false(result)
      expect_false(.check_dir_cleared("output", safe = TRUE))
      expect_false(.check_dir_cleared("output", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_clear_pre_output clears only safe when clear_output='post'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in output directory
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # Call with clear_output = "post"
      result <- .build_clear_pre_output(clear_output = "post")

      # Should clear safe directory but not unsafe
      expect_true(result)
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_false(.check_dir_cleared("output", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_clear_pre_output clears both safe and unsafe when clear_output='pre'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in output directory
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # Call with clear_output = "pre"
      result <- .build_clear_pre_output(clear_output = "pre")

      # Should clear both safe and unsafe directories
      expect_true(result)
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_true(.check_dir_cleared("output", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test .build_clear_pre_output_label
# =============================================================================

test_that(".build_clear_pre_output_label clears safe directory for all clear_output values", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      for (clear_val in c("pre", "post", "never")) {
        # Create content
        .create_clear_test_content("output", safe = TRUE)
        .create_clear_test_content("output", safe = FALSE)

        # Clear
        .build_clear_pre_output_label("output", clear_output = clear_val)

        # Safe should always be cleared
        expect_true(.check_dir_cleared("output", safe = TRUE))

        # Unsafe only cleared for "pre"
        if (clear_val == "pre") {
          expect_true(.check_dir_cleared("output", safe = FALSE))
        } else {
          expect_false(.check_dir_cleared("output", safe = FALSE))
        }
      }
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test .build_clear_pre complete function
# =============================================================================

test_that(".build_clear_pre clears output dirs based on clear_output parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in output
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # Test with clear_output = "pre"
      .build_clear_pre(output_run = TRUE, clear_output = "pre")
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_true(.check_dir_cleared("output", safe = FALSE))

      # Recreate content
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # Test with clear_output = "post"
      .build_clear_pre(output_run = TRUE, clear_output = "post")
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_false(.check_dir_cleared("output", safe = FALSE))

      # Recreate content
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # Test with clear_output = "never"
      .build_clear_pre(output_run = TRUE, clear_output = "never")
      # With "never", nothing should be cleared by .build_clear_pre_output
      # but cache version directory might still be cleared
      expect_false(.check_dir_cleared("output", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test .dir_clear_pre_cache_version
# =============================================================================

test_that(".dir_clear_pre_cache_version clears cache version directory except 'old'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get cache version path (this creates it)
      cache_version_path <- .dir_get_cache_auto_version(profile = NULL)

      # Ensure cache_version_path exists
      if (!dir.exists(cache_version_path)) {
        dir.create(cache_version_path, recursive = TRUE)
      }

      # Create content in cache version directory
      dir.create(file.path(cache_version_path, "temp"), showWarnings = FALSE)
      file.create(file.path(cache_version_path, "temp", "file.txt"))

      # Create "old" directory with content (this should be preserved)
      old_dir <- file.path(cache_version_path, "old")
      dir.create(old_dir, showWarnings = FALSE)
      file.create(file.path(old_dir, "preserved.txt"))

      # Verify setup
      expect_true(dir.exists(file.path(cache_version_path, "temp")))
      expect_true(dir.exists(old_dir))
      expect_true(file.exists(file.path(old_dir, "preserved.txt")))

      # Clear - this should preserve "old" directory due to dir_exc = "old"
      .dir_clear_pre_cache_version()

      # "old" directory should be preserved
      expect_true(dir.exists(old_dir))
      expect_true(file.exists(file.path(old_dir, "preserved.txt")))

      # Other directories should be cleared
      expect_false(dir.exists(file.path(cache_version_path, "temp")))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test with multiple output labels
# =============================================================================

test_that(".build_clear_pre clears all output labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add a second output directory
      .yml_dir_add_label(path = "_output2", label = "output2", profile = "default")

      # Create content in both output directories
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)
      .create_clear_test_content("output2", safe = TRUE)
      .create_clear_test_content("output2", safe = FALSE)

      # Clear with "pre"
      .build_clear_pre(output_run = TRUE, clear_output = "pre")

      # Both should be cleared
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_true(.check_dir_cleared("output", safe = FALSE))
      expect_true(.check_dir_cleared("output2", safe = TRUE))
      expect_true(.check_dir_cleared("output2", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test edge cases
# =============================================================================

test_that(".build_clear_pre works with non-existent directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Don't create any output directories
      # Should not error
      expect_silent(.build_clear_pre(output_run = TRUE, clear_output = "pre"))
      expect_silent(.build_clear_pre(output_run = TRUE, clear_output = "post"))
      expect_silent(.build_clear_pre(output_run = TRUE, clear_output = "never"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_clear_pre preserves nested directory structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create deeply nested content
      output_path <- projr_path_get_dir("output", safe = FALSE)
      deep_path <- file.path(output_path, "level1", "level2", "level3")
      dir.create(deep_path, recursive = TRUE)
      file.create(file.path(deep_path, "deep_file.txt"))

      # Clear
      .build_clear_pre(output_run = TRUE, clear_output = "pre")

      # Directory should be cleared (all files removed)
      expect_true(.check_dir_cleared("output", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test interaction with PROJR_CLEAR_OUTPUT environment variable
# =============================================================================

test_that(".build_clear_pre respects clear_output parameter over env var", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_CLEAR_OUTPUT", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_CLEAR_OUTPUT = old_val) else Sys.unsetenv("PROJR_CLEAR_OUTPUT"))

  # Set env var to "never"
  Sys.setenv(PROJR_CLEAR_OUTPUT = "never")

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)

      # But pass "pre" directly - should clear
      .build_clear_pre(output_run = TRUE, clear_output = "pre")

      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_true(.check_dir_cleared("output", safe = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test that docs are NOT cleared by .build_clear_pre
# =============================================================================

test_that(".build_clear_pre does not clear docs directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in docs (unsafe location)
      docs_path <- projr_path_get_dir("docs", safe = FALSE)
      file.create(file.path(docs_path, "index.html"))
      file.create(file.path(docs_path, "styles.css"))

      # Clear
      .build_clear_pre(output_run = TRUE, clear_output = "pre")

      # Docs should NOT be cleared
      expect_true(file.exists(file.path(docs_path, "index.html")))
      expect_true(file.exists(file.path(docs_path, "styles.css")))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Test .build_clear_pre_docs functions (even though not called by .build_clear_pre)
# =============================================================================

test_that(".build_clear_pre_docs_check returns correct value based on docs path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # With normal docs path, should return FALSE (not identical to project root)
      expect_false(.build_clear_pre_docs_check(clear_output = "pre"))
      expect_false(.build_clear_pre_docs_check(clear_output = "post"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_clear_pre_docs clears appropriate docs directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in both safe and unsafe docs
      docs_safe <- projr_path_get_dir("docs", safe = TRUE)
      docs_unsafe <- projr_path_get_dir("docs", safe = FALSE)

      file.create(file.path(docs_safe, "safe_doc.html"))
      file.create(file.path(docs_unsafe, "unsafe_doc.html"))

      # This function checks if docs == project root, which is normally FALSE
      # So it should return FALSE and not clear anything
      result <- .build_clear_pre_docs(clear_output = "pre")

      expect_false(result)
      expect_true(file.exists(file.path(docs_safe, "safe_doc.html")))
      expect_true(file.exists(file.path(docs_unsafe, "unsafe_doc.html")))
    },
    quiet = TRUE,
    force = TRUE
  )
})
