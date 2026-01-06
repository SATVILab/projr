# Test edge cases for build copy operations
# These tests verify that .build_copy_to_unsafe handles various edge cases correctly

test_that(".build_copy_to_unsafe handles empty safe directories gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create empty safe directory
      safe_dir <- projr_path_get_dir("output", safe = TRUE)
      unsafe_dir_path <- projr_path_get_dir("output", safe = FALSE, create = FALSE)

      # Remove any existing files
      if (dir.exists(safe_dir)) {
        unlink(file.path(safe_dir, "*"), recursive = TRUE)
      }
      if (dir.exists(unsafe_dir_path)) {
        unlink(unsafe_dir_path, recursive = TRUE)
      }

      # Ensure safe directory exists but is empty
      dir.create(safe_dir, showWarnings = FALSE, recursive = TRUE)
      expect_true(dir.exists(safe_dir))
      expect_equal(length(list.files(safe_dir)), 0)

      # Call .build_copy_to_unsafe - should not fail, should skip empty directory
      result <- .build_copy_to_unsafe(output_run = TRUE)

      # Should return TRUE (success)
      expect_true(result)

      # Unsafe directory should not be created for empty safe directory
      # (or if created, should be empty)
      if (dir.exists(unsafe_dir_path)) {
        expect_equal(length(list.files(unsafe_dir_path)), 0)
      }
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_to_unsafe handles non-existent safe directories gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove safe directory if it exists
      safe_dir <- projr_path_get_dir("output", safe = TRUE, create = FALSE)
      if (dir.exists(safe_dir)) {
        unlink(safe_dir, recursive = TRUE)
      }

      # Verify safe directory doesn't exist
      expect_false(dir.exists(safe_dir))

      # Call .build_copy_to_unsafe - should not fail
      result <- .build_copy_to_unsafe(output_run = TRUE)

      # Should return TRUE (success) even though nothing was copied
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_to_unsafe successfully copies when files exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create files in safe directory
      file.create(projr_path_get("output", "test1.txt", safe = TRUE))
      file.create(projr_path_get("output", "test2.txt", safe = TRUE))
      writeLines("content", projr_path_get("output", "test1.txt", safe = TRUE))
      writeLines("more content", projr_path_get("output", "test2.txt", safe = TRUE))

      safe_dir <- projr_path_get_dir("output", safe = TRUE, create = FALSE)
      unsafe_dir <- projr_path_get_dir("output", safe = FALSE, create = FALSE)

      # Remove unsafe directory to ensure clean state
      if (dir.exists(unsafe_dir)) {
        unlink(unsafe_dir, recursive = TRUE)
      }

      # Verify files exist in safe directory
      expect_true(file.exists(file.path(safe_dir, "test1.txt")))
      expect_true(file.exists(file.path(safe_dir, "test2.txt")))

      # Call .build_copy_to_unsafe
      # browser()
      # debugonce(.dir_move_no_exc)
      result <- suppressMessages(.build_copy_to_unsafe(output_run = TRUE))

      # Should return TRUE
      expect_true(result)

      # Files should now be in unsafe directory

      expect_true(dir.exists(unsafe_dir))
      expect_true(file.exists(file.path(unsafe_dir, "test1.txt")))
      expect_true(file.exists(file.path(unsafe_dir, "test2.txt")))

      # Verify content was preserved
      expect_equal(readLines(file.path(unsafe_dir, "test1.txt")), "content")
      expect_equal(readLines(file.path(unsafe_dir, "test2.txt")), "more content")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_to_unsafe shows informative messages when copying", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a file in safe directory
      file.create(projr_path_get("output", "test.txt", safe = TRUE))

      # Capture messages
      messages <- capture.output(
        {
          result <- .build_copy_to_unsafe(output_run = TRUE)
        },
        type = "message"
      )

      # Should have informative message about copying
      expect_true(any(grepl("Copied.*output", messages)))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_to_unsafe returns FALSE for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create files in safe directory
      file.create(projr_path_get("output", "test.txt", safe = TRUE))

      # For dev builds (output_run = FALSE), should return FALSE without copying
      result <- .build_copy_to_unsafe(output_run = FALSE)

      # Should return FALSE for dev builds
      expect_false(result)

      # Files should remain in safe directory
      safe_dir <- projr_path_get_dir("output", safe = TRUE, create = FALSE)
      expect_true(file.exists(file.path(safe_dir, "test.txt")))
    },
    quiet = TRUE,
    force = TRUE
  )
})
