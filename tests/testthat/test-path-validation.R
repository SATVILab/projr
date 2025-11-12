# Tests for path function validation and edge cases

# ==============================================================================
# Path transformation functions
# ==============================================================================

test_that(".path_force_rel handles empty vectors", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty character vector should return empty character vector
      expect_identical(.path_force_rel(character(0)), character(0))
      
      # Valid path should work
      expect_type(.path_force_rel("a/b"), "character")
      expect_length(.path_force_rel("a/b"), 1)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_force_rel validates input types", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Invalid input should error
      expect_error(.path_force_rel(NULL))
      expect_error(.path_force_rel(NA))
      expect_error(.path_force_rel(123))
      expect_error(.path_force_rel(list("a")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_force_abs validates input types", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid input
      expect_type(.path_force_abs("a/b"), "character")
      
      # Invalid input should error
      expect_error(.path_force_abs(NULL))
      expect_error(.path_force_abs(NA))
      expect_error(.path_force_abs(123))
      expect_error(.path_force_abs(list("a")))
      expect_error(.path_force_abs(character(0)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_force_abs handles path_dir correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Should error when fn is absolute and path_dir is not NULL
      abs_path <- fs::path_abs("a")
      expect_error(.path_force_abs(abs_path, "some/dir"))
      
      # Should work when path_dir is NULL with absolute path
      expect_identical(.path_force_abs(abs_path, NULL), abs_path)
      
      # Should work with relative path and path_dir
      result <- .path_force_abs("a/b", dir_test)
      expect_true(fs::is_absolute_path(result))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_get_full validates inputs", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid inputs
      expect_type(.path_get_full(dir_test), "character")
      expect_type(.path_get_full(dir_test, "a", "b"), "character")
      
      # Invalid path_dir should error
      expect_error(.path_get_full(NULL))
      expect_error(.path_get_full(NA))
      expect_error(.path_get_full(123))
      expect_error(.path_get_full(character(0)))
      expect_error(.path_get_full(c("a", "b")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_filter_spec handles empty vectors", {
  skip_if(.is_test_select())
  
  # Empty fn should return empty vector
  expect_identical(.path_filter_spec(character(0)), character(0))
  
  # Valid input with NULL exc
  expect_identical(.path_filter_spec("a/b"), "a/b")
  
  # Valid input with exclusions
  expect_identical(.path_filter_spec("a/b", "a"), character(0))
  expect_identical(.path_filter_spec(c("a/b", "c"), "a"), "c")
})

test_that(".path_filter_spec validates inputs", {
  skip_if(.is_test_select())
  
  # Invalid fn should error
  expect_error(.path_filter_spec(NULL))
  expect_error(.path_filter_spec(NA))
  expect_error(.path_filter_spec(123))
  
  # Invalid exc should error
  expect_error(.path_filter_spec("a", NA))
  expect_error(.path_filter_spec("a", 123))
  expect_error(.path_filter_spec("a", character(0)))
})

test_that(".path_get validates relative parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid relative parameter
      expect_type(.path_get(relative = FALSE), "character")
      expect_type(.path_get(relative = TRUE), "character")
      
      # Invalid relative parameter should error
      expect_error(.path_get(relative = NULL))
      expect_error(.path_get(relative = NA))
      expect_error(.path_get(relative = "true"))
      expect_error(.path_get(relative = 1))
      expect_error(.path_get(relative = c(TRUE, FALSE)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# File filter functions
# ==============================================================================

test_that(".file_filter_dir handles empty vectors", {
  skip_if(.is_test_select())
  
  # Empty vector should return empty vector
  expect_identical(.file_filter_dir(character(0)), character(0))
})

test_that(".file_filter_dir validates inputs", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid input
      expect_type(.file_filter_dir(dir_test), "character")
      
      # Invalid input should error
      expect_error(.file_filter_dir(NULL))
      expect_error(.file_filter_dir(NA))
      expect_error(.file_filter_dir(123))
      expect_error(.file_filter_dir(list("a")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".file_filter_dir_non handles empty vectors", {
  skip_if(.is_test_select())
  
  # Empty vector should return empty vector
  expect_identical(.file_filter_dir_non(character(0)), character(0))
})

test_that(".file_filter_dir_non validates inputs", {
  skip_if(.is_test_select())
  
  # Invalid input should error
  expect_error(.file_filter_dir_non(NULL))
  expect_error(.file_filter_dir_non(NA))
  expect_error(.file_filter_dir_non(123))
  expect_error(.file_filter_dir_non(list("a")))
})

test_that(".file_filter_exists handles empty vectors", {
  skip_if(.is_test_select())
  
  # Empty vector should return empty vector
  expect_identical(.file_filter_exists(character(0)), character(0))
})

test_that(".file_filter_exists validates inputs", {
  skip_if(.is_test_select())
  
  # Invalid input should error
  expect_error(.file_filter_exists(NULL))
  expect_error(.file_filter_exists(NA))
  expect_error(.file_filter_exists(123))
  expect_error(.file_filter_exists(list("a")))
})

test_that(".file_ls validates all parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid inputs
      expect_type(.file_ls(dir_test), "character")
      expect_type(.file_ls(dir_test, recursive = FALSE), "character")
      expect_type(.file_ls(dir_test, full.names = TRUE), "character")
      expect_type(.file_ls(dir_test, all.files = FALSE), "character")
      
      # Invalid path_dir should error
      expect_error(.file_ls(NULL))
      expect_error(.file_ls(NA))
      expect_error(.file_ls(123))
      expect_error(.file_ls(character(0)))
      expect_error(.file_ls(c("a", "b")))
      expect_error(.file_ls("nonexistent/path"))
      
      # Invalid flags should error
      expect_error(.file_ls(dir_test, recursive = NULL))
      expect_error(.file_ls(dir_test, recursive = NA))
      expect_error(.file_ls(dir_test, recursive = "true"))
      expect_error(.file_ls(dir_test, full.names = NULL))
      expect_error(.file_ls(dir_test, all.files = NA))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Directory filter functions
# ==============================================================================

test_that(".dir_filter_exists handles empty vectors", {
  skip_if(.is_test_select())
  
  # Empty vector should return empty vector
  expect_identical(.dir_filter_exists(character(0)), character(0))
})

test_that(".dir_filter_exists validates inputs", {
  skip_if(.is_test_select())
  
  # Invalid input should error
  expect_error(.dir_filter_exists(NULL))
  expect_error(.dir_filter_exists(NA))
  expect_error(.dir_filter_exists(123))
  expect_error(.dir_filter_exists(list("a")))
})

test_that(".dir_ls validates all parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid inputs
      expect_type(.dir_ls(dir_test), "character")
      expect_type(.dir_ls(dir_test, recursive = FALSE), "character")
      expect_type(.dir_ls(dir_test, full.names = TRUE), "character")
      
      # Invalid path_dir should error
      expect_error(.dir_ls(NULL))
      expect_error(.dir_ls(NA))
      expect_error(.dir_ls(123))
      expect_error(.dir_ls(character(0)))
      expect_error(.dir_ls(c("a", "b")))
      expect_error(.dir_ls("nonexistent/path"))
      
      # Invalid flags should error
      expect_error(.dir_ls(dir_test, recursive = NULL))
      expect_error(.dir_ls(dir_test, recursive = NA))
      expect_error(.dir_ls(dir_test, recursive = "true"))
      expect_error(.dir_ls(dir_test, full.names = NULL))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_ls_unremovable handles NULL path_dir", {
  skip_if(.is_test_select())
  
  # NULL path_dir should be allowed
  expect_type(.dir_ls_unremovable(NULL), "character")
  
  # Valid character vector should work
  expect_type(.dir_ls_unremovable(getwd()), "character")
})

test_that(".dir_ls_unremovable validates inputs", {
  skip_if(.is_test_select())
  
  # Invalid input should error
  expect_error(.dir_ls_unremovable(NA))
  expect_error(.dir_ls_unremovable(123))
  expect_error(.dir_ls_unremovable(list("a")))
})

test_that(".dir_clear validates all parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      dir_tmp <- .dir_create_tmp_random()
      
      # Valid inputs
      expect_true(.dir_clear(dir_tmp))
      expect_true(.dir_clear(dir_tmp, recursive_file = TRUE))
      expect_true(.dir_clear(dir_tmp, recursive_dir = TRUE))
      expect_true(.dir_clear(dir_tmp, delete_hidden = FALSE))
      
      # Invalid path_dir should error
      expect_error(.dir_clear(NULL))
      expect_error(.dir_clear(NA))
      expect_error(.dir_clear(123))
      expect_error(.dir_clear(character(0)))
      expect_error(.dir_clear(c("a", "b")))
      
      # Invalid flags should error
      expect_error(.dir_clear(dir_tmp, recursive_file = NULL))
      expect_error(.dir_clear(dir_tmp, recursive_file = NA))
      expect_error(.dir_clear(dir_tmp, recursive_file = "true"))
      expect_error(.dir_clear(dir_tmp, recursive_dir = NULL))
      expect_error(.dir_clear(dir_tmp, delete_hidden = NA))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_clear_check validates path_dir", {
  skip_if(.is_test_select())
  
  # Invalid input should error
  expect_error(.dir_clear_check(NULL))
  expect_error(.dir_clear_check(NA))
  expect_error(.dir_clear_check(123))
  expect_error(.dir_clear_check(character(0)))
  expect_error(.dir_clear_check(c("a", "b")))
})

# ==============================================================================
# Directory copy/move functions
# ==============================================================================

test_that(".dir_copy_file validates all parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      dir_from <- .dir_create_tmp_random()
      dir_to <- .dir_create_tmp_random()
      file.create(file.path(dir_from, "test.txt"))
      
      # Valid inputs
      expect_true(.dir_copy_file("test.txt", dir_from, dir_to))
      
      # Empty fn should return FALSE
      expect_false(.dir_copy_file(character(0), dir_from, dir_to))
      
      # Invalid fn should error
      expect_error(.dir_copy_file(NULL, dir_from, dir_to))
      expect_error(.dir_copy_file(NA, dir_from, dir_to))
      expect_error(.dir_copy_file(123, dir_from, dir_to))
      
      # Invalid path_dir_from should error
      expect_error(.dir_copy_file("test.txt", NULL, dir_to))
      expect_error(.dir_copy_file("test.txt", NA, dir_to))
      expect_error(.dir_copy_file("test.txt", 123, dir_to))
      expect_error(.dir_copy_file("test.txt", character(0), dir_to))
      
      # Invalid path_dir_to should error
      expect_error(.dir_copy_file("test.txt", dir_from, NULL))
      expect_error(.dir_copy_file("test.txt", dir_from, NA))
      expect_error(.dir_copy_file("test.txt", dir_from, 123))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_filter_spec_add_back_file validates inputs", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid inputs with NULL exc
      expect_identical(
        .path_filter_spec_add_back_file("a", dir_test, NULL),
        "a"
      )
      
      # Valid inputs with empty fn
      expect_identical(
        .path_filter_spec_add_back_file(character(0), dir_test, NULL),
        character(0)
      )
      
      # Invalid fn should error
      expect_error(.path_filter_spec_add_back_file(NULL, dir_test, NULL))
      expect_error(.path_filter_spec_add_back_file(NA, dir_test, NULL))
      expect_error(.path_filter_spec_add_back_file(123, dir_test, NULL))
      
      # Invalid path_dir should error
      expect_error(.path_filter_spec_add_back_file("a", NULL, NULL))
      expect_error(.path_filter_spec_add_back_file("a", NA, NULL))
      expect_error(.path_filter_spec_add_back_file("a", 123, NULL))
      expect_error(.path_filter_spec_add_back_file("a", character(0), NULL))
      
      # Invalid path_exc (when not NULL) should error
      expect_error(.path_filter_spec_add_back_file("a", dir_test, NA))
      expect_error(.path_filter_spec_add_back_file("a", dir_test, 123))
      expect_error(.path_filter_spec_add_back_file("a", dir_test, character(0)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Edge cases and special scenarios
# ==============================================================================

test_that("path functions handle paths with special characters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Paths with spaces
      expect_type(.path_filter_spec("a b/c"), "character")
      
      # Paths with dots
      expect_type(.path_filter_spec("../a"), "character")
      expect_type(.path_filter_spec("./a"), "character")
      
      # Paths with multiple slashes (should still work)
      expect_type(.path_filter_spec("a//b"), "character")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("filter functions return correct types for edge cases", {
  skip_if(.is_test_select())
  
  # All filter functions should return character vectors
  expect_type(.path_filter_spec(character(0)), "character")
  expect_type(.file_filter_dir(character(0)), "character")
  expect_type(.file_filter_dir_non(character(0)), "character")
  expect_type(.file_filter_exists(character(0)), "character")
  expect_type(.dir_filter_exists(character(0)), "character")
  
  # All should return length 0
  expect_length(.path_filter_spec(character(0)), 0)
  expect_length(.file_filter_dir(character(0)), 0)
  expect_length(.file_filter_dir_non(character(0)), 0)
  expect_length(.file_filter_exists(character(0)), 0)
  expect_length(.dir_filter_exists(character(0)), 0)
})
