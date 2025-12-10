# Tests for improving coverage of R/path.R
# These tests cover functions that were previously untested or under-tested

# ==============================================================================
# Directory checking functions
# ==============================================================================

test_that(".dir_check_identical works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create two identical directories
      dir1 <- .dir_create_tmp_random()
      dir2 <- .dir_create_tmp_random()

      # Both empty - should be identical
      expect_true(.dir_check_identical(dir1, dir2))

      # Add same file to both
      file.create(file.path(dir1, "test.txt"))
      writeLines("content", file.path(dir1, "test.txt"))
      file.create(file.path(dir2, "test.txt"))
      writeLines("content", file.path(dir2, "test.txt"))
      expect_true(.dir_check_identical(dir1, dir2))

      # Add different file to dir2
      file.create(file.path(dir2, "other.txt"))
      expect_false(.dir_check_identical(dir1, dir2))

      # Test with NULL path_dir_two (uses .path_get())
      expect_false(.dir_check_identical(dir1, NULL))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_check_identical validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_tmp <- .dir_create_tmp_random()

      # Invalid inputs should error
      expect_error(.dir_check_identical(NULL))
      expect_error(.dir_check_identical(NA))
      expect_error(.dir_check_identical(123))
      expect_error(.dir_check_identical(character(0)))
      expect_error(.dir_check_identical(c("a", "b")))

      # Nonexistent directory should error
      expect_error(.dir_check_identical("nonexistent/path"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Directory clearing functions
# ==============================================================================

test_that(".dir_clear_file works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory with files
      dir_tmp <- .test_content_setup_dir()

      # Clear files (default: recursive, all files including hidden)
      .dir_clear_file(dir_tmp)
      expect_identical(.file_ls(dir_tmp), character(0))
      expect_length(.dir_ls(dir_tmp), 2) # subdirs still exist

      # Create files again and test non-recursive
      dir_tmp <- .test_content_setup_dir()
      .dir_clear_file(dir_tmp, recursive = FALSE)
      expect_length(.file_ls(dir_tmp), 2) # Only subdir files remain

      # Test with hidden files exclusion
      dir_tmp <- .test_content_setup_dir()
      .dir_clear_file(dir_tmp, delete_hidden = FALSE)
      expect_length(.file_ls(dir_tmp, all.files = TRUE), 1) # .hidden.txt remains

      # Test with dir_exc
      dir_tmp <- .test_content_setup_dir()
      .dir_clear_file(dir_tmp, dir_exc = "subdir1")
      expect_length(.file_ls(dir_tmp), 2) # subdir1 files remain
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_clear_dir works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory with subdirectories
      dir_tmp <- .test_content_setup_dir()

      # Clear directories (non-recursive by default - removes only immediate subdirs)
      .dir_clear_dir(dir_tmp, recursive = FALSE)
      expect_identical(.dir_ls(dir_tmp, recursive = FALSE), character(0))
      expect_identical(.dir_ls(dir_tmp, recursive = TRUE), character(0))

      # Test recursive clearing
      dir_tmp <- .test_content_setup_dir()
      .dir_clear_dir(dir_tmp, recursive = TRUE)
      expect_identical(.dir_ls(dir_tmp), character(0))

      # Test with dir_exc
      dir_tmp <- .test_content_setup_dir()
      .dir_clear_dir(dir_tmp, recursive = TRUE, dir_exc = "subdir1")
      expect_length(.dir_ls(dir_tmp, recursive = TRUE), 2) # subdir1 and subdir2 remain
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Directory copy functions
# ==============================================================================

test_that(".dir_copy works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create source directory
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()

      # Copy without exclusions
      .dir_copy(dir_from, dir_to)
      expect_identical(
        .file_ls(dir_from) |> sort(),
        .file_ls(dir_to) |> sort()
      )

      # Copy with dir exclusion
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      .dir_copy(dir_from, dir_to, dir_exc = "subdir1")
      expect_length(.file_ls(dir_to), 2) # Only root files

      # Copy with file exclusion
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      .dir_copy(dir_from, dir_to, fn_exc = "abc.txt")
      expect_false("abc.txt" %in% .file_ls(dir_to))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_copy validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_tmp <- .dir_create_tmp_random()

      # Invalid path_dir_from should error
      expect_error(.dir_copy(NULL, dir_tmp))
      expect_error(.dir_copy(NA, dir_tmp))
      expect_error(.dir_copy(123, dir_tmp))
      expect_error(.dir_copy(character(0), dir_tmp))

      # Invalid path_dir_to should error
      expect_error(.dir_copy(dir_tmp, NULL))
      expect_error(.dir_copy(dir_tmp, NA))
      expect_error(.dir_copy(dir_tmp, 123))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_copy_exact_file works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create source directory
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()

      # Add files to destination first
      file.create(file.path(dir_to, "extra.txt"))

      # Copy exact files (should clear destination first)
      files_to_copy <- c("abc.txt", "subdir1/def.txt")
      .dir_copy_exact_file(files_to_copy, dir_from, dir_to)

      # Check that only specified files exist
      expect_identical(
        .file_ls(dir_to) |> sort(),
        files_to_copy |> sort()
      )
      expect_false("extra.txt" %in% .file_ls(dir_to))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_copy_file_check works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_from <- .test_content_setup_dir()

      # Valid files should return TRUE
      expect_true(.dir_copy_file_check("abc.txt", dir_from))

      # Empty vector should return FALSE
      expect_false(.dir_copy_file_check(character(0), dir_from))

      # Nonexistent file should return FALSE
      expect_false(.dir_copy_file_check("nonexistent.txt", dir_from))

      # Multiple files with at least one existing should return TRUE
      expect_true(.dir_copy_file_check(c("abc.txt", "nonexistent.txt"), dir_from))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_copy_file_tree works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_to <- .dir_create_tmp_random()

      # Copy directory tree for files
      files <- c("a/b/c.txt", "x/y/z.txt", "simple.txt")
      .dir_copy_file_tree(files, dir_to)

      # Check that directories were created
      expect_true(dir.exists(file.path(dir_to, "a/b")))
      expect_true(dir.exists(file.path(dir_to, "x/y")))

      # Empty vector should work
      expect_true(.dir_copy_file_tree(character(0), dir_to))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_copy_tree works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create source with subdirectories
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()

      # Copy tree structure
      .dir_copy_tree(dir_from, dir_to)

      # Check that directories were created
      dirs_from <- .dir_ls(dir_from)
      dirs_to <- .dir_ls(dir_to)
      expect_identical(dirs_from, dirs_to)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Directory filter functions
# ==============================================================================

test_that(".dir_filter_exists_single works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_tmp <- .dir_create_tmp_random()

      # Existing directory should return itself
      expect_identical(.dir_filter_exists_single(dir_tmp), dir_tmp)

      # Nonexistent directory should return empty
      expect_identical(.dir_filter_exists_single("nonexistent"), character(0))

      # File path should return empty
      file_path <- file.path(dir_tmp, "test.txt")
      file.create(file_path)
      expect_identical(.dir_filter_exists_single(file_path), character(0))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_filter_exists_single validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid input should error
  expect_error(.dir_filter_exists_single(NULL))
  expect_error(.dir_filter_exists_single(NA))
  expect_error(.dir_filter_exists_single(123))
  expect_error(.dir_filter_exists_single(character(0)))
  expect_error(.dir_filter_exists_single(c("a", "b")))
})

test_that(".dir_filter_removable works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory
      dir_tmp <- .dir_create_tmp_random()
      subdir <- file.path(dir_tmp, "subdir")
      .dir_create(subdir)

      # Subdirectory should be removable
      expect_true(subdir %in% .dir_filter_removable(subdir, dir_tmp))

      # Project directory should not be removable
      expect_false(dir_test %in% .dir_filter_removable(dir_test))

      # . and .. should not be removable
      expect_length(.dir_filter_removable(c(".", "..")), 0)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_filter_removable validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid path should error
  expect_error(.dir_filter_removable(NULL))
  expect_error(.dir_filter_removable(NA))
  expect_error(.dir_filter_removable(123))
})

# ==============================================================================
# Directory path generation
# ==============================================================================

test_that(".dir_get_tmp_random_path generates valid paths", {
  skip_if(.is_test_select())

  # Should generate a path
  path1 <- .dir_get_tmp_random_path()
  expect_type(path1, "character")
  expect_length(path1, 1)

  # Should be in tempdir
  expect_true(grepl(tempdir(), path1, fixed = TRUE))

  # Should contain "randomnia"
  expect_true(grepl("randomnia", path1))

  # Multiple calls should generate different paths
  path2 <- .dir_get_tmp_random_path()
  expect_false(identical(path1, path2))
})

# ==============================================================================
# Directory move functions
# ==============================================================================

test_that(".dir_move_file works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create source directory
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      files_orig <- .file_ls(dir_from)

      # Move files without specifying fn
      .dir_move_file(fn = NULL, path_dir_from = dir_from, path_dir_to = dir_to)
      expect_identical(.file_ls(dir_from), character(0))
      expect_identical(.file_ls(dir_to) |> sort(), files_orig |> sort())

      # Move specific files
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      .dir_move_file(fn = "abc.txt", path_dir_from = dir_from, path_dir_to = dir_to)
      expect_false("abc.txt" %in% .file_ls(dir_from))
      expect_true("abc.txt" %in% .file_ls(dir_to))

      # Move with exclusions
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      .dir_move_file(
        fn = NULL, path_dir_from = dir_from, path_dir_to = dir_to,
        dir_exc = "subdir1"
      )
      expect_length(.file_ls(dir_from), 2) # subdir1 files remain
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_move_dir works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create source directory
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      dirs_orig <- .dir_ls(dir_from)

      # Move directories without specifying path_dir
      suppressWarnings(
        .dir_move_dir(
          path_dir = NULL, path_dir_from = dir_from,
          path_dir_to = dir_to, dir_exc = NULL
        )
      )
      expect_identical(.dir_ls(dir_from), character(0))

      # Move specific directory
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      suppressWarnings(
        .dir_move_dir(
          path_dir = "subdir1", path_dir_from = dir_from,
          path_dir_to = dir_to, dir_exc = NULL
        )
      )
      expect_false("subdir1" %in% .dir_ls(dir_from))
      expect_true("subdir1" %in% .dir_ls(dir_to))

      # Move with exclusions
      dir_from <- .test_content_setup_dir()
      dir_to <- .dir_create_tmp_random()
      suppressWarnings(
        .dir_move_dir(
          path_dir = NULL, path_dir_from = dir_from,
          path_dir_to = dir_to, dir_exc = "subdir1"
        )
      )
      expect_true("subdir1" %in% .dir_ls(dir_from))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Directory removal functions
# ==============================================================================

test_that(".dir_rm works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directories
      dir1 <- .dir_create_tmp_random()
      dir2 <- .dir_create_tmp_random()
      file.create(file.path(dir1, "test.txt"))

      # Remove directories
      .dir_rm(c(dir1, dir2))
      expect_false(dir.exists(dir1))
      expect_false(dir.exists(dir2))

      # Removing nonexistent directory should work
      expect_true(.dir_rm("nonexistent"))

      # Empty vector should work
      expect_true(.dir_rm(character(0)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_rm validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid input should error
  expect_error(.dir_rm(NULL))
  expect_error(.dir_rm(NA))
  expect_error(.dir_rm(123))
})

test_that(".dir_rm_single works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory
      dir_tmp <- .dir_create_tmp_random()
      file.create(file.path(dir_tmp, "test.txt"))

      # Remove directory
      .dir_rm_single(dir_tmp)
      expect_false(dir.exists(dir_tmp))

      # Removing nonexistent directory should work
      expect_true(.dir_rm_single("nonexistent"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_rm_single validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid input should error
  expect_error(.dir_rm_single(NULL))
  expect_error(.dir_rm_single(NA))
  expect_error(.dir_rm_single(123))
  expect_error(.dir_rm_single(character(0)))
  expect_error(.dir_rm_single(c("a", "b")))
})

# ==============================================================================
# File listing functions
# ==============================================================================

test_that(".file_ls_rm_dir works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_tmp <- .test_content_setup_dir()

      # With full.names = FALSE
      files <- list.files(dir_tmp, recursive = TRUE, all.files = TRUE)
      filtered <- .file_ls_rm_dir(files, dir_tmp, full.names = FALSE)
      expect_true(all(!grepl("/$", filtered)))

      # With full.names = TRUE
      files_full <- list.files(dir_tmp, recursive = TRUE, all.files = TRUE, full.names = TRUE)
      filtered_full <- .file_ls_rm_dir(files_full, dir_tmp, full.names = TRUE)
      expect_true(all(file.exists(filtered_full)))
      expect_false(any(fs::is_dir(filtered_full)))

      # Empty input should return empty
      expect_identical(
        .file_ls_rm_dir(character(0), dir_tmp, full.names = FALSE),
        character(0)
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".file_ls_rm_dir validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      dir_tmp <- .dir_create_tmp_random()

      # Invalid fn should error
      expect_error(.file_ls_rm_dir(NULL, dir_tmp, FALSE))
      expect_error(.file_ls_rm_dir(NA, dir_tmp, FALSE))
      expect_error(.file_ls_rm_dir(123, dir_tmp, FALSE))

      # Invalid path_dir should error
      expect_error(.file_ls_rm_dir("test", NULL, FALSE))
      expect_error(.file_ls_rm_dir("test", NA, FALSE))
      expect_error(.file_ls_rm_dir("test", 123, FALSE))
      expect_error(.file_ls_rm_dir("test", character(0), FALSE))

      # Invalid full.names should error
      expect_error(.file_ls_rm_dir("test", dir_tmp, NULL))
      expect_error(.file_ls_rm_dir("test", dir_tmp, NA))
      expect_error(.file_ls_rm_dir("test", dir_tmp, "true"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Filter utility functions
# ==============================================================================

test_that(".filter_filter_non_na works correctly", {
  skip_if(.is_test_select())

  # Numeric vector with NA
  expect_identical(.filter_filter_non_na(c(1, NA, 3)), c(1, 3))

  # Character vector with NA
  expect_identical(.filter_filter_non_na(c("a", NA, "c")), c("a", "c"))

  # All NA - returns logical(0) because input is logical
  expect_identical(.filter_filter_non_na(c(NA, NA)), logical(0))

  # No NA
  expect_identical(.filter_filter_non_na(c(1, 2, 3)), c(1, 2, 3))

  # Empty vector
  expect_identical(.filter_filter_non_na(character(0)), character(0))
  expect_identical(.filter_filter_non_na(numeric(0)), numeric(0))

  # Logical with NA
  expect_identical(.filter_filter_non_na(c(TRUE, NA, FALSE)), c(TRUE, FALSE))
})

# ==============================================================================
# Path validation functions
# ==============================================================================

test_that(".path_force_abs_check works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Relative path with NULL path_dir should pass
      expect_silent(.path_force_abs_check("a/b", NULL))

      # Relative path with path_dir should pass
      expect_silent(.path_force_abs_check("a/b", dir_test))

      # Absolute path with NULL path_dir should pass
      abs_path <- fs::path_abs("a")
      expect_silent(.path_force_abs_check(abs_path, NULL))

      # Absolute path with non-NULL path_dir should error
      expect_error(.path_force_abs_check(abs_path, dir_test))

      # Multiple absolute paths with path_dir should error
      abs_paths <- c(fs::path_abs("a"), fs::path_abs("b"))
      expect_error(.path_force_abs_check(abs_paths, dir_test))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_force_abs_check validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid fn should error
  expect_error(.path_force_abs_check(NULL, NULL))
  expect_error(.path_force_abs_check(NA, NULL))
  expect_error(.path_force_abs_check(123, NULL))
  expect_error(.path_force_abs_check(character(0), NULL))
})
