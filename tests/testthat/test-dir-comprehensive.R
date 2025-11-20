# Comprehensive tests for directory functions
# Tests for projr_path_get_dir, projr_path_get, and related functions

test_that("projr_path_get_dir works with different labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test basic label retrieval
      expect_true(is.character(projr_path_get_dir("raw-data")))
      expect_true(is.character(projr_path_get_dir("cache")))
      expect_true(is.character(projr_path_get_dir("output")))
      expect_true(is.character(projr_path_get_dir("docs")))
      expect_true(is.character(projr_path_get_dir("project")))
      expect_true(is.character(projr_path_get_dir("code")))
      expect_true(is.character(projr_path_get_dir("data")))
      
      # Test invalid label
      expect_error(projr_path_get_dir("invalid_label"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get_dir works with safe parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test safe vs unsafe paths for output
      path_safe <- projr_path_get_dir("output", safe = TRUE, create = FALSE)
      path_unsafe <- projr_path_get_dir("output", safe = FALSE, create = FALSE)
      
      expect_false(identical(path_safe, path_unsafe))
      expect_true(grepl("projr", path_safe))
      expect_false(grepl("projr", path_unsafe))
      
      # Test safe vs unsafe paths for docs
      path_safe_docs <- projr_path_get_dir("docs", safe = TRUE, create = FALSE)
      path_unsafe_docs <- projr_path_get_dir("docs", safe = FALSE, create = FALSE)
      
      expect_false(identical(path_safe_docs, path_unsafe_docs))
      expect_true(grepl("projr", path_safe_docs))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get_dir create parameter works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Clean up if cache exists
      cache_path <- projr_path_get_dir("cache", create = FALSE)
      if (dir.exists(cache_path)) {
        unlink(cache_path, recursive = TRUE)
      }
      
      # Test create = FALSE
      path_no_create <- projr_path_get_dir("cache", create = FALSE)
      expect_false(dir.exists(path_no_create))
      
      # Test create = TRUE
      path_create <- projr_path_get_dir("cache", create = TRUE)
      expect_true(dir.exists(path_create))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get_dir relative and absolute parameters work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test default (neither relative nor absolute)
      path_default <- projr_path_get_dir("cache", create = FALSE)
      
      # Test relative
      path_relative <- projr_path_get_dir("cache", create = FALSE, relative = TRUE)
      expect_false(fs::is_absolute_path(path_relative))
      
      # Test absolute
      path_absolute <- projr_path_get_dir("cache", create = FALSE, absolute = TRUE)
      expect_true(fs::is_absolute_path(path_absolute))
      
      # Test that relative and absolute cannot both be TRUE
      expect_error(projr_path_get_dir("cache", relative = TRUE, absolute = TRUE))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get_dir works with subdirectories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test single subdirectory
      path_sub1 <- projr_path_get_dir("cache", "subdir1")
      expect_true(grepl("subdir1", path_sub1))
      
      # Test multiple subdirectories
      path_sub_multi <- projr_path_get_dir("cache", "sub1", "sub2", "sub3")
      expect_true(grepl("sub1", path_sub_multi))
      expect_true(grepl("sub2", path_sub_multi))
      expect_true(grepl("sub3", path_sub_multi))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get works for file paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test with no additional arguments (should behave like projr_path_get_dir)
      path_dir <- projr_path_get("cache", create = FALSE)
      path_dir_expected <- projr_path_get_dir("cache", create = FALSE)
      expect_identical(path_dir, path_dir_expected)
      
      # Test with filename
      path_file <- projr_path_get("cache", "test.txt", create = FALSE)
      expect_true(grepl("test\\.txt", path_file))
      
      # Test with subdirectory and filename
      path_file_sub <- projr_path_get("cache", "subdir", "test.txt", create = FALSE)
      expect_true(grepl("subdir", path_file_sub))
      expect_true(grepl("test\\.txt", path_file_sub))
      
      # Test parent directory creation
      path_file_create <- projr_path_get("cache", "newdir", "file.txt", create = TRUE)
      expect_true(dir.exists(dirname(path_file_create)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directory creation works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test directory creation using projr_path_get_dir with create = TRUE
      path_raw_safe <- projr_path_get_dir("raw-data", safe = TRUE, create = TRUE)
      expect_true(dir.exists(path_raw_safe))
      
      path_raw_unsafe <- projr_path_get_dir("raw-data", safe = FALSE, create = TRUE)
      expect_true(dir.exists(path_raw_unsafe))
      
      # Test cache directory creation
      path_cache <- projr_path_get_dir("cache", create = TRUE)
      expect_true(dir.exists(path_cache))
      
      # Test output directory creation  
      path_output <- projr_path_get_dir("output", create = TRUE)
      expect_true(dir.exists(path_output))
      
      # Test that created directories can be accessed again
      path_cache_again <- projr_path_get_dir("cache", create = FALSE)
      expect_true(dir.exists(path_cache_again))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get_cache_build_dir works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test basic cache build directory
      path_cache_build <- projr_path_get_cache_build_dir(create = FALSE, profile = NULL)
      expect_true(is.character(path_cache_build))
      expect_true(grepl("projr", path_cache_build))
      expect_true(grepl("v0\\.0\\.0-1", path_cache_build))
      
      # Test with subdirectory
      path_cache_sub <- projr_path_get_cache_build_dir("output", create = FALSE, profile = NULL)
      expect_true(grepl("output", path_cache_sub))
      
      # Test creation
      path_create <- projr_path_get_cache_build_dir("testdir", create = TRUE, profile = NULL)
      expect_true(dir.exists(path_create))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get_cache_build works for files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test basic cache build path
      path_cache_build <- projr_path_get_cache_build(create = FALSE, profile = NULL)
      expect_true(is.character(path_cache_build))
      
      # Test with subdirectory and file
      path_file <- projr_path_get_cache_build("output", "test.txt", create = FALSE, profile = NULL)
      expect_true(grepl("output", path_file))
      expect_true(grepl("test\\.txt", path_file))
      
      # Test parent directory creation
      path_create_file <- projr_path_get_cache_build("newdir", "file.txt", create = TRUE, profile = NULL)
      expect_true(dir.exists(dirname(path_create_file)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directory label validation works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Valid labels
      valid_labels <- c("raw-data", "cache", "output", "docs", "project", "code", "data")
      for (label in valid_labels) {
        expect_silent(projr_path_get_dir(label, create = FALSE))
      }
      
      # Invalid labels should error
      expect_error(projr_path_get_dir("invalid"))
      expect_error(projr_path_get_dir(""))
      expect_error(projr_path_get_dir(123))
      expect_error(projr_path_get_dir(NULL))
      expect_error(projr_path_get_dir(c("cache", "output")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directory functions handle edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test code label returns temporary directory
      path_code <- projr_path_get_dir("code", create = FALSE)
      expect_true(grepl("projr", path_code))
      
      # Test project label returns current directory
      path_project <- projr_path_get_dir("project", create = FALSE)
      expect_identical(path_project, ".")
      
      # Test data label with safe = TRUE (default) uses cache build dir
      path_data_safe <- projr_path_get_dir("data", create = FALSE, safe = TRUE)
      expect_true(grepl("projr", path_data_safe))
      
      # Test data label with safe = FALSE
      path_data_unsafe <- projr_path_get_dir("data", create = FALSE, safe = FALSE)
      expect_identical(path_data_unsafe, "data")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directory path normalization works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test that paths are normalized
      path_normal <- projr_path_get_dir("cache", "sub1", "sub2", create = FALSE)
      
      # Should not contain double slashes or trailing slashes
      expect_false(grepl("//", path_normal))
      expect_false(grepl("/$", path_normal))
      
      # Test path_get file normalization
      path_file <- projr_path_get("cache", "dir1", "file.txt", create = FALSE)
      expect_true(file.path("cache", "dir1", "file.txt") != path_file ||
                  grepl("cache.*dir1.*file\\.txt", path_file))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_docs_quarto_project_unset_default handles NULL project type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal _projr.yml WITHOUT docs path
      writeLines("build:\n  cache: []", "_projr.yml")
      writeLines("0.0.0-1", "VERSION")

      # Create a _quarto.yml WITHOUT project type specified
      # This is an edge case that can happen in real projects
      writeLines("format:\n  html: default", "_quarto.yml")

      # Create a quarto document
      writeLines(c("---", "title: Test", "---", "", "# Hello"), "test.qmd")

      # Engine should be detected as quarto_project (because _quarto.yml exists)
      expect_identical(.engine_get(), "quarto_project")

      # This should not error - should return "_site" as default
      result <- .dir_get_docs_quarto_project_unset_default()
      expect_identical(result, "_site")

      # Verify the full path retrieval also works without error
      docs_path <- projr_path_get_dir("docs", safe = FALSE, create = FALSE)
      expect_true(is.character(docs_path))
      expect_identical(docs_path, "_site")
    },
    force = TRUE,
    quiet = TRUE
  )
})
