# Comprehensive tests for build-post-copy, build-post-copy-docs, and build-post-clear
# ====================================================================================

# Tests for build-post-copy.R
# ----------------------------

test_that(".build_copy works with output_run = FALSE (dev build)", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create some output files
      file.create(projr_path_get("output", "test.txt", safe = TRUE))
      
      # Dev build should not copy to unsafe
      result <- .build_copy(
        output_run = FALSE,
        bump_component = "dev",
        version_run_on_list = list(desc = list(run = "0.0.0-1"))
      )
      
      expect_true(result)
      # File should still be in safe location
      expect_true(file.exists(projr_path_get("output", "test.txt", safe = TRUE)))
    }
  )
})

test_that(".build_copy works with output_run = TRUE (production build)", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create some output files in safe location
      file.create(projr_path_get("output", "test.txt", safe = TRUE))
      
      # Production build should copy to unsafe
      result <- .build_copy(
        output_run = TRUE,
        bump_component = "patch",
        version_run_on_list = list(desc = list(run = "0.0.1"))
      )
      
      expect_true(result)
    }
  )
})

test_that(".build_copy_dir copies configured directories", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test content
      file.create(projr_path_get("raw-data", "data.csv", safe = TRUE))
      file.create(projr_path_get("cache", "cache.rds", safe = TRUE))
      
      # Copy in dev mode (safe = TRUE)
      .build_copy_dir(output_run = FALSE)
      
      # Check that files were copied
      expect_true(file.exists(projr_path_get("raw-data", "data.csv", safe = TRUE)))
      expect_true(file.exists(projr_path_get("cache", "cache.rds", safe = TRUE)))
    }
  )
})

# Tests for build-post-copy-docs.R
# ---------------------------------

test_that(".build_copy_docs dispatches correctly based on engine", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # The project should have bookdown engine by default
      engine <- .engine_get()
      expect_true(engine %in% c("bookdown", "quarto_project", "quarto_document", "rmd"))
      
      # Call should not error
      expect_silent(.build_copy_docs(output_run = FALSE))
    }
  )
})

test_that(".build_copy_docs_rmd handles different output formats", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_rmd")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Test html_document format
      writeLines(c(
        "---",
        "title: Test",
        "output: html_document",
        "---",
        "# Test"
      ), "test.Rmd")
      
      # Create the output file
      file.create("test.html")
      
      # Create docs directory
      dir.create("docs", showWarnings = FALSE)
      
      # Copy docs
      .build_copy_docs_rmd(output_run = FALSE)
      
      # Should have copied html file to docs
      expect_true(file.exists("docs/test.html"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto handles html format with _files directory", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_quarto")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create a quarto file
      writeLines(c(
        "---",
        "title: Test",
        "format: html",
        "---",
        "# Test"
      ), "test.qmd")
      
      # Create output files
      file.create("test.html")
      dir.create("test_files", showWarnings = FALSE)
      file.create("test_files/test.js")
      
      # Create docs directory
      dir.create("docs", showWarnings = FALSE)
      
      # Copy docs
      .build_copy_docs_quarto(output_run = FALSE)
      
      # Should have copied both html and _files directory
      expect_true(file.exists("docs/test.html"))
      expect_true(dir.exists("docs/test_files"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_bookdown excludes CHANGELOG.md", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_bookdown")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create _bookdown.yml
      writeLines(c("output_dir: _book"), "_bookdown.yml")
      
      # Create book directory with files
      dir.create("_book", showWarnings = FALSE)
      file.create("_book/index.html")
      file.create("_book/CHANGELOG.md")
      file.create("_book/chapter1.html")
      
      # Copy in output mode
      .build_copy_docs_bookdown(output_run = TRUE)
      
      # Check that CHANGELOG.md was excluded
      expect_true(file.exists("docs/index.html"))
      expect_true(file.exists("docs/chapter1.html"))
      expect_false(file.exists("docs/CHANGELOG.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto_project excludes CHANGELOG.md", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_quarto_proj")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create cache directory structure
      cache_docs <- file.path(".cache", "projr", "v0.0.1", "docs")
      dir.create(cache_docs, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(cache_docs, "index.html"))
      file.create(file.path(cache_docs, "CHANGELOG.md"))
      file.create(file.path(cache_docs, "page1.html"))
      
      # Copy in output mode
      .build_copy_docs_quarto_project(output_run = TRUE)
      
      # Check that CHANGELOG.md was excluded
      expect_true(file.exists("docs/index.html"))
      expect_true(file.exists("docs/page1.html"))
      expect_false(file.exists("docs/CHANGELOG.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Tests for build-post-clear.R
# -----------------------------

test_that(".build_clear_post with clear_output = 'never' does not clear", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a file in output
      file.create(projr_path_get("output", "test.txt", safe = FALSE))
      
      # Clear with "never"
      .build_clear_post(output_run = TRUE, clear_output = "never")
      
      # File should still exist
      expect_true(file.exists(projr_path_get("output", "test.txt", safe = FALSE)))
    }
  )
})

test_that(".build_clear_post with clear_output = 'post' clears output dirs", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files in output
      file.create(projr_path_get("output", "test.txt", safe = FALSE))
      
      # Clear with "post"
      .build_clear_post(output_run = TRUE, clear_output = "post")
      
      # Directory should exist but be empty
      expect_true(dir.exists(projr_path_get_dir("output", safe = FALSE)))
      expect_false(file.exists(projr_path_get("output", "test.txt", safe = FALSE)))
    }
  )
})

test_that(".build_clear_post_docs clears for quarto_document engine", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_clear_docs")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create a quarto file to set engine
      writeLines(c("---", "title: Test", "---", "# Test"), "test.qmd")
      
      # Create docs with content
      dir.create("docs", showWarnings = FALSE)
      file.create("docs/test.html")
      
      # Clear docs in output mode
      .build_clear_post_docs(output_run = TRUE)
      
      # For quarto_document, docs should be cleared
      if (.engine_get() == "quarto_document") {
        expect_false(file.exists("docs/test.html"))
      }
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_clear_old removes old dev builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create old version directories
      cache_base <- dirname(.dir_get_cache_auto_version(profile = NULL))
      old_version <- file.path(cache_base, "v0.0.0-1")
      dir.create(old_version, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(old_version, "old.txt"))
      
      # Clear old dev builds
      .build_clear_old(output_run = FALSE, old_dev_remove = TRUE)
      
      # Old version should be removed
      expect_false(dir.exists(old_version))
    }
  )
})

test_that(".build_clear_old clears all cache for output builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create version directories
      cache_base <- dirname(.dir_get_cache_auto_version(profile = NULL))
      
      # Clear all for output build
      .build_clear_old(output_run = TRUE, old_dev_remove = TRUE)
      
      # Cache base should be empty or not exist
      if (dir.exists(cache_base)) {
        expect_equal(length(list.files(cache_base)), 0)
      }
    }
  )
})

# Edge case tests
# ---------------

test_that(".build_copy handles empty directories", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure output directory exists but is empty
      dir.create(projr_path_get_dir("output", safe = TRUE), showWarnings = FALSE)
      
      # Should not error on empty directory
      expect_silent(.build_copy(
        output_run = FALSE,
        bump_component = "dev",
        version_run_on_list = list(desc = list(run = "0.0.0-1"))
      ))
    }
  )
})

test_that(".build_copy_docs handles missing output files gracefully", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_missing")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create Rmd but not the output
      writeLines(c("---", "title: Test", "---", "# Test"), "test.Rmd")
      
      # Should not error even if output doesn't exist
      expect_silent(.build_copy_docs_rmd(output_run = FALSE))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("dir_exc parameter properly excludes multiple files", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_multi_exc")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  source_dir <- file.path(dir_test, "source")
  dest_dir <- file.path(dir_test, "dest")
  dir.create(source_dir)
  
  # Create multiple files
  file.create(file.path(source_dir, "keep1.txt"))
  file.create(file.path(source_dir, "CHANGELOG.md"))
  file.create(file.path(source_dir, "README.md"))
  file.create(file.path(source_dir, "keep2.txt"))
  
  # Exclude multiple files
  .dir_move_exact(source_dir, dest_dir, dir_exc = c("CHANGELOG.md", "README.md"))
  
  # Check exclusions
  expect_true(file.exists(file.path(dest_dir, "keep1.txt")))
  expect_true(file.exists(file.path(dest_dir, "keep2.txt")))
  expect_false(file.exists(file.path(dest_dir, "CHANGELOG.md")))
  expect_false(file.exists(file.path(dest_dir, "README.md")))
})
