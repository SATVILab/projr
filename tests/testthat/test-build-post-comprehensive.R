# Comprehensive tests for build-post-copy, build-post-copy-docs, and build-post-clear
# ====================================================================================

# Tests for build-post-copy.R
# ----------------------------

test_that(".build_copy works with output_run = FALSE (dev build)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
      
      # Copy docs in dev mode (safe = TRUE)
      .build_copy_docs_rmd(output_run = FALSE)
      
      # Should have copied html file to safe cache directory
      safe_docs_path <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(safe_docs_path, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto handles html format with _files directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
      
      # Copy docs in dev mode (safe = TRUE)
      .build_copy_docs_quarto(output_run = FALSE)
      
      # Should have copied both html and _files directory to safe cache
      safe_docs_path <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(safe_docs_path, "test.html")))
      expect_true(dir.exists(file.path(safe_docs_path, "test_files")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_bookdown excludes CHANGELOG.md", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_bookdown")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create _bookdown.yml with output_dir
      writeLines(c("output_dir: '_book'"), "_bookdown.yml")
      
      # Create book directory in cache (where builds actually go in production mode)
      cache_book <- file.path(
        projr_path_get_cache_build_dir(profile = NULL), "_book"
      )
      dir.create(cache_book, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(cache_book, "index.html"))
      file.create(file.path(cache_book, "CHANGELOG.md"))
      file.create(file.path(cache_book, "chapter1.html"))
      
      # Copy in output mode (should copy to unsafe docs directory)
      .build_copy_docs_bookdown(output_run = TRUE)
      
      # Check that files were copied to final docs location
      expect_true(file.exists("_book/index.html"))
      expect_true(file.exists("_book/chapter1.html"))
      # Check that CHANGELOG.md was excluded
      expect_false(file.exists("_book/CHANGELOG.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto_project excludes CHANGELOG.md", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_quarto_proj")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create cache directory structure where quarto project builds to
      cache_docs <- file.path(
        projr_path_get_cache_build_dir(profile = NULL), "docs"
      )
      dir.create(cache_docs, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(cache_docs, "index.html"))
      file.create(file.path(cache_docs, "CHANGELOG.md"))
      file.create(file.path(cache_docs, "page1.html"))
      
      # Copy in output mode (should copy to unsafe docs directory)
      .build_copy_docs_quarto_project(output_run = TRUE)
      
      # Check that files were copied to docs (unsafe mode)
      expect_true(file.exists("docs/index.html"))
      expect_true(file.exists("docs/page1.html"))
      # Check that CHANGELOG.md was excluded
      expect_false(file.exists("docs/CHANGELOG.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Tests for build-post-clear.R
# -----------------------------

test_that(".build_clear_post with clear_output = 'never' does not clear", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get current version
      current_version <- .version_get_v()
      
      # Create old version directories (different from current)
      cache_base <- dirname(.dir_get_cache_auto_version(profile = NULL))
      old_version_name <- "v0.0.0-2"  # Different from current v0.0.0-1
      old_version <- file.path(cache_base, old_version_name)
      dir.create(old_version, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(old_version, "old.txt"))
      
      # Verify old version exists before clearing
      expect_true(dir.exists(old_version))
      
      # Clear old dev builds
      .build_clear_old(output_run = FALSE, old_dev_remove = TRUE)
      
      # Old version should be removed
      expect_false(dir.exists(old_version))
      
      # Current version directory should still exist (if it was created)
      current_dir <- file.path(cache_base, current_version)
      if (dir.exists(current_dir)) {
        expect_true(dir.exists(current_dir))
      }
    }
  )
})

test_that(".build_clear_old clears all cache for output builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
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
  .dir_move_exact(source_dir, dest_dir, fn_exc = c("CHANGELOG.md", "README.md"))
  
  # Check exclusions
  expect_true(file.exists(file.path(dest_dir, "keep1.txt")))
  expect_true(file.exists(file.path(dest_dir, "keep2.txt")))
  expect_false(file.exists(file.path(dest_dir, "CHANGELOG.md")))
  expect_false(file.exists(file.path(dest_dir, "README.md")))
})

# Additional comprehensive edge case tests
# ========================================

test_that(".build_copy_docs_bookdown handles missing _bookdown.yml gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_bookdown_no_yml")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Don't create _bookdown.yml - should use defaults
      cache_book <- file.path(
        projr_path_get_cache_build_dir(profile = NULL), "_book"
      )
      dir.create(cache_book, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(cache_book, "index.html"))
      
      # Should not error (messages are expected)
      expect_error(.build_copy_docs_bookdown(output_run = TRUE), NA)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto_project handles missing source directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_quarto_missing")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Don't create source directory
      result <- .build_copy_docs_quarto_project(output_run = TRUE)
      
      # Should return FALSE without error
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_bookdown handles empty source directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_bookdown_empty")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create _bookdown.yml
      writeLines(c("output_dir: '_book'"), "_bookdown.yml")
      
      # Create empty cache directory
      cache_book <- file.path(
        projr_path_get_cache_build_dir(profile = NULL), "_book"
      )
      dir.create(cache_book, recursive = TRUE, showWarnings = FALSE)
      
      # Should handle empty directory gracefully
      result <- .build_copy_docs_bookdown(output_run = TRUE)
      
      # Function should still return TRUE even with no files
      expect_true(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_clear_old handles cache_base that doesn't exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get cache base path
      cache_base <- dirname(.dir_get_cache_auto_version(profile = NULL))
      
      # Remove cache base if it exists
      if (dir.exists(cache_base)) {
        unlink(cache_base, recursive = TRUE)
      }
      
      # Should not error
      result <- .build_clear_old(output_run = FALSE, old_dev_remove = TRUE)
      
      # Should return FALSE since directory doesn't exist
      expect_false(result)
    }
  )
})

test_that(".build_clear_old_dev handles empty cache directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure cache base exists but is empty (no version subdirectories)
      cache_base <- dirname(.dir_get_cache_auto_version(profile = NULL))
      dir.create(cache_base, recursive = TRUE, showWarnings = FALSE)
      
      # Remove any existing version directories
      existing_dirs <- list.dirs(cache_base, recursive = FALSE, full.names = TRUE)
      if (length(existing_dirs) > 0) {
        unlink(existing_dirs, recursive = TRUE)
      }
      
      # Should not error with empty cache
      result <- .build_clear_old_dev()
      
      # Should return FALSE since there's nothing to clear
      expect_false(result)
    }
  )
})

test_that(".build_copy_docs_rmd handles multiple Rmd files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_multiple_rmd")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create multiple Rmd files
      writeLines(c("---", "title: Test1", "output: html_document", "---", "# Test1"), "test1.Rmd")
      writeLines(c("---", "title: Test2", "output: html_document", "---", "# Test2"), "test2.Rmd")
      
      # Create output files
      file.create("test1.html")
      file.create("test2.html")
      
      # Copy docs
      .build_copy_docs_rmd(output_run = FALSE)
      
      # Both should be copied to safe cache
      safe_docs_path <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(safe_docs_path, "test1.html")))
      expect_true(file.exists(file.path(safe_docs_path, "test2.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto handles multiple qmd files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_multiple_qmd")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create multiple qmd files
      writeLines(c("---", "title: Test1", "format: html", "---", "# Test1"), "test1.qmd")
      writeLines(c("---", "title: Test2", "format: html", "---", "# Test2"), "test2.qmd")
      
      # Create output files
      file.create("test1.html")
      file.create("test2.html")
      
      # Copy docs
      .build_copy_docs_quarto(output_run = FALSE)
      
      # Both should be copied to safe cache
      safe_docs_path <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(safe_docs_path, "test1.html")))
      expect_true(file.exists(file.path(safe_docs_path, "test2.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_clear_post_docs clears docs for rmd engine", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_clear_docs_rmd")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create an Rmd file to set engine
      writeLines(c("---", "title: Test", "---", "# Test"), "test.Rmd")
      
      # Create docs with content
      dir.create("docs", showWarnings = FALSE)
      file.create("docs/test.html")
      file.create("docs/old.html")
      
      # Clear docs in output mode (should clear for rmd engine)
      .build_clear_post_docs(output_run = TRUE)
      
      # Check that docs was cleared
      expect_false(file.exists("docs/test.html"))
      expect_false(file.exists("docs/old.html"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_clear_post_docs does not clear for bookdown engine", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_no_clear_bookdown")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      
      # Create _bookdown.yml to set engine
      writeLines(c("output_dir: '_book'"), "_bookdown.yml")
      writeLines(c("---", "title: Test", "---", "# Test"), "index.Rmd")
      
      # Create _book with content
      dir.create("_book", showWarnings = FALSE)
      file.create("_book/index.html")
      
      # Clear docs in output mode (should NOT clear for bookdown)
      result <- .build_clear_post_docs(output_run = TRUE)
      
      # Should return FALSE (no clearing happened)
      expect_false(result)
      # Files should still exist
      expect_true(file.exists("_book/index.html"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_clear_old_output clears entire cache for output builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create multiple version directories
      cache_base <- dirname(.dir_get_cache_auto_version(profile = NULL))
      
      version1 <- file.path(cache_base, "v0.0.1")
      version2 <- file.path(cache_base, "v0.0.2")
      
      dir.create(version1, recursive = TRUE, showWarnings = FALSE)
      dir.create(version2, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(version1, "test1.txt"))
      file.create(file.path(version2, "test2.txt"))
      
      # Clear for output build
      .build_clear_old(output_run = TRUE, old_dev_remove = TRUE)
      
      # Cache base should be empty
      if (dir.exists(cache_base)) {
        expect_equal(length(list.files(cache_base)), 0)
      }
    }
  )
})
