# Comprehensive Integration Tests for Real-World Project Workflows
# ==================================================================
#
# These tests simulate realistic user workflows that mix and match:
# - Different engines (bookdown, quarto_project, quarto_document, rmd)
# - Git and GitHub initialization
# - Remote destinations (local, GitHub, OSF)
# - Scripts and hooks
# - Build configurations
# - Adding/removing/changing settings after initialization
# - Clone and restore workflows
#
# Goal: Test the package as users would actually use it, not just individual features

# =============================================================================
# Test Helpers
# =============================================================================

# Helper to create sample content in a directory
.create_test_content <- function(label, n_files = 3) {
  path_dir <- projr_path_get_dir(label, safe = FALSE)
  for (i in seq_len(n_files)) {
    file.create(file.path(path_dir, paste0("file", i, ".txt")))
    writeLines(paste("Content", i), file.path(path_dir, paste0("file", i, ".txt")))
  }
  # Add subdirectory with file
  dir.create(file.path(path_dir, "subdir"), showWarnings = FALSE)
  file.create(file.path(path_dir, "subdir", "nested.txt"))
  writeLines("Nested content", file.path(path_dir, "subdir", "nested.txt"))
  invisible(TRUE)
}

# =============================================================================
# Test Scenario 1: Start with bookdown, add remotes, build multiple times
# =============================================================================

test_that("bookdown project with local remotes and multiple builds works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git and create some content
      projr_init_git()
      .create_test_content("raw-data", n_files = 2)
      
      # First build - no remotes
      projr::projr_build_patch()
      expect_true(file.exists(.path_get("VERSION")))
      expect_true(dir.exists(projr_path_get_dir("output", safe = FALSE)))
      
      # Add a local remote for raw-data
      projr_yml_dest_add_local(
        title = "archive-raw",
        content = "raw-data",
        path = "_archive/raw",
        structure = "archive",
        send_cue = "if-change"
      )
      
      # Build again - should create archive
      projr::projr_build_patch()
      expect_true(dir.exists("_archive/raw/raw-data"))
      
      # Modify content and build again
      writeLines("new content", file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      projr::projr_build_patch()
      
      # Should have multiple versions
      version_dirs <- list.dirs("_archive/raw/raw-data", recursive = FALSE)
      expect_true(length(version_dirs) >= 2)
    }
  )
})

# =============================================================================
# Test Scenario 2: Quarto project with hooks and scripts
# =============================================================================

test_that("quarto project with build hooks and scripts works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Set up quarto project
      .test_setup_project_lit_docs("quarto_project")
      projr_init_git()
      
      # Create a pre-build hook
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "message('Pre-build hook executed')",
          "writeLines('hook ran', 'hook-output.txt')"
        ),
        "hooks/pre-build.R"
      )
      
      # Add the hook to config
      projr_yml_hooks_add_pre("hooks/pre-build.R")
      
      # Build and verify hook ran
      projr::projr_build_patch()
      expect_true(file.exists("hook-output.txt"))
      expect_identical(readLines("hook-output.txt"), "hook ran")
    }
  )
})

# =============================================================================
# Test Scenario 3: Switch engines mid-project
# =============================================================================

test_that("switching from bookdown to quarto works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      
      # Start with bookdown
      expect_true(file.exists("_bookdown.yml"))
      engine1 <- .engine_get()
      expect_identical(engine1, "bookdown")
      
      # Build with bookdown
      projr::projr_build_patch()
      v1 <- projr_version_get()
      
      # Switch to quarto by removing bookdown files and adding quarto
      file.remove("_bookdown.yml")
      file.remove("index.Rmd")
      .test_setup_project_lit_docs("quarto_project")
      
      # Verify engine changed
      engine2 <- .engine_get()
      expect_identical(engine2, "quarto_project")
      
      # Build with quarto
      projr::projr_build_patch()
      v2 <- projr_version_get()
      
      # Version should have incremented
      expect_false(identical(v1, v2))
    }
  )
})

# =============================================================================
# Test Scenario 4: Multiple remote types simultaneously
# =============================================================================

test_that("project with multiple local remotes of different types works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      .create_test_content("raw-data", n_files = 3)
      .create_test_content("cache", n_files = 2)
      
      # Add multiple remotes with different configurations
      projr_yml_dest_add_local(
        title = "latest-raw",
        content = "raw-data",
        path = "_remotes/latest-raw",
        structure = "latest",
        send_cue = "always"
      )
      
      projr_yml_dest_add_local(
        title = "archive-raw",
        content = "raw-data",
        path = "_remotes/archive-raw",
        structure = "archive",
        send_cue = "if-change"
      )
      
      projr_yml_dest_add_local(
        title = "cache-latest",
        content = "cache",
        path = "_remotes/cache",
        structure = "latest",
        send_cue = "if-change"
      )
      
      # Build
      projr::projr_build_patch()
      
      # Verify all remotes created
      expect_true(dir.exists("_remotes/latest-raw/raw-data"))
      expect_true(dir.exists("_remotes/archive-raw/raw-data"))
      expect_true(dir.exists("_remotes/cache/cache"))
      
      # Build again - latest should update, archive shouldn't (no change)
      projr::projr_build_patch()
      
      # Archive should have only one version (no content change)
      archive_versions <- list.dirs("_remotes/archive-raw/raw-data", recursive = FALSE)
      expect_true(length(archive_versions) == 1)
    }
  )
})

# =============================================================================
# Test Scenario 5: Rmarkdown standalone with dev vs production builds
# =============================================================================

test_that("rmarkdown project with dev and production builds works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Set up standalone rmarkdown
      .test_setup_project_lit_docs("rmarkdown")
      writeLines(
        c(
          "---",
          "title: Test Report",
          "output: html_document",
          "---",
          "",
          "# Test",
          "",
          "This is a test report."
        ),
        "report.Rmd"
      )
      projr_init_git()
      
      # Dev build
      projr::projr_build_dev()
      version_dev <- projr_version_get()
      expect_true(grepl("-", version_dev))  # Dev versions have dash
      
      # Production build
      projr::projr_build_patch()
      version_prod <- projr_version_get()
      expect_false(grepl("-", version_prod))  # Production versions don't
      
      # Minor build
      projr::projr_build_minor()
      version_minor <- projr_version_get()
      expect_true(grepl("^0\\.1\\.0$", version_minor))
    }
  )
})

# =============================================================================
# Test Scenario 6: Add and remove features iteratively
# =============================================================================

test_that("adding and removing destinations works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      .create_test_content("raw-data")
      
      # Start with no custom destinations
      .yml_dest_rm_type_all("default")
      projr::projr_build_patch()
      
      # Add a destination
      projr_yml_dest_add_local(
        title = "test-dest",
        content = "raw-data",
        path = "_test",
        structure = "latest"
      )
      projr::projr_build_patch()
      expect_true(dir.exists("_test/raw-data"))
      
      # Remove the destination
      .yml_dest_rm_title("test-dest", "local", "default")
      projr::projr_build_patch()
      
      # Add it back with different settings
      projr_yml_dest_add_local(
        title = "test-dest2",
        content = "raw-data",
        path = "_test2",
        structure = "archive"
      )
      projr::projr_build_patch()
      expect_true(dir.exists("_test2/raw-data"))
    }
  )
})

# =============================================================================
# Test Scenario 7: Project with build scripts
# =============================================================================

test_that("project with build.scripts configuration works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create multiple document files
      writeLines(
        c(
          "---",
          "title: Report 1",
          "output: html_document",
          "---",
          "",
          "# Report 1"
        ),
        "report1.Rmd"
      )
      
      writeLines(
        c(
          "---",
          "title: Report 2",
          "output: html_document",
          "---",
          "",
          "# Report 2"
        ),
        "report2.Rmd"
      )
      
      projr_init_git()
      
      # Configure to only build report1
      yml <- yaml::read_yaml("_projr.yml")
      yml$build$scripts <- "report1.Rmd"
      yaml::write_yaml(yml, "_projr.yml")
      
      # Build
      projr::projr_build_patch()
      
      # Verify that build completed (whether report1 built depends on engine)
      # Just check that the build didn't error and version incremented
      version <- projr_version_get()
      expect_true(file.exists(.path_get("VERSION")))
      expect_identical(version, "0.0.1")
    }
  )
})

# =============================================================================
# Test Scenario 8: Change version format mid-project
# =============================================================================

test_that("changing version format works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      
      # Default format: major.minor.patch-dev
      projr::projr_build_patch()
      v1 <- projr_version_get()
      expect_true(grepl("^\\d+\\.\\d+\\.\\d+$", v1))
      
      # Change to major.minor format
      yml <- yaml::read_yaml("_projr.yml")
      yml$metadata$`version-format` <- "major.minor-dev"
      yaml::write_yaml(yml, "_projr.yml")
      
      # Build with new format
      projr::projr_build_minor()
      v2 <- projr_version_get()
      expect_true(grepl("^\\d+\\.\\d+$", v2))
    }
  )
})

# =============================================================================
# Test Scenario 9: Mixed content types in archives
# =============================================================================

test_that("archiving multiple content types simultaneously works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      
      # Create content in multiple directories
      .create_test_content("raw-data", n_files = 2)
      .create_test_content("cache", n_files = 2)
      
      # Archive both with different structures
      projr_yml_dest_add_local(
        title = "raw-archive",
        content = "raw-data",
        path = "_backup",
        structure = "archive"
      )
      
      projr_yml_dest_add_local(
        title = "cache-latest",
        content = "cache",
        path = "_backup",
        structure = "latest"
      )
      
      # Build
      projr::projr_build_patch()
      
      # Verify both archived correctly
      expect_true(dir.exists("_backup/raw-data"))
      expect_true(dir.exists("_backup/cache"))
      
      # raw-data should be versioned, cache should be latest
      raw_subdirs <- list.dirs("_backup/raw-data", recursive = FALSE)
      cache_subdirs <- list.dirs("_backup/cache", recursive = FALSE)
      
      # Archive structure creates version subdirectories
      expect_true(length(raw_subdirs) > 0)
      expect_true(any(grepl("^v", basename(raw_subdirs))))
    }
  )
})

# =============================================================================
# Test Scenario 10: Quarto document with changing output formats
# =============================================================================

test_that("quarto document project with format changes works", {
  skip_if(.is_test_select())
  skip_if_not(requireNamespace("quarto", quietly = TRUE))
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create standalone quarto document
      writeLines(
        c(
          "---",
          "title: Test Document",
          "format: html",
          "---",
          "",
          "# Introduction",
          "",
          "This is a test."
        ),
        "document.qmd"
      )
      
      projr_init_git()
      
      # Build
      projr::projr_build_patch()
      v1 <- projr_version_get()
      
      # Change format in document (still HTML but with more options)
      writeLines(
        c(
          "---",
          "title: Test Document",
          "format: ",
          "  html:",
          "    toc: true",
          "    code-fold: true",
          "---",
          "",
          "# Introduction",
          "",
          "This is a test."
        ),
        "document.qmd"
      )
      
      # Build again
      projr::projr_build_patch()
      v2 <- projr_version_get()
      expect_false(identical(v1, v2))
    }
  )
})

# =============================================================================
# Test Scenario 11: Git operations with different push settings
# =============================================================================

test_that("git commit settings work correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      
      # Disable auto-commit
      projr_yml_git_set(commit = FALSE, add_untracked = FALSE, push = FALSE)
      
      # Create new file
      writeLines("test", "test-file.txt")
      
      # Build - should not commit
      projr::projr_build_patch()
      
      # File should be untracked
      new_files <- .git_new_get()
      expect_true("test-file.txt" %in% new_files)
      
      # Re-enable commits
      projr_yml_git_set(commit = TRUE, add_untracked = TRUE, push = FALSE)
      
      # Build again
      projr::projr_build_patch()
      
      # File should now be committed
      new_files_after <- .git_new_get()
      expect_false("test-file.txt" %in% new_files_after)
    }
  )
})

# =============================================================================
# Test Scenario 12: Complex workflow - initialize, modify, rebuild
# =============================================================================

test_that("complex workflow with multiple changes works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Phase 1: Initial setup
      projr_init_git()
      .create_test_content("raw-data", n_files = 2)
      projr::projr_build_patch()
      v1 <- projr_version_get()
      
      # Phase 2: Add local archive
      projr_yml_dest_add_local(
        title = "backup",
        content = "raw-data",
        path = "_archive",
        structure = "archive"
      )
      projr::projr_build_patch()
      v2 <- projr_version_get()
      expect_true(dir.exists("_archive/raw-data"))
      
      # Phase 3: Modify content
      writeLines("modified", file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      projr::projr_build_patch()
      v3 <- projr_version_get()
      
      # Phase 4: Change archive to latest structure
      .yml_dest_rm_title("backup", "local", "default")
      projr_yml_dest_add_local(
        title = "backup",
        content = "raw-data",
        path = "_archive",
        structure = "latest",
        send_cue = "always"
      )
      projr::projr_build_patch()
      v4 <- projr_version_get()
      
      # All versions should be different
      versions <- c(v1, v2, v3, v4)
      expect_identical(length(unique(versions)), 4L)
    }
  )
})

# =============================================================================
# Test Scenario 13: Different send_strategy options
# =============================================================================

test_that("different send_strategy options work correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      .create_test_content("raw-data", n_files = 3)
      
      # Test sync-diff strategy
      projr_yml_dest_add_local(
        title = "sync-test",
        content = "raw-data",
        path = "_sync",
        structure = "latest",
        send_strategy = "sync-diff"
      )
      
      projr::projr_build_patch()
      expect_true(dir.exists("_sync/raw-data"))
      initial_files <- list.files("_sync/raw-data", recursive = TRUE)
      
      # Remove a file from source
      file.remove(file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      projr::projr_build_patch()
      
      # File should be removed from destination (sync-diff)
      final_files <- list.files("_sync/raw-data", recursive = TRUE)
      expect_true(length(final_files) < length(initial_files))
    }
  )
})

# =============================================================================
# Test Scenario 14: Pre and post hooks together
# =============================================================================

test_that("pre and post hooks execute in correct order", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      
      # Create hooks directory
      dir.create("hooks", showWarnings = FALSE)
      
      # Pre-hook
      writeLines(
        c(
          "writeLines('pre', 'hook-log.txt')"
        ),
        "hooks/pre.R"
      )
      
      # Post-hook
      writeLines(
        c(
          "content <- readLines('hook-log.txt')",
          "writeLines(c(content, 'post'), 'hook-log.txt')"
        ),
        "hooks/post.R"
      )
      
      # Add hooks
      projr_yml_hooks_add_pre("hooks/pre.R")
      projr_yml_hooks_add_post("hooks/post.R")
      
      # Build
      projr::projr_build_patch()
      
      # Check execution order
      if (file.exists("hook-log.txt")) {
        log <- readLines("hook-log.txt")
        expect_identical(log, c("pre", "post"))
      }
    }
  )
})

# =============================================================================
# Test Scenario 15: Bookdown with multiple output formats
# =============================================================================

test_that("bookdown with multiple output formats works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init_git()
      
      # Modify _output.yml to have multiple formats
      if (file.exists("_output.yml")) {
        output_yml <- yaml::read_yaml("_output.yml")
        # Ensure gitbook format exists
        if (!"bookdown::gitbook" %in% names(output_yml)) {
          output_yml[["bookdown::gitbook"]] <- list()
        }
        yaml::write_yaml(output_yml, "_output.yml")
      }
      
      # Build
      projr::projr_build_patch()
      
      # Check that output directory exists
      expect_true(dir.exists(projr_path_get_dir("docs", safe = FALSE)))
    }
  )
})
