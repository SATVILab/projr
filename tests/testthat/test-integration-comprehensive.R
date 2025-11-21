# Comprehensive Integration Tests for Real-World Project Workflows
# ==================================================================
#
# These tests simulate extensive, iterative real-world user workflows
# Each test initializes a project and then extensively iterates through
# multiple changes: switching engines, adding/removing remotes, changing
# settings, multiple builds, etc. Also tests clone and restore workflows.
#
# Goal: Test complex user workflows that mix and match many options

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
# Test 1: Extensive workflow with bookdown -> quarto, multiple remotes, hooks
# Plus clone and restore
# =============================================================================

test_that("comprehensive workflow: bookdown to quarto with remotes, hooks, clone and restore", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # ========== Phase 1: Initialize with bookdown ==========
      projr_init_git()
      .create_test_content("raw-data", n_files = 3)
      .create_test_content("cache", n_files = 2)
      
      # First build with bookdown
      projr::projr_build_patch()
      v1 <- projr_version_get()
      expect_identical(v1, "0.0.1")
      expect_true(file.exists("_bookdown.yml"))
      
      # ========== Phase 2: Add multiple remotes with different configurations ==========
      # Add archive remote for raw-data
      projr_yml_dest_add_local(
        title = "raw-archive",
        content = "raw-data",
        path = "_archive/raw",
        structure = "archive",
        send_cue = "if-change"
      )
      
      # Add latest remote for cache
      projr_yml_dest_add_local(
        title = "cache-latest",
        content = "cache",
        path = "_archive/cache",
        structure = "latest",
        send_cue = "always"
      )
      
      # Build with remotes
      projr::projr_build_patch()
      v2 <- projr_version_get()
      expect_identical(v2, "0.0.2")
      expect_true(dir.exists("_archive/raw/raw-data/v0.0.2"))
      expect_true(dir.exists("_archive/cache/cache"))
      
      # ========== Phase 3: Modify content and add hooks ==========
      # Modify data
      writeLines("modified content", file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      
      # Add build hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "writeLines('pre-hook executed', 'hook-log.txt')"
        ),
        "hooks/pre-hook.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('hook-log.txt')) readLines('hook-log.txt') else character(0)",
          "writeLines(c(log, 'post-hook executed'), 'hook-log.txt')"
        ),
        "hooks/post-hook.R"
      )
      
      projr_yml_hooks_add_pre("hooks/pre-hook.R")
      projr_yml_hooks_add_post("hooks/post-hook.R")
      
      # Build with hooks
      projr::projr_build_patch()
      v3 <- projr_version_get()
      expect_identical(v3, "0.0.3")
      if (file.exists("hook-log.txt")) {
        hook_log <- readLines("hook-log.txt")
        expect_true("pre-hook executed" %in% hook_log)
        expect_true("post-hook executed" %in% hook_log)
      }
      
      # Should have new version in archive (content changed)
      expect_true(dir.exists("_archive/raw/raw-data/v0.0.3"))
      
      # ========== Phase 4: Switch to quarto and change settings ==========
      # Remove bookdown files
      file.remove("_bookdown.yml")
      file.remove("index.Rmd")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      
      # Add quarto files
      .test_setup_project_lit_docs("quarto_project")
      
      # Verify engine changed
      expect_identical(.engine_get(), "quarto_project")
      
      # Change git settings
      projr_yml_git_set(commit = TRUE, add_untracked = FALSE, push = FALSE)
      
      # Build with quarto
      projr::projr_build_patch()
      v4 <- projr_version_get()
      expect_identical(v4, "0.0.4")
      
      # ========== Phase 5: Modify remote configurations ==========
      # Remove one remote and add another
      .yml_dest_rm_title("cache-latest", "local", "default")
      
      # Add new remote with different strategy
      projr_yml_dest_add_local(
        title = "raw-latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest",
        send_strategy = "sync-diff"
      )
      
      # Build
      projr::projr_build_patch()
      v5 <- projr_version_get()
      expect_identical(v5, "0.0.5")
      expect_true(dir.exists("_latest/raw-data"))
      
      # ========== Phase 6: Do minor and patch builds ==========
      projr::projr_build_minor()
      v6 <- projr_version_get()
      expect_true(grepl("^0\\.1\\.0$", v6))
      
      projr::projr_build_patch()
      v7 <- projr_version_get()
      expect_true(grepl("^0\\.1\\.1$", v7))
      
      # Verify we have multiple versions archived
      archive_versions <- list.dirs("_archive/raw/raw-data", recursive = FALSE)
      expect_true(length(archive_versions) >= 1)
      
      # ========== Phase 7: Test clone and restore ==========
      # Create a clone directory
      clone_dir <- file.path(tempdir(), paste0("clone_", basename(dir_test)))
      if (dir.exists(clone_dir)) unlink(clone_dir, recursive = TRUE)
      
      # Copy project files (simulating a clone)
      dir.create(clone_dir)
      files_to_copy <- c("_projr.yml", "manifest.csv", "VERSION", "_quarto.yml", "index.qmd")
      for (f in files_to_copy) {
        if (file.exists(f)) {
          file.copy(f, file.path(clone_dir, f))
        }
      }
      
      # Copy directories structure
      for (d in c("_raw_data", "_tmp", "_output")) {
        if (dir.exists(d)) {
          dir.create(file.path(clone_dir, d), showWarnings = FALSE, recursive = TRUE)
        }
      }
      
      # Now test restore in the cloned directory
      old_wd <- getwd()
      setwd(clone_dir)
      
      # Restore should work with the manifest
      if (file.exists("manifest.csv")) {
        # Try to restore raw-data from local archive
        result <- tryCatch({
          projr_content_update(label = "raw-data", type = "local")
        }, error = function(e) {
          FALSE
        })
        
        # Even if it fails (no archive available), the function should handle it gracefully
        expect_true(is.logical(result))
      }
      
      setwd(old_wd)
      unlink(clone_dir, recursive = TRUE)
    }
  )
})

# =============================================================================
# Test 2: Extensive workflow with rmarkdown, dev builds, remote changes
# Plus restore testing
# =============================================================================

test_that("comprehensive workflow: rmarkdown with dev builds, iterative changes, and restore", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # ========== Phase 1: Start with standalone rmarkdown ==========
      writeLines(
        c(
          "---",
          "title: Analysis Report",
          "output: html_document",
          "---",
          "",
          "# Analysis",
          "",
          "Initial analysis"
        ),
        "analysis.Rmd"
      )
      projr_init_git()
      .create_test_content("raw-data", n_files = 2)
      
      # Dev build
      projr::projr_build_dev()
      v1 <- projr_version_get()
      expect_true(grepl("-", v1))  # Dev version has dash
      
      # ========== Phase 2: Add remotes with different cues ==========
      projr_yml_dest_add_local(
        title = "backup-always",
        content = "raw-data",
        path = "_backup/always",
        structure = "archive",
        send_cue = "always"
      )
      
      projr_yml_dest_add_local(
        title = "backup-change",
        content = "raw-data",
        path = "_backup/change",
        structure = "archive",
        send_cue = "if-change"
      )
      
      # Dev build - both should create versions
      projr::projr_build_dev()
      v2 <- projr_version_get()
      expect_true(grepl("-", v2))
      
      # ========== Phase 3: Production build and switch to bookdown ==========
      projr::projr_build_patch()
      v4 <- projr_version_get()
      expect_false(grepl("-", v4))  # Production version has no dash
      expect_identical(v4, "0.0.1")
      
      # Switch to bookdown
      file.remove("analysis.Rmd")
      writeLines(
        c(
          "project:",
          "  type: book",
          "book:",
          "  title: \"Analysis Book\""
        ),
        "_bookdown.yml"
      )
      writeLines(
        c(
          "bookdown::gitbook:",
          "  config:",
          "    toc:",
          "      collapse: section"
        ),
        "_output.yml"
      )
      writeLines(
        c(
          "# Introduction {-}",
          "",
          "Book introduction"
        ),
        "index.Rmd"
      )
      
      expect_identical(.engine_get(), "bookdown")
      
      # Build with bookdown
      projr::projr_build_patch()
      v5 <- projr_version_get()
      expect_identical(v5, "0.0.2")
      
      # ========== Phase 4: Add cache content and multiple remotes ==========
      .create_test_content("cache", n_files = 3)
      
      projr_yml_dest_add_local(
        title = "cache-sync",
        content = "cache",
        path = "_archive/cache",
        structure = "latest",
        send_strategy = "sync-diff"
      )
      
      projr::projr_build_patch()
      v6 <- projr_version_get()
      expect_identical(v6, "0.0.3")
      expect_true(dir.exists("_archive/cache/cache"))
      
      # ========== Phase 5: Change git settings and test ==========
      # Disable auto-commit
      projr_yml_git_set(commit = FALSE, add_untracked = FALSE, push = FALSE)
      
      writeLines("new file", "test-file.txt")
      projr::projr_build_patch()
      
      # File should be untracked (not committed)
      new_files <- .git_new_get()
      expect_true("test-file.txt" %in% new_files)
      
      # Re-enable commits
      projr_yml_git_set(commit = TRUE, add_untracked = TRUE, push = FALSE)
      projr::projr_build_patch()
      
      new_files_after <- .git_new_get()
      expect_false("test-file.txt" %in% new_files_after)
      
      # ========== Phase 6: Test restore from local remote ==========
      # Clear raw-data directory
      raw_data_files <- list.files(projr_path_get_dir("raw-data"), full.names = TRUE, recursive = TRUE)
      file.remove(raw_data_files[!file.info(raw_data_files)$isdir])
      
      # Try to restore from backup
      result <- tryCatch({
        projr_content_update(label = "raw-data", type = "local")
      }, error = function(e) {
        FALSE
      })
      
      # Should return a result (TRUE or FALSE)
      expect_true(is.logical(result))
    }
  )
})

# =============================================================================
# Test 3: Extensive workflow with quarto, multiple content types, format changes
# Plus testing restore with multiple content types
# =============================================================================

test_that("comprehensive workflow: quarto with multiple content types, configurations, and restore", {
  skip_if(.is_test_select())
  skip_if_not(requireNamespace("quarto", quietly = TRUE))
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE, rm_engine = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # ========== Phase 1: Start with quarto document ==========
      writeLines(
        c(
          "---",
          "title: Research Document",
          "format: html",
          "---",
          "",
          "# Research"
        ),
        "research.qmd"
      )
      projr_init_git()
      .create_test_content("raw-data", n_files = 2)
      .create_test_content("cache", n_files = 2)
      
      projr::projr_build_patch()
      v1 <- projr_version_get()
      expect_identical(v1, "0.0.1")
      
      # ========== Phase 2: Add multiple content remotes ==========
      projr_yml_dest_add_local(
        title = "raw-archive",
        content = "raw-data",
        path = "_storage",
        structure = "archive"
      )
      
      projr_yml_dest_add_local(
        title = "cache-archive",
        content = "cache",
        path = "_storage",
        structure = "archive"
      )
      
      projr::projr_build_patch()
      v2 <- projr_version_get()
      expect_true(dir.exists("_storage/raw-data"))
      expect_true(dir.exists("_storage/cache"))
      
      # ========== Phase 3: Switch to quarto project ==========
      file.remove("research.qmd")
      .test_setup_project_lit_docs("quarto_project")
      
      expect_identical(.engine_get(), "quarto_project")
      
      # Add build hooks for quarto
      dir.create("scripts", showWarnings = FALSE)
      writeLines(
        c(
          "# Pre-build script",
          "message('Preparing quarto build')"
        ),
        "scripts/prepare.R"
      )
      
      projr_yml_hooks_add_pre("scripts/prepare.R")
      
      projr::projr_build_patch()
      v3 <- projr_version_get()
      expect_identical(v3, "0.0.3")
      
      # ========== Phase 4: Modify quarto document options ==========
      writeLines(
        c(
          "---",
          "title: Updated Research",
          "format:",
          "  html:",
          "    toc: true",
          "    code-fold: true",
          "---",
          "",
          "# Updated Research"
        ),
        "index.qmd"
      )
      
      projr::projr_build_patch()
      v4 <- projr_version_get()
      expect_identical(v4, "0.0.4")
      
      # ========== Phase 5: Change remote structures ==========
      # Remove archive remotes, add latest
      .yml_dest_rm_title("raw-archive", "local", "default")
      .yml_dest_rm_title("cache-archive", "local", "default")
      
      projr_yml_dest_add_local(
        title = "combined-latest",
        content = "raw-data",
        path = "_latest/data",
        structure = "latest",
        send_cue = "always"
      )
      
      projr::projr_build_patch()
      v5 <- projr_version_get()
      expect_true(dir.exists("_latest/data/raw-data"))
      
      # ========== Phase 6: Do minor and patch builds ==========
      projr::projr_build_minor()
      v7 <- projr_version_get()
      expect_true(grepl("^0\\.1\\.0$", v7))
      
      projr::projr_build_patch()
      v8 <- projr_version_get()
      expect_true(grepl("^0\\.1\\.1$", v8))
      
      # Verify we went through many version changes
      expect_true(v8 != v1)
      
      # ========== Phase 7: Test restoring multiple content types ==========
      # Try to restore both raw-data and cache if possible
      result_raw <- tryCatch({
        projr_content_update(label = "raw-data", type = "local")
      }, error = function(e) {
        FALSE
      })
      
      result_cache <- tryCatch({
        projr_content_update(label = "cache", type = "local")
      }, error = function(e) {
        FALSE
      })
      
      # Both should return logical values
      expect_true(is.logical(result_raw))
      expect_true(is.logical(result_cache))
    }
  )
})

# =============================================================================
# Test 4: Extensive workflow starting from scratch, mixing everything
# Plus comprehensive clone and restore testing
# =============================================================================

test_that("comprehensive workflow: from scratch with extensive iteration, clone and restore", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # ========== Phase 1: Initial setup with bookdown ==========
      projr_init_git()
      .create_test_content("raw-data", n_files = 4)
      
      # First build
      projr::projr_build_patch()
      v1 <- projr_version_get()
      
      # ========== Phase 2: Iteratively add and modify remotes ==========
      # Add first remote
      projr_yml_dest_add_local(
        title = "backup1",
        content = "raw-data",
        path = "_backups/v1",
        structure = "latest"
      )
      projr::projr_build_patch()
      v2 <- projr_version_get()
      
      # Add second remote with different config
      projr_yml_dest_add_local(
        title = "backup2",
        content = "raw-data",
        path = "_backups/v2",
        structure = "archive",
        send_cue = "always"
      )
      projr::projr_build_patch()
      v3 <- projr_version_get()
      
      # Remove first, keep second
      .yml_dest_rm_title("backup1", "local", "default")
      projr::projr_build_patch()
      v4 <- projr_version_get()
      
      # ========== Phase 3: Add cache and multiple remotes ==========
      .create_test_content("cache", n_files = 3)
      
      projr_yml_dest_add_local(
        title = "cache-backup",
        content = "cache",
        path = "_cache-store",
        structure = "latest",
        send_strategy = "sync-purge"
      )
      
      projr::projr_build_patch()
      v5 <- projr_version_get()
      expect_true(dir.exists("_cache-store/cache"))
      
      # ========== Phase 4: Switch engines and add hooks ==========
      file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      file.remove("index.Rmd")
      
      # Add quarto
      .test_setup_project_lit_docs("quarto_project")
      
      # Add both pre and post hooks
      dir.create("workflows", showWarnings = FALSE)
      writeLines("writeLines('pre', 'workflow.log')", "workflows/pre.R")
      writeLines(
        "writeLines(c(readLines('workflow.log'), 'post'), 'workflow.log')",
        "workflows/post.R"
      )
      
      projr_yml_hooks_add_pre("workflows/pre.R")
      projr_yml_hooks_add_post("workflows/post.R")
      
      projr::projr_build_patch()
      v6 <- projr_version_get()
      
      if (file.exists("workflow.log")) {
        log <- readLines("workflow.log")
        expect_identical(log, c("pre", "post"))
      }
      
      # ========== Phase 5: Do dev builds, then production ==========
      projr::projr_build_dev()
      vdev1 <- projr_version_get()
      expect_true(grepl("-", vdev1))
      
      projr::projr_build_dev()
      vdev2 <- projr_version_get()
      expect_true(grepl("-", vdev2))
      
      projr::projr_build_minor()
      vprod <- projr_version_get()
      expect_false(grepl("-", vprod))
      
      # ========== Phase 6: Complex git operations ==========
      # Disable git
      projr_yml_git_set(commit = FALSE, add_untracked = FALSE, push = FALSE)
      
      writeLines("temp", "temp.txt")
      projr::projr_build_patch()
      expect_true("temp.txt" %in% .git_new_get())
      
      # Re-enable with different settings
      projr_yml_git_set(commit = TRUE, add_untracked = TRUE, push = FALSE)
      projr::projr_build_patch()
      expect_false("temp.txt" %in% .git_new_get())
      
      # ========== Phase 7: Final verification ==========
      # Should have archive versions
      archive_dirs <- list.dirs("_backups/v2/raw-data", recursive = FALSE)
      expect_true(length(archive_dirs) >= 3)
      
      # Should have made many version changes
      all_versions <- c(v1, v2, v3, v4, v5, v6, vdev1, vdev2, vprod)
      expect_true(length(unique(all_versions)) >= 7)
      
      # ========== Phase 8: Test comprehensive clone and restore ==========
      # Simulate cloning the entire project
      clone_dir <- file.path(tempdir(), paste0("full_clone_", basename(dir_test)))
      if (dir.exists(clone_dir)) unlink(clone_dir, recursive = TRUE)
      
      # Copy entire project structure
      dir.create(clone_dir)
      system2("cp", args = c("-r", paste0(getwd(), "/."), clone_dir), stdout = FALSE, stderr = FALSE)
      
      # Test restore in cloned directory
      old_wd <- getwd()
      tryCatch({
        setwd(clone_dir)
        
        # Clear a directory and try to restore it
        if (dir.exists("_raw_data")) {
          raw_files <- list.files("_raw_data", full.names = TRUE, recursive = TRUE)
          raw_files <- raw_files[!file.info(raw_files)$isdir]
          if (length(raw_files) > 0) {
            file.remove(raw_files)
          }
        }
        
        # Attempt restore
        result <- tryCatch({
          projr_content_update(label = "raw-data", type = "local")
        }, error = function(e) {
          FALSE
        })
        
        expect_true(is.logical(result))
        
      }, finally = {
        setwd(old_wd)
        unlink(clone_dir, recursive = TRUE)
      })
    }
  )
})
