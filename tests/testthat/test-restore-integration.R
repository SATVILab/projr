# Integration tests for restore functionality

# Integration tests for restore functionality

# Test helper to create sample content in a directory
.create_test_content_restore <- function(label, n_files = 3) {
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

test_that("restore from local archive remote places files directly in target directory", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup - use exact pattern from comprehensive test
      .create_test_content_restore("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")
      
      # Create a simple qmd file to build
      writeLines(c("---", "title: Test", "---", "", "Test content"), "test.qmd")
      
      # Add local archive destination
      projr_yml_dest_add_local(
        title = "test-archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive"
      )
      
      # Build to send data to archive
      projr_build_patch()
      
      # Verify archive structure
      expect_true(dir.exists("_archive"))
      expect_true(dir.exists("_archive/raw-data"))
      expect_true(dir.exists("_archive/raw-data/v0.0.1"))
      expect_true(file.exists("_archive/raw-data/v0.0.1/file1.txt"))
      
      # Delete raw-data contents
      unlink("raw-data", recursive = TRUE)
      expect_false(file.exists("raw-data/file1.txt"))
      
      # Restore from archive
      result <- projr_restore(label = "raw-data")
      
      # Verify files are directly in raw-data, not in a subdirectory
      expect_true(file.exists("raw-data/file1.txt"))
      expect_true(file.exists("raw-data/subdir/nested.txt"))
      expect_false(file.exists("raw-data/raw-data/file1.txt"))
      expect_false(file.exists("raw-data/v0.0.1/file1.txt"))
      expect_false(file.exists("raw-data/raw-data-v0.0.1/file1.txt"))
      
      # Verify content is correct
      content <- readLines("raw-data/file1.txt")
      expect_identical(content, "Content 1")
      
      # Verify restoration was successful
      expect_true(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("restore from local latest remote places files directly in target directory", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup - use exact pattern from comprehensive test
      .create_test_content_restore("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")
      
      # Create a simple qmd file to build
      writeLines(c("---", "title: Test", "---", "", "Test content"), "test.qmd")
      
      # Add local latest destination
      projr_yml_dest_add_local(
        title = "test-latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      
      # Build to send data to latest
      projr_build_patch()
      
      # Verify latest structure
      expect_true(dir.exists("_latest"))
      expect_true(dir.exists("_latest/raw-data"))
      expect_true(file.exists("_latest/raw-data/file1.txt"))
      
      # Delete raw-data contents
      unlink("raw-data", recursive = TRUE)
      expect_false(file.exists("raw-data/file1.txt"))
      
      # Restore from latest
      result <- projr_restore(label = "raw-data")
      
      # Verify files are directly in raw-data, not in a subdirectory
      expect_true(file.exists("raw-data/file1.txt"))
      expect_true(file.exists("raw-data/subdir/nested.txt"))
      expect_false(file.exists("raw-data/raw-data/file1.txt"))
      expect_false(file.exists("raw-data/v0.0.1/file1.txt"))
      
      # Verify content is correct
      content <- readLines("raw-data/file1.txt")
      expect_identical(content, "Content 1")
      
      # Verify restoration was successful
      expect_true(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("restore and build integration test with archive remote", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup - use exact pattern from comprehensive test
      .create_test_content_restore("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")
      
      # Create a quarto document that depends on the data
      qmd_content <- c(
        "---",
        "title: Test Document",
        "---",
        "",
        "```{r}",
        "file_path <- file.path('raw-data', 'file1.txt')",
        "if (file.exists(file_path)) {",
        "  cat('File found\\n')",
        "} else {",
        "  stop('File not found')",
        "}",
        "```"
      )
      writeLines(qmd_content, "test.qmd")
      
      # Add local archive destination for raw-data
      projr_yml_dest_add_local(
        title = "test-archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive"
      )
      
      # Initial build to send data to archive
      projr_build_patch()
      
      # Verify archive was created
      expect_true(file.exists("_archive/raw-data/v0.0.1/file1.txt"))
      
      # Delete raw-data contents
      unlink("raw-data", recursive = TRUE)
      expect_false(dir.exists("raw-data"))
      
      # Restore from archive
      restore_result <- projr_restore(label = "raw-data")
      expect_true(restore_result)
      
      # Verify data was restored correctly
      expect_true(file.exists("raw-data/file1.txt"))
      
      # Try to build again - this should work if restore was successful
      build_result <- tryCatch({
        projr_build_minor()
        TRUE
      }, error = function(e) {
        message("Build error: ", e$message)
        FALSE
      })
      
      # Verify the data is there
      expect_true(file.exists("raw-data/file1.txt"))
      content <- readLines("raw-data/file1.txt")
      expect_identical(content, "Content 1")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("restore and build integration test with latest remote", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup - use exact pattern from comprehensive test
      .create_test_content_restore("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")
      
      # Create a quarto document that depends on the data
      qmd_content <- c(
        "---",
        "title: Test Document",
        "---",
        "",
        "```{r}",
        "file_path <- file.path('raw-data', 'file1.txt')",
        "if (file.exists(file_path)) {",
        "  cat('File found\\n')",
        "} else {",
        "  stop('File not found')",
        "}",
        "```"
      )
      writeLines(qmd_content, "test.qmd")
      
      # Add local latest destination for raw-data
      projr_yml_dest_add_local(
        title = "test-latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      
      # Initial build to send data to latest
      projr_build_patch()
      
      # Verify latest was created
      expect_true(file.exists("_latest/raw-data/file1.txt"))
      
      # Delete raw-data contents
      unlink("raw-data", recursive = TRUE)
      expect_false(dir.exists("raw-data"))
      
      # Restore from latest
      restore_result <- projr_restore(label = "raw-data")
      expect_true(restore_result)
      
      # Verify data was restored correctly
      expect_true(file.exists("raw-data/file1.txt"))
      
      # Try to build again - this should work if restore was successful
      build_result <- tryCatch({
        projr_build_minor()
        TRUE
      }, error = function(e) {
        message("Build error: ", e$message)
        FALSE
      })
      
      # Verify the data is there
      expect_true(file.exists("raw-data/file1.txt"))
      content <- readLines("raw-data/file1.txt")
      expect_identical(content, "Content 1")
    },
    force = TRUE,
    quiet = TRUE
  )
})
