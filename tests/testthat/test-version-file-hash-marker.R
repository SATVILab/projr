test_that(".version_file_mark_labels_stale marks labels with hash", {
  skip_if(.is_test_select())
  
  # Test with no labels
  version_file <- character(0L)
  result <- .version_file_mark_labels_stale(version_file)
  expect_identical(result, character(0L))
  
  # Test with one label
  version_file <- c("raw-data: v0.3.0")
  result <- .version_file_mark_labels_stale(version_file)
  expect_identical(result, c("raw-data: v0.3.0#"))
  
  # Test with multiple labels
  version_file <- c("raw-data: v0.3.0", "cache: v0.2.0")
  result <- .version_file_mark_labels_stale(version_file)
  expect_identical(result, c("raw-data: v0.3.0#", "cache: v0.2.0#"))
  
  # Test with label already having asterisk (don't add hash)
  version_file <- c("raw-data: v0.3.0*")
  result <- .version_file_mark_labels_stale(version_file)
  expect_identical(result, c("raw-data: v0.3.0*"))
  
  # Test with label already having hash (don't add another)
  version_file <- c("raw-data: v0.3.0#")
  result <- .version_file_mark_labels_stale(version_file)
  expect_identical(result, c("raw-data: v0.3.0#"))
  
  # Test with mixed markers
  version_file <- c("raw-data: v0.3.0", "cache: v0.2.0*", "output: v0.1.0#")
  result <- .version_file_mark_labels_stale(version_file)
  expect_identical(result, c("raw-data: v0.3.0#", "cache: v0.2.0*", "output: v0.1.0#"))
})

test_that(".version_file_update_project_version marks labels stale when version changes", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Start with empty version file
      version_file <- character(0L)
      result <- .version_file_update_project_version(version_file)
      expect_identical(result[[1]], paste0("Project: ", projr_version_get()))
      expect_identical(length(result), 1L)
      
      # Add a label version, then update project version (no change)
      projr_version_set("0.0.1")
      version_file <- c("Project: 0.0.1", "raw-data: v0.0.1")
      
      result <- .version_file_update_project_version(version_file)
      # Should not add hash since version didn't change
      expect_identical(result, c("Project: 0.0.1", "raw-data: v0.0.1"))
      
      # Now bump project version and update
      projr_version_set("0.0.2")
      result <- .version_file_update_project_version(version_file)
      # Should add hash to raw-data since project version changed
      expect_identical(result[[1]], "Project: 0.0.2")
      expect_identical(result[[2]], "raw-data: v0.0.1#")
      
      # Multiple labels with version change
      projr_version_set("0.0.3")
      version_file <- c("Project: 0.0.2", "raw-data: v0.0.2", "cache: v0.0.1")
      result <- .version_file_update_project_version(version_file)
      expect_identical(result[[1]], "Project: 0.0.3")
      expect_identical(result[[2]], "raw-data: v0.0.2#")
      expect_identical(result[[3]], "cache: v0.0.1#")
      
      # Label with asterisk should keep it when adding hash (but only add hash, not replace)
      projr_version_set("0.0.4")
      version_file <- c("Project: 0.0.3", "raw-data: v0.0.3*")
      result <- .version_file_update_project_version(version_file)
      expect_identical(result[[1]], "Project: 0.0.4")
      expect_identical(result[[2]], "raw-data: v0.0.3*")
    }
  )
})

test_that(".version_file_update_label_version removes hash marker", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.5")
      
      # Add new label (no hash to remove)
      version_file <- c("Project: 0.0.5")
      result <- .version_file_update_label_version(version_file, "raw-data", FALSE)
      expect_identical(result[[2]], "raw-data: 0.0.5")
      
      # Update existing label with hash (should remove hash)
      version_file <- c("Project: 0.0.5", "raw-data: v0.0.3#")
      result <- .version_file_update_label_version(version_file, "raw-data", FALSE)
      expect_identical(result[[2]], "raw-data: 0.0.5")
      expect_false(grepl("#", result[[2]]))
      
      # Update with asterisk should not have hash
      version_file <- c("Project: 0.0.5", "raw-data: v0.0.3#")
      result <- .version_file_update_label_version(version_file, "raw-data", TRUE)
      expect_identical(result[[2]], "raw-data: 0.0.5*")
      expect_false(grepl("#", result[[2]]))
    }
  )
})

test_that(".remote_get_version_latest_label_non_project_file_extract handles hash marker", {
  skip_if(.is_test_select())

  # Test extracting version without markers
  version_file <- c("Project: v0.5.0", "raw-data: v0.3.0")
  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "raw-data")
  expect_identical(result, "0.3.0")

  # Test extracting version with hash marker
  version_file <- c("Project: v0.5.0", "raw-data: v0.3.0#")
  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "raw-data")
  expect_identical(result, "0.3.0")

  # Test extracting version with asterisk marker
  version_file <- c("Project: v0.5.0", "raw-data: v0.3.0*")
  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "raw-data")
  expect_identical(result, "0.3.0")

  # Test extracting version with hash marker only (should still extract correctly)
  version_file <- c("Project: v0.5.0", "raw-data: v0.3.0#")
  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "raw-data")
  expect_identical(result, "0.3.0")

  # Test with multiple labels, extract specific one
  version_file <- c("Project: v0.5.0", "raw-data: v0.3.0#", "cache: v0.2.0", "output: v0.4.0*")
  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "cache")
  expect_identical(result, "0.2.0")

  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "raw-data")
  expect_identical(result, "0.3.0")

  result <- .remote_get_version_latest_label_non_project_file_extract(version_file, "output")
  expect_identical(result, "0.4.0")
})
