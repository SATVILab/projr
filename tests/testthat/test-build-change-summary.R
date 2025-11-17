test_that(".build_change_summary_get_previous_version works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # No manifest yet - should return NULL
      prev <- .build_change_summary_get_previous_version()
      expect_null(prev)
      
      # Create first version
      projr_version_set("0.0.1")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Still only one version - should return NULL
      prev <- .build_change_summary_get_previous_version()
      expect_null(prev)
      
      # Create second version
      projr_version_set("0.0.2")
      .build_manifest_post(TRUE)
      
      # Now should return 0.0.1
      prev <- .build_change_summary_get_previous_version()
      expect_identical(prev, "0.0.1")
      
      # Create third version
      projr_version_set("0.0.3")
      .build_manifest_post(TRUE)
      
      # Should return 0.0.2 (second most recent)
      prev <- .build_change_summary_get_previous_version()
      expect_identical(prev, "0.0.2")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get returns NULL for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Dev build (output_run = FALSE) should return NULL
      summary <- .build_change_summary_get(output_run = FALSE)
      expect_null(summary)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get returns NULL for first build", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("0.0.1")
      
      # First build - no previous version
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_null(summary)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get detects added files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version - no output files
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      # Second version - add output files
      projr_version_set("0.0.2")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Should detect added files
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_true(is.character(summary))
      expect_true(length(summary) > 0)
      expect_true(any(grepl("added", summary, ignore.case = TRUE)))
      expect_true(any(grepl("output", summary, ignore.case = TRUE)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get detects modified files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version with output
      projr_version_set("0.0.1")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Modify a file
      output_files <- list.files(projr_path_get_dir("output", safe = FALSE), full.names = TRUE)
      if (length(output_files) > 0) {
        writeLines("modified content", output_files[1])
      }
      
      # Second version
      projr_version_set("0.0.2")
      .build_manifest_post(TRUE)
      
      # Should detect modified files
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_true(is.character(summary))
      expect_true(any(grepl("modified", summary, ignore.case = TRUE)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get detects removed files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version with output
      projr_version_set("0.0.1")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Remove files
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      unlink(output_dir, recursive = TRUE)
      dir.create(output_dir, recursive = TRUE)
      
      # Second version
      projr_version_set("0.0.2")
      .build_manifest_post(TRUE)
      
      # Should detect removed files
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_true(is.character(summary))
      expect_true(any(grepl("removed", summary, ignore.case = TRUE)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get shows file names for <10 changes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version - empty
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      # Second version - add 3 files (< 10)
      projr_version_set("0.0.2")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("content1", file.path(output_dir, "file1.txt"))
      writeLines("content2", file.path(output_dir, "file2.txt"))
      writeLines("content3", file.path(output_dir, "file3.txt"))
      .build_manifest_post(TRUE)
      
      # Should show individual file names
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_true(any(grepl("file1.txt", summary)))
      expect_true(any(grepl("file2.txt", summary)))
      expect_true(any(grepl("file3.txt", summary)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get shows counts for >=10 changes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version - empty
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      # Second version - add 12 files (>= 10)
      projr_version_set("0.0.2")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      for (i in 1:12) {
        writeLines(paste0("content", i), file.path(output_dir, paste0("file", i, ".txt")))
      }
      .build_manifest_post(TRUE)
      
      # Should show counts but not all individual file names
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_true(any(grepl("12 added", summary)))
      # Should not list all files individually
      expect_false(any(grepl("Added:.*file1.txt.*file2.txt.*file3.txt", summary)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get handles no changes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version with output
      projr_version_set("0.0.1")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Second version - no changes
      projr_version_set("0.0.2")
      .build_manifest_post(TRUE)
      
      # Should indicate no changes
      summary <- .build_change_summary_get(output_run = TRUE)
      expect_true(is.character(summary))
      expect_true(any(grepl("No changes|unchanged", summary, ignore.case = TRUE)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_format_section formats correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Create test manifest data
      projr_version_set("0.0.1")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      projr_version_set("0.0.2")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("new content", file.path(output_dir, "newfile.txt"))
      .build_manifest_post(TRUE)
      
      manifest_all <- .manifest_read_project()
      
      # Test formatting
      section <- .build_change_summary_format_section(
        "Outputs", "output", manifest_all, "0.0.1", "0.0.2"
      )
      
      expect_true(is.character(section))
      expect_true(any(grepl("\\*\\*Outputs Changes", section)))
      expect_true(any(grepl("output", section)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_get_debug returns correct structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test with no previous version
      result <- .build_change_summary_get_debug(output_run = TRUE)
      expect_true(is.list(result))
      expect_true("has_changes" %in% names(result))
      expect_true("message" %in% names(result))
      expect_false(result$has_changes)
      expect_null(result$message)
      
      # Test with changes
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      projr_version_set("0.0.2")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      result <- .build_change_summary_get_debug(output_run = TRUE)
      expect_true(result$has_changes)
      expect_true(is.character(result$message))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".buildlog_get_change_summary integrates correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First build - should return empty
      result <- .buildlog_get_change_summary("patch")
      expect_identical(length(result), 0L)
      
      # Create versions with changes
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      projr_version_set("0.0.2")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Should return change summary
      result <- .buildlog_get_change_summary("patch")
      expect_true(is.character(result))
      expect_true(length(result) > 0)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_change_summary_display works at debug level", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Should not error even with no changes
      expect_error(
        .build_change_summary_display("patch", output_level = "debug"),
        NA
      )
      
      # Create versions
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      projr_version_set("0.0.2")
      .test_setup_content("output", safe = FALSE)
      .build_manifest_post(TRUE)
      
      # Should work with changes
      expect_error(
        .build_change_summary_display("patch", output_level = "debug"),
        NA
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})
