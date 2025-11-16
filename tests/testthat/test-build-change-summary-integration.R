# Integration test for build change summary feature
# Tests the full workflow from build to BUILDLOG

test_that("build change summary integrates correctly into full build workflow", {
  skip_if(.is_test_select())
  skip_if_not(nzchar(Sys.getenv("GITHUB_PAT")))
  
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First build with initial content
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("initial content", file.path(output_dir, "file1.txt"))
      .build_manifest_post(TRUE)
      
      # Create a basic buildlog entry for first version
      .buildlog_add(
        msg = "First build",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.1")),
        total_time = as.difftime(60, units = "secs")
      )
      
      # Second build with changes
      projr_version_set("0.0.2")
      writeLines("modified content", file.path(output_dir, "file1.txt"))
      writeLines("new content", file.path(output_dir, "file2.txt"))
      .build_manifest_post(TRUE)
      
      # Add buildlog entry for second version
      .buildlog_add(
        msg = "Second build with changes",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.2")),
        total_time = as.difftime(90, units = "secs")
      )
      
      # Read buildlog
      buildlog_content <- .buildlog_read()
      
      # Verify change summary is present
      expect_true(any(grepl("Inputs Changes", buildlog_content)))
      expect_true(any(grepl("Outputs Changes", buildlog_content)))
      expect_true(any(grepl("v0.0.1.*v0.0.2", buildlog_content)))
      
      # Verify specific changes are documented
      expect_true(any(grepl("1 added.*1 modified", buildlog_content)))
      expect_true(any(grepl("Added: file2.txt", buildlog_content)))
      expect_true(any(grepl("Modified: file1.txt", buildlog_content)))
      
      # Verify first version has no change summary (no previous version)
      # Use more specific pattern to match section headers only
      v1_section_start <- grep("^#### v0\\.0\\.1:", buildlog_content)[1]
      v2_section_start <- grep("^#### v0\\.0\\.2:", buildlog_content)[1]
      v1_content <- buildlog_content[v1_section_start:(v2_section_start - 1)]
      
      # First version should not have change summary
      expect_false(any(grepl("Inputs Changes", v1_content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("change summary handles mixed input and output changes", {
  skip_if(.is_test_select())
  
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version with both raw-data and output
      projr_version_set("0.0.1")
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      
      writeLines("raw data 1", file.path(raw_dir, "data1.csv"))
      writeLines("output 1", file.path(output_dir, "report1.html"))
      
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      
      # Second version with changes to both
      projr_version_set("0.0.2")
      
      # Add to raw-data
      writeLines("raw data 2", file.path(raw_dir, "data2.csv"))
      
      # Modify and add to output
      writeLines("modified output 1", file.path(output_dir, "report1.html"))
      writeLines("output 2", file.path(output_dir, "report2.html"))
      
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      
      # Get change summary
      summary <- .build_change_summary_get(output_run = TRUE)
      
      # Should have both input and output sections
      expect_true(any(grepl("Inputs Changes", summary)))
      expect_true(any(grepl("Outputs Changes", summary)))
      
      # Verify input changes
      expect_true(any(grepl("raw-data", summary)))
      expect_true(any(grepl("data2.csv", summary)))
      
      # Verify output changes
      expect_true(any(grepl("output", summary)))
      expect_true(any(grepl("report1.html", summary)))
      expect_true(any(grepl("report2.html", summary)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("change summary respects hashing configuration", {
  skip_if(.is_test_select())
  
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # First version
      projr_version_set("0.0.1")
      .build_manifest_post(TRUE)
      
      # Second version with output changes
      projr_version_set("0.0.2")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("content", file.path(output_dir, "file.txt"))
      .build_manifest_post(TRUE)
      
      # Get labels that are actually hashed
      labels_output <- .yml_dir_get_label_output(NULL)
      labels_output_hashed <- labels_output[vapply(
        labels_output, .yml_dir_get_hash_complete, logical(1), profile = NULL
      )]
      
      # Summary should only include hashed labels
      summary <- .build_change_summary_get(output_run = TRUE)
      
      if (length(labels_output_hashed) > 0) {
        expect_true(!is.null(summary))
        expect_true(any(grepl("Outputs Changes", summary)))
      }
    },
    force = TRUE,
    quiet = TRUE
  )
})
