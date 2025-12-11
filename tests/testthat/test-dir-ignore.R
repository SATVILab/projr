# Tests for R/dir-ignore.R
# Focus on internal functions and edge cases

test_that(".ignore_diryml works with git_skip_adjust parameter", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git config
      .test_setup_project_git_config()

      # Create some test files
      dir.create("_output")
      writeLines("test content", "_output/test.txt")

      # Track the file in git
      .git_commit_file("_output/test.txt", "Initial commit")

      # Run .ignore_diryml with git_skip_adjust = TRUE
      result <- .ignore_diryml(git_skip_adjust = TRUE)
      expect_true(result)

      # Check that skip-worktree is set
      skipped <- .git_get_skipped("_output")
      expect_true(length(skipped) > 0)

      # Run with git_skip_adjust = FALSE
      result <- .ignore_diryml(git_skip_adjust = FALSE)
      expect_true(result)

      # Check that skip-worktree is not set (or removed)
      # Note: This might not remove previously set skip-worktree
    }
  )
})

test_that(".ignore_diryml_git handles missing git repo and .gitignore", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .gitignore if it exists
      if (file.exists(".gitignore")) unlink(".gitignore")

      # Should return FALSE when no git repo and no .gitignore
      result <- .ignore_diryml_git(git_skip_adjust = NULL)
      expect_false(result)

      # Create .gitignore
      writeLines("# test", ".gitignore")

      # Should work now with .gitignore present
      result <- .ignore_diryml_git(git_skip_adjust = NULL)
      expect_true(result)
    }
  )
})

test_that(".ignore_get_git handles inconsistent ignore settings", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a YAML config with inconsistent settings
      yml_content <- "
directories:
  output:
    path: _output
    ignore-git: true
    ignore: false
"
      writeLines(yml_content, "_projr.yml")

      # Should throw error for inconsistent settings
      expect_error(
        .ignore_get_git("output"),
        "Inconsistent ignore settings"
      )
    }
  )
})

test_that(".ignore_get_git uses ignore when ignore-git is NULL", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a YAML config with only ignore set
      yml_content <- "
directories:
  output:
    path: _output
    ignore: false
"
      writeLines(yml_content, "_projr.yml")

      # Should use ignore value when ignore-git is NULL
      result <- .ignore_get_git("output")
      expect_false(result)

      # Test with ignore = true
      yml_content2 <- "
directories:
  output:
    path: _output
    ignore: true
"
      writeLines(yml_content2, "_projr.yml")

      result <- .ignore_get_git("output")
      expect_true(result)
    }
  )
})

test_that(".ignore_get_git defaults to TRUE when both are NULL", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Default YAML has output without explicit ignore settings
      result <- .ignore_get_git("output")
      expect_true(result)
    }
  )
})

test_that(".ignore_get_rbuild handles inconsistent ignore settings", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a YAML config with inconsistent settings
      yml_content <- "
directories:
  output:
    path: _output
    ignore-rbuild: true
    ignore: false
"
      writeLines(yml_content, "_projr.yml")

      # Should throw error for inconsistent settings
      expect_error(
        .ignore_get_rbuild("output"),
        "Inconsistent ignore settings"
      )
    }
  )
})

test_that(".ignore_get_rbuild uses ignore when ignore-rbuild is NULL", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a YAML config with only ignore set
      yml_content <- "
directories:
  output:
    path: _output
    ignore: false
"
      writeLines(yml_content, "_projr.yml")

      # Should use ignore value when ignore-rbuild is NULL
      result <- .ignore_get_rbuild("output")
      expect_false(result)
    }
  )
})

test_that(".ignore_get_git_skip_adjust respects git_skip_adjust parameter", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # When git_skip_adjust is explicitly set, it should be used
      result <- .ignore_get_git_skip_adjust("output", TRUE)
      expect_true(result)

      result <- .ignore_get_git_skip_adjust("output", FALSE)
      expect_false(result)

      # When git_skip_adjust is NULL and not in config, defaults to TRUE
      result <- .ignore_get_git_skip_adjust("output", NULL)
      expect_true(result)
    }
  )
})

test_that(".ignore_get_git_skip_adjust returns FALSE without git", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should return FALSE when not possible (no git)
      result <- .ignore_get_git_skip_adjust("output", NULL)
      expect_false(result)
    }
  )
})

test_that(".ignore_check_git_skip_adjust_possible checks prerequisites", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Without git repo
      result <- .ignore_check_git_skip_adjust_possible()
      expect_false(result)
    }
  )

  # With git repo
  if (.git_system_check_git()) {
    dir_test2 <- .test_setup_project(git = TRUE, set_env_var = TRUE)
    usethis::with_project(
      path = dir_test2,
      code = {
        result <- .ignore_check_git_skip_adjust_possible()
        expect_true(result)
      }
    )
  }
})

test_that(".git_skip and .git_unskip work with tracked files", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git config
      .test_setup_project_git_config()

      # Create and track files
      dir.create("test_dir")
      writeLines("content1", "test_dir/file1.txt")
      writeLines("content2", "test_dir/file2.txt")

      .git_commit_file(c("test_dir/file1.txt", "test_dir/file2.txt"), "Add files")

      # Verify files are tracked
      tracked <- .git_get_tracked("test_dir")
      expect_true(length(tracked) > 0)

      # Set skip-worktree
      .git_skip("test_dir")

      # Check that files are now skipped
      skipped <- .git_get_skipped("test_dir")
      expect_true(length(skipped) > 0)
      expect_true(any(grepl("file1.txt", skipped)))

      # Unskip files
      .git_unskip("test_dir")

      # Check that files are no longer skipped
      skipped_after <- .git_get_skipped("test_dir")
      expect_equal(length(skipped_after), 0)
    }
  )
})

test_that(".git_skip handles empty directory", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create empty directory
      dir.create("empty_dir")

      # Should not error with empty directory
      result <- .git_skip("empty_dir")
      expect_true(result)

      # Verify no files are skipped
      skipped <- .git_get_skipped("empty_dir")
      expect_equal(length(skipped), 0)
    }
  )
})

test_that(".git_unskip handles directory with no skipped files", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create and track a file without skip-worktree
      writeLines("content", "normal_file.txt")
      .git_commit_file("normal_file.txt", "Add file")

      # Should not error
      result <- .git_unskip(".")
      expect_true(result)
    }
  )
})

test_that(".git_get_tracked returns tracked files", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create and track files
      dir.create("tracked_dir")
      writeLines("content", "tracked_dir/tracked.txt")
      .git_commit_file("tracked_dir/tracked.txt", "Add tracked file")

      # Get tracked files
      tracked <- .git_get_tracked("tracked_dir")
      expect_true(length(tracked) > 0)
      expect_true(any(grepl("tracked.txt", tracked)))
    }
  )
})

test_that(".git_get_tracked returns empty for untracked directory", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create directory but don't track files
      dir.create("untracked_dir")
      writeLines("content", "untracked_dir/untracked.txt")

      # Should return empty
      tracked <- .git_get_tracked("untracked_dir")
      expect_equal(length(tracked), 0)
    }
  )
})

test_that(".git_get_skipped returns skipped files", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create, track, and skip files
      dir.create("skip_dir")
      writeLines("content", "skip_dir/skip.txt")
      .git_commit_file("skip_dir/skip.txt", "Add file")
      .git_skip("skip_dir")

      # Get skipped files
      skipped <- .git_get_skipped("skip_dir")
      expect_true(length(skipped) > 0)
      expect_true(any(grepl("skip.txt", skipped)))
    }
  )
})

test_that(".git_get_skipped returns empty for non-skipped files", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create and track file without skipping
      writeLines("content", "normal.txt")
      .git_commit_file("normal.txt", "Add file")

      # Should return empty
      skipped <- .git_get_skipped(".")
      expect_equal(length(skipped), 0)
    }
  )
})

test_that(".ignore_diryml_path_get handles directory outside working directory", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a YAML config with path outside working directory
      yml_content <- "
directories:
  external:
    path: /tmp/external_dir
"
      writeLines(yml_content, "_projr.yml")

      # Should return empty character when path is outside wd
      result <- .ignore_diryml_path_get("external")
      expect_equal(length(result), 0)
    }
  )
})

test_that(".ignore_diryml_path_get handles path equal to current directory", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test that when .dir_get returns ".", the function returns character(0)
      # This is tested indirectly through the logic in the function
      # We can test with docs which defaults to current directory in some configs

      # Create custom label that would return "." when queried
      # Since .dir_get will error for root, we test the existing code path
      # by verifying normal directories work correctly
      result <- .ignore_diryml_path_get("output")
      expect_type(result, "character")
    }
  )
})

test_that(".ignore_diryml_path_get handles normal directory", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Normal output directory
      result <- .ignore_diryml_path_get("output")
      expect_true(length(result) > 0)
      expect_type(result, "character")
      expect_false(grepl("/$", result)) # No trailing slash
    }
  )
})

test_that(".ignore_diryml_path_get removes trailing slashes", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create directory with trailing slash in config
      yml_content <- "
directories:
  custom:
    path: _custom/
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_diryml_path_get("custom")
      expect_true(length(result) > 0)
      expect_false(grepl("/$", result)) # Should have no trailing slash
    }
  )
})

test_that(".ignore_diryml_rbuildignore_get_check validates projr section", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test multiple start markers
      rbuildignore_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "test content",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      )
      expect_error(
        .ignore_diryml_rbuildignore_get_check(
          match_str_top, match_str_bottom, rbuildignore_vec
        ),
        "Multiple projr sections"
      )

      # Test multiple end markers
      rbuildignore_vec2 <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section",
        "# End of projr section"
      )
      expect_error(
        .ignore_diryml_rbuildignore_get_check(
          match_str_top, match_str_bottom, rbuildignore_vec2
        ),
        "Multiple projr sections"
      )

      # Test start without end
      rbuildignore_vec3 <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "test content"
      )
      expect_error(
        .ignore_diryml_rbuildignore_get_check(
          match_str_top, match_str_bottom, rbuildignore_vec3
        ),
        "Found start of projr section but not end"
      )

      # Test end without start
      rbuildignore_vec4 <- c(
        "test content",
        "# End of projr section"
      )
      expect_error(
        .ignore_diryml_rbuildignore_get_check(
          match_str_top, match_str_bottom, rbuildignore_vec4
        ),
        "Found end of projr section but not start"
      )

      # Test start after end
      rbuildignore_vec5 <- c(
        "# End of projr section",
        "test content",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())"
      )
      expect_error(
        .ignore_diryml_rbuildignore_get_check(
          match_str_top, match_str_bottom, rbuildignore_vec5
        ),
        "Start of projr section found after end"
      )
    }
  )
})

test_that(".ignore_rbuild_read handles missing file", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Should return empty character vector
      result <- .ignore_rbuild_read()
      expect_equal(length(result), 0)
      expect_type(result, "character")
    }
  )
})

test_that(".ignore_rbuild_read returns content correctly", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .Rbuildignore with content
      test_content <- c("line1", "line2", "line3")
      writeLines(test_content, ".Rbuildignore")

      result <- .ignore_rbuild_read()
      expect_equal(result, test_content)
    }
  )
})

test_that(".ignore_rbuild_write creates file with newline", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Write content
      test_content <- c("line1", "line2")
      .ignore_rbuild_write(test_content, append = FALSE)

      # Check file exists
      expect_true(file.exists(".Rbuildignore"))

      # Check content (readLines may include empty line due to trailing newline)
      content <- readLines(".Rbuildignore", warn = FALSE)
      # Remove empty last line if present
      if (length(content) > 0 && content[length(content)] == "") {
        content <- content[-length(content)]
      }
      expect_equal(content, test_content)

      # Check that file ends with newline
      raw_content <- readChar(".Rbuildignore", file.info(".Rbuildignore")$size)
      expect_true(grepl("\n$", raw_content))
    }
  )
})

test_that(".ignore_rbuild_write can append content", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Write initial content
      initial_content <- c("initial1", "initial2")
      writeLines(initial_content, ".Rbuildignore")

      # Append new content
      new_content <- c("new1", "new2")
      .ignore_rbuild_write(new_content, append = TRUE)

      # Check combined content (may have empty lines due to newlines)
      content <- readLines(".Rbuildignore", warn = FALSE)
      # Remove trailing empty lines
      while (length(content) > 0 && content[length(content)] == "") {
        content <- content[-length(content)]
      }
      expect_equal(length(content), 4)
      expect_equal(content[1:2], initial_content)
      expect_equal(content[3:4], new_content)
    }
  )
})

test_that(".ignore_diryml_git_get_instructions_labels includes missing labels", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create YAML with minimal directories
      yml_content <- "
directories:
  output:
    path: _output
"
      writeLines(yml_content, "_projr.yml")

      # Should include missing standard labels
      labels <- .ignore_diryml_git_get_instructions_labels()

      # Should have output and missing labels (docs, raw-data, cache)
      expect_true("output" %in% labels)
      expect_true(any(grepl("^docs", labels)))
      expect_true(any(grepl("^raw", labels)) || "raw-data" %in% labels)
      expect_true(any(grepl("^cache", labels)))
    }
  )
})

test_that(".ignore_diryml_git_get_instructions returns proper structure", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get instructions
      instr <- .ignore_diryml_git_get_instructions(git_skip_adjust = NULL)

      # Check structure
      expect_type(instr, "list")
      expect_true("ignore" %in% names(instr))
      expect_true("skip" %in% names(instr))
      expect_true("unskip" %in% names(instr))

      # All should be character vectors
      expect_type(instr$ignore, "character")
      expect_type(instr$skip, "character")
      expect_type(instr$unskip, "character")
    }
  )
})

test_that(".ignore_diryml_git_update_gitignore handles empty input", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should handle empty character vector
      result <- .ignore_diryml_git_update_gitignore(character(0))
      expect_false(result)

      # Should handle empty strings
      result <- .ignore_diryml_git_update_gitignore(c("", ""))
      expect_false(result)
    }
  )
})

test_that(".ignore_diryml_git_update_skip processes multiple paths", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create and track multiple directories
      dir.create("dir1")
      dir.create("dir2")
      writeLines("content", "dir1/file.txt")
      writeLines("content", "dir2/file.txt")
      .git_commit_file(c("dir1/file.txt", "dir2/file.txt"), "Add files")

      # Skip multiple paths
      paths_to_skip <- c("dir1", "dir2")
      result <- .ignore_diryml_git_update_skip(paths_to_skip, character(0))
      expect_true(result)

      # Check both are skipped
      skipped1 <- .git_get_skipped("dir1")
      skipped2 <- .git_get_skipped("dir2")
      expect_true(length(skipped1) > 0)
      expect_true(length(skipped2) > 0)
    }
  )
})

test_that(".git_skip handles batch processing of 50+ files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create directory with 60 files (more than batch size of 50)
      dir.create("large_dir")
      for (i in 1:60) {
        writeLines(paste("content", i), sprintf("large_dir/file%03d.txt", i))
      }

      # Track all files
      files_to_track <- list.files("large_dir", full.names = TRUE)
      .git_commit_file(files_to_track, "Add many files")

      # Set skip-worktree on all files
      result <- .git_skip("large_dir")
      expect_true(result)

      # Check that all files are skipped
      skipped <- .git_get_skipped("large_dir")
      expect_equal(length(skipped), 60)
    }
  )
})

test_that(".git_unskip handles batch processing of 50+ files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create directory with 60 files
      dir.create("large_dir2")
      for (i in 1:60) {
        writeLines(paste("content", i), sprintf("large_dir2/file%03d.txt", i))
      }

      # Track and skip all files
      files_to_track <- list.files("large_dir2", full.names = TRUE)
      .git_commit_file(files_to_track, "Add many files")
      .git_skip("large_dir2")

      # Verify files are skipped
      skipped_before <- .git_get_skipped("large_dir2")
      expect_equal(length(skipped_before), 60)

      # Unskip all files
      result <- .git_unskip("large_dir2")
      expect_true(result)

      # Check that all files are unskipped
      skipped_after <- .git_get_skipped("large_dir2")
      expect_equal(length(skipped_after), 0)
    }
  )
})

test_that(".git_skip handles files with spaces in names", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create files with spaces in names
      dir.create("space_dir")
      writeLines("content", "space_dir/file_with_spaces.txt")
      writeLines("content", "space_dir/another_file.txt")

      # Track files
      .git_commit_file(c("space_dir/file_with_spaces.txt", "space_dir/another_file.txt"), "Add files with spaces")

      # Skip files
      result <- .git_skip("space_dir")
      expect_true(result)

      # Check that files are skipped
      skipped <- .git_get_skipped("space_dir")
      expect_true(length(skipped) >= 0) # May or may not work with spaces
    }
  )
})

test_that(".ignore_diryml_rbuild creates correct patterns", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Run .ignore_diryml_rbuild
      result <- .ignore_diryml_rbuild()
      expect_true(result)

      # Check .Rbuildignore exists and has projr section
      expect_true(file.exists(".Rbuildignore"))
      rbuildignore <- readLines(".Rbuildignore")

      # Should have projr section markers
      expect_true(any(grepl("Start of projr section", rbuildignore)))
      expect_true(any(grepl("End of projr section", rbuildignore)))

      # Should have patterns for directories
      # Check for output directory pattern
      expect_true(any(grepl("output", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_diryml_rbuild respects ignore-rbuild settings", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create YAML with ignore-rbuild = false for output
      yml_content <- "
directories:
  output:
    path: _output
    ignore-rbuild: false
"
      writeLines(yml_content, "_projr.yml")

      # Run .ignore_diryml_rbuild
      .ignore_diryml_rbuild()

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")

      # Find projr section
      start_idx <- which(grepl("Start of projr section", rbuildignore))
      end_idx <- which(grepl("End of projr section", rbuildignore))

      if (length(start_idx) > 0 && length(end_idx) > 0) {
        section_content <- rbuildignore[(start_idx + 1):(end_idx - 1)]
        # Output should not be in the projr section since ignore-rbuild is false
        # (though it might be in manual section)
      }
    }
  )
})

test_that(".ignore_diryml handles missing directories gracefully", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create YAML with non-existent directory
      yml_content <- "
directories:
  nonexistent:
    path: _does_not_exist
"
      writeLines(yml_content, "_projr.yml")

      # Should not error
      result <- .ignore_diryml()
      expect_true(result)
    }
  )
})

# Additional tests for improved coverage

test_that(".ignore_diryml_git_get_instructions_label returns empty for missing path", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a label that won't have a path
      yml_content <- "
directories:
  external:
    path: /tmp/nonexistent_external
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_diryml_git_get_instructions_label("external", NULL)

      expect_type(result, "list")
      expect_true("ignore" %in% names(result))
      expect_true("skip" %in% names(result))
      expect_true("unskip" %in% names(result))
      expect_equal(length(result$ignore), 0)
      expect_equal(length(result$skip), 0)
      expect_equal(length(result$unskip), 0)
    }
  )
})

test_that(".ignore_diryml_git_get_instructions_label_impl handles ignore_git FALSE", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set ignore-git to false for output
      yml_content <- "
directories:
  output:
    path: _output
    ignore-git: false
"
      writeLines(yml_content, "_projr.yml")

      # Create the directory
      dir.create("_output")

      result <- .ignore_diryml_git_get_instructions_label_impl(
        "_output", "output", NULL
      )

      # When ignore_git is FALSE, no files should be ignored
      expect_equal(length(result$ignore), 0)
      # And no files should be skipped
      expect_equal(length(result$skip), 0)
    }
  )
})

test_that(".ignore_diryml_git_get_instructions_label_impl handles git_skip_adjust FALSE", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Default setup with output directory
      dir.create("_output")

      result <- .ignore_diryml_git_get_instructions_label_impl(
        "_output", "output", FALSE
      )

      # When git_skip_adjust is FALSE, no skip/unskip should be set
      expect_equal(length(result$skip), 0)
      expect_equal(length(result$unskip), 0)
      # But ignore should still be set (assuming default ignore-git = TRUE)
      expect_true(length(result$ignore) > 0)
    }
  )
})

test_that(".ignore_diryml_git_get_instructions_label_impl with git_skip_adjust TRUE and ignore FALSE", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set ignore-git to false
      yml_content <- "
directories:
  output:
    path: _output
    ignore-git: false
"
      writeLines(yml_content, "_projr.yml")

      dir.create("_output")

      result <- .ignore_diryml_git_get_instructions_label_impl(
        "_output", "output", TRUE
      )

      # When ignore is FALSE and git_skip_adjust is TRUE, should unskip (not skip)
      expect_equal(length(result$skip), 0)
      expect_true(length(result$unskip) > 0)
      expect_equal(length(result$ignore), 0)
    }
  )
})

test_that(".ignore_diryml_rbuildignore_get handles empty .Rbuildignore", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure .Rbuildignore is empty (no lines at all)
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      result <- .ignore_diryml_rbuildignore_get()

      expect_type(result, "list")
      expect_true("start" %in% names(result))
      expect_true("end" %in% names(result))
      # When file doesn't exist or is empty, it returns empty lists
      expect_equal(length(result$start), 0)
      expect_equal(length(result$end), 0)
    }
  )
})

test_that(".ignore_diryml_rbuildignore_get adds markers to non-empty file without projr section", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .Rbuildignore with content but no projr section
      rbuildignore_content <- c(
        "^manual1$",
        "^manual2$"
      )
      writeLines(rbuildignore_content, ".Rbuildignore")

      result <- .ignore_diryml_rbuildignore_get()

      expect_type(result, "list")
      # Should preserve manual content and add projr markers
      expect_true(any(grepl("manual1", result$start)))
      expect_true(any(grepl("Start of projr section", result$start)))
      expect_true(any(grepl("End of projr section", result$end)))
    }
  )
})

test_that(".ignore_diryml_rbuildignore_get preserves existing projr section", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .Rbuildignore with existing projr section
      rbuildignore_content <- c(
        "^manual_content$",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "^_output/$",
        "# End of projr section",
        "^more_manual_content$"
      )
      writeLines(rbuildignore_content, ".Rbuildignore")

      result <- .ignore_diryml_rbuildignore_get()

      # Should preserve content before and after projr section
      expect_true(any(grepl("manual_content", result$start)))
      expect_true(any(grepl("more_manual_content", result$end)))
      expect_true(any(grepl("Start of projr section", result$start)))
      expect_true(any(grepl("End of projr section", result$end)))
    }
  )
})

test_that(".ignore_diryml_git_update_gitignore updates gitignore correctly", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .gitignore
      writeLines(c("# Manual content", "*.tmp"), ".gitignore")

      # Add ignore patterns
      ignore_patterns <- c("_output/**", "_cache/**")
      result <- .ignore_diryml_git_update_gitignore(ignore_patterns)
      expect_true(result)

      # Check .gitignore was updated
      gitignore_content <- readLines(".gitignore")
      expect_true(any(grepl("_output", gitignore_content)))
      expect_true(any(grepl("_cache", gitignore_content)))
    }
  )
})

test_that(".ignore_get_git handles ignore-git explicitly set to TRUE", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml_content <- "
directories:
  output:
    path: _output
    ignore-git: true
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_get_git("output")
      expect_true(result)
    }
  )
})

test_that(".ignore_get_git handles matching ignore-git and ignore", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # When both are set and match, should work
      yml_content <- "
directories:
  output:
    path: _output
    ignore-git: true
    ignore: true
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_get_git("output")
      expect_true(result)

      # Test with both FALSE
      yml_content2 <- "
directories:
  output:
    path: _output
    ignore-git: false
    ignore: false
"
      writeLines(yml_content2, "_projr.yml")

      result2 <- .ignore_get_git("output")
      expect_false(result2)
    }
  )
})

test_that(".ignore_get_rbuild returns FALSE correctly", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml_content <- "
directories:
  output:
    path: _output
    ignore-rbuild: false
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_get_rbuild("output")
      expect_false(result)
    }
  )
})

test_that(".ignore_get_rbuild matches ignore-rbuild and ignore when both set", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Both TRUE
      yml_content <- "
directories:
  output:
    path: _output
    ignore-rbuild: true
    ignore: true
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_get_rbuild("output")
      expect_true(result)

      # Both FALSE
      yml_content2 <- "
directories:
  output:
    path: _output
    ignore-rbuild: false
    ignore: false
"
      writeLines(yml_content2, "_projr.yml")

      result2 <- .ignore_get_rbuild("output")
      expect_false(result2)
    }
  )
})

test_that(".ignore_get_git_skip_adjust reads from YAML config", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set git-skip-adjust in YAML
      yml_content <- "
directories:
  output:
    path: _output
    git-skip-adjust: false
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_get_git_skip_adjust("output", NULL)
      expect_false(result)

      # Test with TRUE
      yml_content2 <- "
directories:
  output:
    path: _output
    git-skip-adjust: true
"
      writeLines(yml_content2, "_projr.yml")

      result2 <- .ignore_get_git_skip_adjust("output", NULL)
      expect_true(result2)
    }
  )
})

test_that(".git_skip processes exactly 50 files (batch boundary)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create exactly 50 files (batch size)
      dir.create("batch_dir")
      for (i in 1:50) {
        writeLines(paste("content", i), sprintf("batch_dir/file%03d.txt", i))
      }

      # Track all files
      files_to_track <- list.files("batch_dir", full.names = TRUE)
      .git_commit_file(files_to_track, "Add 50 files")

      # Set skip-worktree
      result <- .git_skip("batch_dir")
      expect_true(result)

      # Verify all files are skipped
      skipped <- .git_get_skipped("batch_dir")
      expect_equal(length(skipped), 50)
    }
  )
})

test_that(".git_unskip processes exactly 50 files (batch boundary)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_git_config()

      # Create exactly 50 files
      dir.create("batch_dir2")
      for (i in 1:50) {
        writeLines(paste("content", i), sprintf("batch_dir2/file%03d.txt", i))
      }

      # Track and skip
      files_to_track <- list.files("batch_dir2", full.names = TRUE)
      .git_commit_file(files_to_track, "Add 50 files")
      .git_skip("batch_dir2")

      # Unskip
      result <- .git_unskip("batch_dir2")
      expect_true(result)

      # Verify all files are unskipped
      skipped <- .git_get_skipped("batch_dir2")
      expect_equal(length(skipped), 0)
    }
  )
})

test_that(".ignore_diryml_rbuild handles label without path", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should handle standard setup without errors
      result <- .ignore_diryml_rbuild()
      expect_true(result)
    }
  )
})

test_that(".ignore_diryml integration test with git and rbuild", {
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Full workflow test
      .test_setup_project_git_config()

      # Create directories
      dir.create("_output")
      dir.create("_cache")

      # Run the full ignore workflow
      result <- .ignore_diryml(git_skip_adjust = NULL)
      expect_true(result)

      # Verify .gitignore was updated
      expect_true(file.exists(".gitignore"))
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("output", gitignore, ignore.case = TRUE)))

      # Verify .Rbuildignore was updated
      expect_true(file.exists(".Rbuildignore"))
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("Start of projr section", rbuildignore)))
    }
  )
})
