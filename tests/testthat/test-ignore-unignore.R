# Tests for R/ignore-unignore.R
# Focus on increasing test coverage for unignore functionality

test_that("projr_unignore_manual handles empty strings", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create ignore files with projr section
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Test with only empty strings
      result <- projr_unignore_manual(c("", ""))
      expect_false(result)

      # Test with mix of empty and valid - function doesn't return invisible(TRUE)
      # It calls sub-functions that may return invisible(TRUE), but main function doesn't
      projr_unignore_manual(c("", "valid.txt", ""))

      # Check that only valid.txt was added
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!valid.txt", gitignore)))
    }
  )
})

test_that("projr_unignore_manual handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with numeric input
      result <- projr_unignore_manual(123)
      expect_false(result)

      # Test with NULL
      result <- projr_unignore_manual(NULL)
      expect_false(result)

      # Test with list
      result <- projr_unignore_manual(list("test"))
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual distinguishes files and directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Create projr sections
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Create a file and directory
      file.create("myfile.txt")
      dir.create("mydir")

      # Unignore both
      projr_unignore_manual(c("myfile.txt", "mydir"))

      # Check .gitignore
      gitignore <- readLines(".gitignore")
      # File should not have /**
      expect_true(any(grepl("!myfile.txt", gitignore, fixed = TRUE)))
      expect_false(any(grepl("!myfile.txt/**", gitignore, fixed = TRUE)))
      # Directory should have /**
      expect_true(any(grepl("!mydir/**", gitignore, fixed = TRUE)))
    }
  )
})

test_that("projr_unignore_manual handles paths with trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Unignore path with trailing slash (treated as directory)
      projr_unignore_manual("mypath/")

      # Check .gitignore - paths with trailing / are treated as dirs
      # and should have /** appended (after stripping the trailing /)
      gitignore <- readLines(".gitignore")
      # The pattern should be !mypath/** (grepl needs escaped \)
      expect_true(any(grepl("!mypath/", gitignore)))
    }
  )
})

test_that("projr_unignore_manual handles nonexistent paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Unignore nonexistent path (should be treated as file)
      projr_unignore_manual("nonexistent.log")

      # Check .gitignore - should NOT have /** (treated as file)
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!nonexistent.log", gitignore)))
      expect_false(any(grepl("!nonexistent.log/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_dir handles empty strings", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with only empty strings
      result <- projr_unignore_manual_dir(c("", ""))
      expect_false(result)

      # Test with character(0)
      result <- projr_unignore_manual_dir(character(0))
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_dir handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with numeric
      result <- projr_unignore_manual_dir(456)
      expect_false(result)

      # Test with NULL
      result <- projr_unignore_manual_dir(NULL)
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_file handles empty strings", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with only empty strings
      result <- projr_unignore_manual_file(c("", ""))
      expect_false(result)

      # Test with character(0)
      result <- projr_unignore_manual_file(character(0))
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_file handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with numeric
      result <- projr_unignore_manual_file(789)
      expect_false(result)

      # Test with NULL
      result <- projr_unignore_manual_file(NULL)
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_file_git handles existing ! prefix", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Unignore with existing ! prefix
      projr_unignore_manual_file_git("!alreadyhas.txt")

      # Check .gitignore - should have single !
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!alreadyhas.txt", gitignore)))
      # Should not have double !!
      expect_false(any(grepl("!!alreadyhas.txt", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_file_git handles multiple ! prefixes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Unignore with multiple ! prefixes
      projr_unignore_manual_file_git("!!!multiple.txt")

      # Check .gitignore - should have single !
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!multiple.txt", gitignore)))
      # Should not have !!
      expect_false(any(grepl("!!multiple.txt", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_dir_git handles existing /** suffix", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Unignore directory that already has /**
      projr_unignore_manual_dir_git("mydir/**")

      # Check .gitignore - should have /** but not doubled
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!mydir/\\*\\*", gitignore)))
      expect_false(any(grepl("!mydir/\\*\\*/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_dir_git handles paths without /** suffix", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Unignore directory without /**
      projr_unignore_manual_dir_git("plaindir")

      # Check .gitignore - should have /** appended
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!plaindir/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_file_rbuild handles trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Unignore file with trailing slashes
      projr_unignore_manual_file_rbuild("myfile.txt///")

      # Check .Rbuildignore - trailing slashes should be removed
      # glob2rx creates anchored patterns like ^myfile\.txt$
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("!^myfile\\.txt$", rbuildignore, fixed = TRUE)))
    }
  )
})

test_that("projr_unignore_manual_file_rbuild handles whitespace", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Unignore file with whitespace
      projr_unignore_manual_file_rbuild("  whitespace.txt  ")

      # Check .Rbuildignore - whitespace should be trimmed
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("!.*whitespace", rbuildignore)))
    }
  )
})

test_that("projr_unignore_manual_dir_rbuild handles trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Unignore directory with trailing slashes
      projr_unignore_manual_dir_rbuild("mydir///")

      # Check .Rbuildignore - should have proper regex patterns
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("!.*mydir", rbuildignore)))
    }
  )
})

test_that("projr_unignore_manual_dir_rbuild creates correct regex patterns", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Unignore directory
      projr_unignore_manual_dir_rbuild("testdir")

      # Check .Rbuildignore - should have two patterns (dir/ and glob)
      rbuildignore <- readLines(".Rbuildignore")

      # Should have patterns with ! prefix
      expect_true(any(grepl("^!.*testdir", rbuildignore)))

      # Count patterns with testdir - should have 2 (one for dir/, one for glob)
      testdir_patterns <- rbuildignore[grepl("testdir", rbuildignore)]
      expect_true(length(testdir_patterns) >= 2)
    }
  )
})

test_that(".unignore_manual_path_add handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a gitignore file
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Try to add empty vector
      result <- .unignore_manual_path_add(character(0), ".gitignore")
      expect_false(result)

      # Try to add only empty strings
      result <- .unignore_manual_path_add(c("", ""), ".gitignore")
      expect_false(result)
    }
  )
})

test_that(".unignore_manual_path_add adds patterns after projr section", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create gitignore with projr section
      initial_content <- c(
        "manual_pattern1",
        "",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "auto_pattern",
        "# End of projr section"
      )
      writeLines(initial_content, ".gitignore")

      # Add unignore pattern
      .unignore_manual_path_add("!unignore_pattern", ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")

      # Unignore pattern should be AFTER projr section (in the end portion)
      unignore_idx <- which(gitignore == "!unignore_pattern")
      end_idx <- which(grepl("End of projr section", gitignore))

      expect_true(length(unignore_idx) > 0)
      expect_true(length(end_idx) > 0)
      expect_true(unignore_idx[1] > end_idx[1])

      # Manual pattern should still exist
      expect_true(any(grepl("manual_pattern1", gitignore)))

      # Projr section should still exist
      expect_true(any(grepl("auto_pattern", gitignore)))
    }
  )
})

test_that(".unignore_manual_path_add handles file without projr section", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create gitignore without projr section
      initial_content <- c("existing_pattern")
      writeLines(initial_content, ".gitignore")

      # Add unignore pattern
      .unignore_manual_path_add("!new_unignore", ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")

      # Should have new pattern
      expect_true(any(grepl("!new_unignore", gitignore)))

      # Existing pattern should still be there
      expect_true(any(grepl("existing_pattern", gitignore)))

      # Should create projr section markers
      expect_true(any(grepl("Start of projr section", gitignore)))
      expect_true(any(grepl("End of projr section", gitignore)))
    }
  )
})

test_that(".unignore_manual_path_add removes duplicates", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create gitignore
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Add same pattern multiple times
      .unignore_manual_path_add(c("!pattern1", "!pattern1", "!pattern2"), ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")

      # Each pattern should appear only once
      count_pattern1 <- sum(gitignore == "!pattern1")
      count_pattern2 <- sum(gitignore == "!pattern2")

      expect_equal(count_pattern1, 1)
      expect_equal(count_pattern2, 1)
    }
  )
})

test_that(".unignore_manual_path_add_get_updated_end handles empty end", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with length 0 end
  result <- .unignore_manual_path_add_get_updated_end(c("!ignore1", "!ignore2"), character(0))
  expect_identical(result, c("!ignore1", "!ignore2"))
})

test_that(".unignore_manual_path_add_get_updated_end handles single empty string end", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with single empty string
  result <- .unignore_manual_path_add_get_updated_end(c("!ignore1"), "")
  expect_identical(result, c("", "!ignore1"))
})

test_that(".unignore_manual_path_add_get_updated_end handles non-empty single end", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with non-empty single element
  result <- .unignore_manual_path_add_get_updated_end(c("!ignore1", "!ignore2"), "# End marker")
  expect_identical(result, c("# End marker", "", "!ignore1", "!ignore2"))
})

test_that(".unignore_manual_path_add_get_updated_end handles multiple end elements", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with multiple end elements
  result <- .unignore_manual_path_add_get_updated_end(
    c("!ignore1", "!ignore2"),
    c("# End marker", "more content")
  )
  expect_identical(result, c("# End marker", "more content", "!ignore1", "!ignore2"))
})

test_that(".unignore_manual_path_add_get_updated_end avoids duplicates", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test when ignore patterns are already in end
  result <- .unignore_manual_path_add_get_updated_end(
    c("!ignore1", "!ignore2"),
    c("# End marker", "!ignore1")
  )

  # !ignore1 should not be duplicated
  expect_identical(result, c("# End marker", "!ignore1", "!ignore2"))
})

test_that("projr_unignore_manual works with mixed existing and nonexistent paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".Rbuildignore")

      # Create a file but not a directory
      file.create("existing.txt")

      # Unignore both existing and nonexistent
      projr_unignore_manual(c("existing.txt", "nonexistent.log", "nonexistent_dir/"))

      # Check .gitignore
      gitignore <- readLines(".gitignore")

      # Existing file should be unignored without /**
      expect_true(any(grepl("!existing.txt", gitignore)))
      expect_false(any(grepl("!existing.txt/\\*\\*", gitignore)))

      # Nonexistent file (no trailing slash) should be unignored without /**
      expect_true(any(grepl("!nonexistent.log", gitignore)))
      expect_false(any(grepl("!nonexistent.log/\\*\\*", gitignore)))

      # Nonexistent path with trailing slash should be treated as directory
      # Should have /** pattern
      expect_true(any(grepl("!nonexistent_dir/", gitignore)))
    }
  )
})

test_that("projr_unignore_manual is idempotent", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")

      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      # Unignore multiple times
      projr_unignore_manual("test.txt")
      projr_unignore_manual("test.txt")
      projr_unignore_manual("test.txt")

      # Check that it only appears once in the manual section
      gitignore <- readLines(".gitignore")
      count <- sum(gitignore == "!test.txt")
      expect_equal(count, 1)
    }
  )
})

test_that("projr_unignore_manual_file_git handles empty and non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty strings
      result <- projr_unignore_manual_file_git(c("", ""))
      expect_false(result)

      # Non-character
      result <- projr_unignore_manual_file_git(456)
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_dir_git handles empty and non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty strings
      result <- projr_unignore_manual_dir_git(c("", ""))
      expect_false(result)

      # Non-character
      result <- projr_unignore_manual_dir_git(NULL)
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_file_rbuild handles empty and non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty strings
      result <- projr_unignore_manual_file_rbuild(c("", ""))
      expect_false(result)

      # Non-character
      result <- projr_unignore_manual_file_rbuild(list("test"))
      expect_false(result)
    }
  )
})

test_that("projr_unignore_manual_dir_rbuild handles empty and non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty strings
      result <- projr_unignore_manual_dir_rbuild(c("", ""))
      expect_false(result)

      # Non-character
      result <- projr_unignore_manual_dir_rbuild(789)
      expect_false(result)
    }
  )
})
