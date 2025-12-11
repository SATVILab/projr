# Tests for R/ignore-manual.R
# Focus on increasing test coverage for internal helper functions

test_that("projr_ignore handles trailing slashes in directory paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Test with path ending in /
      projr_ignore("testdir/")

      # Check .gitignore has /** appended (results in testdir//**)
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("testdir", gitignore)))
      # Since "testdir/" is treated as a directory, it should have /** appended
      expect_true(any(grepl("/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_ignore handles multiple paths with mixed types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Create some test files and directories
      dir.create("mydir")
      file.create("myfile.txt")

      # Ignore multiple paths at once
      projr_ignore(c("mydir", "myfile.txt", "nonexistent.log", "anotherdir/"))

      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("mydir/\\*\\*", gitignore)))
      expect_true(any(grepl("myfile.txt", gitignore)))
      expect_true(any(grepl("nonexistent.log", gitignore)))
      # anotherdir/ is treated as directory and gets /** appended (results in anotherdir//**)
      expect_true(any(grepl("anotherdir", gitignore)))
      expect_true(any(grepl("/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_ignore_dir_git with paths already containing /**", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      # Ignore directory with /** already appended
      projr_ignore_dir_git("mydir/**")

      # Check that it doesn't double-append
      gitignore <- readLines(".gitignore")
      # Should have mydir/** but not mydir/**/**
      expect_true(any(grepl("mydir/\\*\\*", gitignore)))
      expect_false(any(grepl("mydir/\\*\\*/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_ignore_file_git with force_create=FALSE and no git repo", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .gitignore and ensure no git repo
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".git")) unlink(".git", recursive = TRUE)

      # Try to ignore without force_create
      result <- projr_ignore_file_git("test.txt", force_create = FALSE)

      # Should not create .gitignore
      expect_false(file.exists(".gitignore"))
      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_file_git with force_create=FALSE and git repo exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(!.git_system_check_git())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      # Try to ignore without force_create but with git repo
      result <- projr_ignore_file_git("test.txt", force_create = FALSE)

      # Should create .gitignore because git repo exists
      expect_true(file.exists(".gitignore"))
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("test.txt", gitignore)))
    }
  )
})

test_that("projr_ignore_dir_git with force_create=FALSE and no git repo", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .gitignore and ensure no git repo
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".git")) unlink(".git", recursive = TRUE)

      # Try to ignore without force_create
      result <- projr_ignore_dir_git("testdir", force_create = FALSE)

      # Should not create .gitignore
      expect_false(file.exists(".gitignore"))
      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_file_rbuild with force_create=FALSE and no DESCRIPTION", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .Rbuildignore and DESCRIPTION
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")
      if (file.exists("DESCRIPTION")) unlink("DESCRIPTION")

      # Try to ignore without force_create
      result <- projr_ignore_file_rbuild("test.txt", force_create = FALSE)

      # Should not create .Rbuildignore
      expect_false(file.exists(".Rbuildignore"))
      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_file_rbuild with force_create=FALSE and DESCRIPTION exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .Rbuildignore but keep DESCRIPTION
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Try to ignore without force_create but with DESCRIPTION
      result <- projr_ignore_file_rbuild("test.txt", force_create = FALSE)

      # Should create .Rbuildignore because DESCRIPTION exists
      expect_true(file.exists(".Rbuildignore"))
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("test", rbuildignore)))
    }
  )
})

test_that("projr_ignore_dir_rbuild with force_create=FALSE and no DESCRIPTION", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .Rbuildignore and DESCRIPTION
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")
      if (file.exists("DESCRIPTION")) unlink("DESCRIPTION")

      # Try to ignore without force_create
      result <- projr_ignore_dir_rbuild("testdir", force_create = FALSE)

      # Should not create .Rbuildignore
      expect_false(file.exists(".Rbuildignore"))
      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_dir_rbuild creates correct regex patterns", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Ignore a directory
      projr_ignore_dir_rbuild("mydir")

      # Check .Rbuildignore has proper patterns
      rbuildignore <- readLines(".Rbuildignore")

      # Should have two patterns: one for directory and one for glob
      # Pattern format after glob2rx and modifications
      expect_true(any(grepl("mydir", rbuildignore)))
    }
  )
})

test_that("projr_ignore_file_rbuild handles trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Ignore file with trailing slashes (should be removed)
      projr_ignore_file_rbuild("myfile.txt///")

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")

      # Trailing slashes should be removed before glob2rx
      expect_true(any(grepl("myfile", rbuildignore)))
    }
  )
})

test_that("projr_ignore_dir_rbuild handles trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Ignore directory with multiple trailing slashes
      projr_ignore_dir_rbuild("mydir///")

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")

      # Trailing slashes should be removed before processing
      expect_true(any(grepl("mydir", rbuildignore)))
    }
  )
})

test_that("projr_ignore handles empty strings in input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")

      # Test with mix of empty and valid paths
      projr_ignore(c("", "valid.txt", "", ""))

      # Check that only valid.txt is added
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("valid.txt", gitignore)))
      # Should not have empty lines that are empty-string entries
    }
  )
})

test_that("projr_ignore_dir handles empty strings in input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with only empty strings
      result <- projr_ignore_dir(c("", ""))

      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_file handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with numeric input
      result <- projr_ignore_file(123)

      expect_identical(result, FALSE)

      # Test with NULL
      result <- projr_ignore_file(NULL)

      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_dir handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with numeric input
      result <- projr_ignore_dir(123)

      expect_identical(result, FALSE)

      # Test with list input
      result <- projr_ignore_dir(list("test"))

      expect_identical(result, FALSE)
    }
  )
})

test_that(".ignore_manual_path_add handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a gitignore file
      writeLines("test", ".gitignore")

      # Try to add empty vector
      result <- .ignore_manual_path_add(character(0), ".gitignore")

      expect_identical(result, FALSE)

      # Try to add only empty strings
      result <- .ignore_manual_path_add(c("", ""), ".gitignore")

      expect_identical(result, FALSE)
    }
  )
})

test_that(".ignore_manual_path_add adds patterns to empty file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create empty gitignore
      writeLines("", ".gitignore")

      # Add patterns
      .ignore_manual_path_add(c("pattern1", "pattern2"), ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("pattern1", gitignore)))
      expect_true(any(grepl("pattern2", gitignore)))
    }
  )
})

test_that(".ignore_manual_path_add preserves existing projr section", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create gitignore with projr section
      initial_content <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "auto_pattern",
        "# End of projr section"
      )
      writeLines(initial_content, ".gitignore")

      # Add manual pattern
      .ignore_manual_path_add("manual_pattern", ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")
      # Manual pattern should be before projr section
      manual_idx <- which(gitignore == "manual_pattern")
      start_idx <- which(grepl("Start of projr section", gitignore))

      expect_true(length(manual_idx) > 0)
      expect_true(length(start_idx) > 0)
      expect_true(manual_idx[1] < start_idx[1])

      # Projr section should still exist
      expect_true(any(grepl("auto_pattern", gitignore)))
    }
  )
})

test_that(".ignore_manual_path_add handles file with only projr section marker", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create gitignore with only the section marker
      initial_content <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      )
      writeLines(initial_content, ".gitignore")

      # Add manual pattern
      .ignore_manual_path_add("new_pattern", ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")

      # Should have blank line before the section marker
      start_idx <- which(grepl("Start of projr section", gitignore))
      expect_true(length(start_idx) > 0)

      # Pattern should be added before the marker
      expect_true(any(grepl("new_pattern", gitignore)))
      pattern_idx <- which(gitignore == "new_pattern")
      expect_true(pattern_idx[1] < start_idx[1])
    }
  )
})

test_that(".ignore_manual_path_add handles multiple additions correctly", {
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

      # Add first pattern
      .ignore_manual_path_add("pattern1", ".gitignore")

      # Add second pattern
      .ignore_manual_path_add("pattern2", ".gitignore")

      # Add duplicate pattern
      .ignore_manual_path_add("pattern1", ".gitignore")

      # Check file
      gitignore <- readLines(".gitignore")

      # Both patterns should exist
      expect_true(any(grepl("pattern1", gitignore)))
      expect_true(any(grepl("pattern2", gitignore)))

      # pattern1 should appear only once
      count_pattern1 <- sum(gitignore == "pattern1")
      expect_equal(count_pattern1, 1)
    }
  )
})

test_that("projr_ignore_dir_rbuild handles multiple directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Ignore multiple directories
      projr_ignore_dir_rbuild(c("dir1", "dir2", "dir3"))

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")

      # All directories should be present
      expect_true(any(grepl("dir1", rbuildignore)))
      expect_true(any(grepl("dir2", rbuildignore)))
      expect_true(any(grepl("dir3", rbuildignore)))
    }
  )
})

test_that("projr_ignore_file_rbuild handles whitespace in paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .Rbuildignore
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Ignore file with leading/trailing whitespace
      projr_ignore_file_rbuild("  myfile.txt  ")

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")

      # Whitespace should be trimmed
      expect_true(any(grepl("myfile", rbuildignore)))
    }
  )
})

test_that("projr_ignore with existing files and nonexistent files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # Create a file
      file.create("existing.txt")

      # Ignore both existing and nonexistent
      projr_ignore(c("existing.txt", "nonexistent.log"))

      # Check .gitignore
      gitignore <- readLines(".gitignore")

      # Both should be treated as files (nonexistent assumed to be file)
      expect_true(any(grepl("existing.txt", gitignore)))
      expect_true(any(grepl("nonexistent.log", gitignore)))

      # Neither should have /** appended (they're files)
      expect_false(any(grepl("existing.txt/\\*\\*", gitignore)))
      expect_false(any(grepl("nonexistent.log/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_ignore_dir_git handles multiple patterns with /**", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing .gitignore
      if (file.exists(".gitignore")) unlink(".gitignore")

      # Ignore multiple directories, some with /** already
      projr_ignore_dir_git(c("dir1", "dir2/**", "dir3"))

      # Check .gitignore
      gitignore <- readLines(".gitignore")

      # All should have /** (but not doubled)
      expect_true(any(grepl("dir1/\\*\\*", gitignore)))
      expect_true(any(grepl("dir2/\\*\\*", gitignore)))
      expect_true(any(grepl("dir3/\\*\\*", gitignore)))

      # No double appending
      expect_false(any(grepl("dir2/\\*\\*/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_ignore_file without default force_create parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")

      # projr_ignore_file doesn't have force_create parameter
      # It should create files if they don't exist
      projr_ignore_file("test.txt")

      # Both files should be created
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
    }
  )
})
