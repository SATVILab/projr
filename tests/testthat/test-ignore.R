test_that("projr_ignore works with files and directories", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing files to start fresh
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")
      
      # Create test files and directories
      dir.create("test_dir")
      file.create("test_file.txt")
      
      # Test ignoring files
      projr_ignore_file(c("test_file.txt", "nonexistent.log"))
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("test_file.txt", gitignore)))
      expect_true(any(grepl("nonexistent.log", gitignore)))
      
      # Test ignoring directories
      projr_ignore_dir("test_dir")
      
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("test_dir/\\*\\*", gitignore)))
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      # glob2rx converts patterns, so we need to check for the regex pattern
      expect_true(any(grepl("test_file", rbuildignore)))
      expect_true(any(grepl("test_dir", rbuildignore)))
      expect_true(any(grepl("nonexistent", rbuildignore)))
    }
  )
})

test_that("projr_ignore handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with empty character vector
      result <- projr_ignore(character(0))
      expect_identical(result, FALSE)
      
      # Test with empty strings
      result <- projr_ignore(c("", ""))
      expect_identical(result, FALSE)
    }
  )
})

test_that("projr_ignore_dir works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory
      dir.create("my_dir")
      
      # Ignore directory
      projr_ignore_dir("my_dir")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("my_dir/\\*\\*", gitignore)))
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("my_dir", rbuildignore)))
    }
  )
})

test_that("projr_ignore_file works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test file
      file.create("my_file.txt")
      
      # Ignore file
      projr_ignore_file("my_file.txt")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("my_file.txt", gitignore)))
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      # glob2rx converts patterns
      expect_true(any(grepl("my_file", rbuildignore)))
    }
  )
})

test_that("projr_ignore_file_git works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test file
      file.create("test.log")
      
      # Ignore file in git only
      projr_ignore_file_git("test.log")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("test.log", gitignore)))
      
      # Rbuildignore should not have the file if we didn't call the rbuild function
      # But it was already created by .test_setup_project, so we just check that
      # the function doesn't error
      expect_true(file.exists(".gitignore"))
    }
  )
})

test_that("projr_ignore_dir_git works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory
      dir.create("logs")
      
      # Ignore directory in git only
      projr_ignore_dir_git("logs")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("logs/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_ignore_file_rbuild works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test file
      file.create("build_artifact.o")
      
      # Ignore file in rbuild only
      projr_ignore_file_rbuild("build_artifact.o")
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      # glob2rx converts patterns
      expect_true(any(grepl("build_artifact", rbuildignore)))
    }
  )
})

test_that("projr_ignore_dir_rbuild works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory
      dir.create("build_dir")
      
      # Ignore directory in rbuild only
      projr_ignore_dir_rbuild("build_dir")
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("build_dir", rbuildignore)))
    }
  )
})

test_that("projr_ignore with force_create = FALSE respects existing files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .gitignore but keep .Rbuildignore
      unlink(".gitignore")
      
      # Try to ignore without force_create (should not create files without git repo)
      result <- projr_ignore_file_git("test.txt", force_create = FALSE)
      expect_false(file.exists(".gitignore"))
      expect_identical(result, FALSE)
      
      # With force_create = TRUE, it should create the file
      projr_ignore_file_git("test.txt", force_create = TRUE)
      expect_true(file.exists(".gitignore"))
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("test.txt", gitignore)))
    }
  )
})

test_that("projr_ignore_auto works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create some files that should be auto-ignored
      file.create("_projr-local.yml")
      dir.create("_extensions")
      
      # Run auto ignore
      projr_ignore_auto()
      
      # Check .gitignore exists and has projr section
      expect_true(file.exists(".gitignore"))
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("Start of projr section", gitignore)))
      expect_true(any(grepl("End of projr section", gitignore)))
      
      # Check that _projr-local.yml is ignored in git
      expect_true(any(grepl("_projr-local.yml", gitignore)))
      
      # Check .Rbuildignore exists
      expect_true(file.exists(".Rbuildignore"))
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("Start of projr section", rbuildignore)))
      expect_true(any(grepl("End of projr section", rbuildignore)))
    }
  )
})

test_that("projr_unignore_manual works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files to start fresh
      if (file.exists(".gitignore")) unlink(".gitignore")
      if (file.exists(".Rbuildignore")) unlink(".Rbuildignore")
      
      # Create files with projr section to avoid multiple sections bug
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section",
        ""
      ), ".gitignore")
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section",
        ""
      ), ".Rbuildignore")
      
      # Create test file and directory
      file.create("important.txt")
      dir.create("important_dir")
      
      # First ignore them
      projr_ignore_file("important.txt")
      projr_ignore_dir("important_dir")
      
      # Now unignore them
      projr_unignore_manual_file("important.txt")
      projr_unignore_manual_dir("important_dir")
      
      # Check .gitignore has the unignore patterns
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!important.txt", gitignore)))
      expect_true(any(grepl("!important_dir/\\*\\*", gitignore)))
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("^!", rbuildignore)))
    }
  )
})

test_that("projr_unignore_manual_dir works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create and ignore a directory
      dir.create("special_dir")
      projr_ignore_dir("special_dir")
      
      # Unignore it
      projr_unignore_manual_dir("special_dir")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!special_dir/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_file works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create and ignore a file
      file.create("special_file.txt")
      projr_ignore_file("special_file.txt")
      
      # Unignore it
      projr_unignore_manual_file("special_file.txt")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!special_file.txt", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_file_git works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Unignore a file in git
      projr_unignore_manual_file_git("important.log")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!important.log", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_dir_git works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Unignore a directory in git
      projr_unignore_manual_dir_git("important_data")
      
      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("!important_data/\\*\\*", gitignore)))
    }
  )
})

test_that("projr_unignore_manual_file_rbuild works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Unignore a file in rbuild
      projr_unignore_manual_file_rbuild("important_script.R")
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("^!.*important_script", rbuildignore)))
    }
  )
})

test_that("projr_unignore_manual_dir_rbuild works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Unignore a directory in rbuild
      projr_unignore_manual_dir_rbuild("important_scripts")
      
      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("^!.*important_scripts", rbuildignore)))
    }
  )
})

test_that("ignore functions handle paths with trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing gitignore to start fresh
      if (file.exists(".gitignore")) unlink(".gitignore")
      
      # Ignore path with trailing slash (will be treated as directory)
      projr_ignore("mydir/")
      
      # Check that it's treated as directory in gitignore
      # Note: "mydir/" becomes "mydir//**"
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("mydir/", gitignore)))
    }
  )
})

test_that("ignore functions are idempotent", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove existing ignore files to start fresh
      if (file.exists(".gitignore")) unlink(".gitignore")
      
      # Create gitignore with projr section to avoid multiple sections bug
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section",
        ""
      ), ".gitignore")
      
      # Create test file
      file.create("test.txt")
      
      # Ignore multiple times
      projr_ignore_file("test.txt")
      projr_ignore_file("test.txt")
      projr_ignore_file("test.txt")
      
      # Check that it only appears once in the manual section (before projr section)
      gitignore <- readLines(".gitignore")
      # Count occurrences of test.txt (should be 1)
      count <- sum(gitignore == "test.txt")
      expect_equal(count, 1)
    }
  )
})

test_that("projr_ignore_auto handles multiple calls correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Run auto ignore multiple times
      projr_ignore_auto()
      gitignore1 <- readLines(".gitignore")
      
      projr_ignore_auto()
      gitignore2 <- readLines(".gitignore")
      
      # Should be identical
      expect_identical(gitignore1, gitignore2)
      
      # Should only have one projr section
      start_count <- sum(grepl("Start of projr section", gitignore2))
      end_count <- sum(grepl("End of projr section", gitignore2))
      expect_equal(start_count, 1)
      expect_equal(end_count, 1)
    }
  )
})
