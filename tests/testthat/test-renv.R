test_that("projr_test_renv_restore works", {
  # setup
  # skip_if(.is_test_select())
  skip_on_cran()
  skip_if_offline()
  skip_if(.is_test_fast())
  skip()
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_init_renv(force = FALSE, bioc = FALSE, skip_init = FALSE)
      pak_setting <- Sys.getenv("RENV_CONFIG_PAK_ENABLED")
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")
      try(renv::snapshot(prompt = FALSE))
      try(renv::update(prompt = FALSE))
      expect_true(projr_renv_test())
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = pak_setting)
    }
  )
})

# tests/testthat/test-projr_renv_test.R

# Ensure that your package's namespace is loaded to access internal functions
# Replace 'your_package' with the actual name of your packag

test_that("projr_renv_test successfully restores renv environment", {
  skip_if_offline()
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # browser()
      
      # Initialize renv in the temporary directory without installing any packages
      .projr_init_renv(force = FALSE, bioc = FALSE, skip_init = FALSE)
      
      # Install a sample package to establish dependencies
      # Here, we're using 'dplyr' as an example; you can choose any lightweight package
      renv::install("tinytest", prompt = FALSE)
      cat("library(tinytest)\n", file = "_dependencies.R")
      
      # Snapshot the environment to create renv.lock
      renv::snapshot(prompt = FALSE)
      
      # Create an .Rprofile to test file copying
      # This simulates additional configuration files that might be needed
      writeLines(
        c(
          "# .Rprofile for testing",
          "Sys.setenv(TEST_ENV_VAR = 'test_value')"
        ),
        ".Rprofile"
      )
      
      # Ensure that the renv.lock file exists
      expect_true(file.exists("renv.lock"))
      
      # Optionally, verify that the .Rprofile was created
      expect_true(file.exists(".Rprofile"))
      
      # Call projr_renv_test with the .Rprofile
      # Assuming projr_renv_test is exported or accessible within your package's namespace
      # If it's an internal function (starts with a dot), use your_package:::projr_renv_test
      debugonce(projr_renv_test)
      result <- projr_renv_test(file = ".Rprofile")
      
      # Check that the result is TRUE indicating success
      expect_true(result)
      
      # Define expected log file paths based on projr_renv_test implementation
      # Adjust the paths if your package uses different logging directories
      output_log <- projr_path_get_file("cache", "projr", "log-renv_restore-output.txt")
      error_log  <- projr_path_get_file("cache", "projr", "log-renv_restore-error.txt")
      
      # Check that log files exist
      expect_true(file.exists(output_log))
      expect_true(file.exists(error_log))
      
      # Optionally, inspect the log files for specific content
      # For example, ensure that "renv restore successful" is present in the output log
      output_content <- readLines(output_log, warn = FALSE)
      expect_true(any(grepl("renv restore successful", output_content, ignore.case = TRUE)),
                  info = "The output log should indicate a successful restore.")
      
      # Additionally, check that no errors were logged
      error_content <- readLines(error_log, warn = FALSE)
      expect_true(length(error_content) == 0 || all(nzchar(error_content) == FALSE),
                  info = "The error log should be empty if restoration was successful.")
  })
})
