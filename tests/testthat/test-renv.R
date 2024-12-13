test_that("projr_renv_test successfully restores renv environment", {
  skip_if_offline()
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Initialize renv and create a snapshot
  .projr_renv_rest_init()
  .projr_renv_rest_activate()
  .projr_renv_test_test_lockfile_create(
    file.path(dir_test, "renv.lock"),
    bad = FALSE
  )

  # Create a random file to copy across
  writeLines("blah", "test.txt")
  
  # Ensure that the renv.lock file exists
  expect_true(file.exists("renv.lock"))

  # Test restoration
  result <- projr_renv_test(file = "test.txt", delete_lib = FALSE)
  
  # Check that the result is TRUE indicating success
  expect_true(result)
})

test_that("projr_renv_test fails when it should", {
  skip_if_offline()
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)
  # Initialize renv and create a snapshot
  .projr_renv_rest_init()
  .projr_renv_rest_activate()
  .projr_renv_test_test_lockfile_create(
    file.path(dir_test, "renv.lock"),
    bad = TRUE
  )

  # Create a random file to copy across
  writeLines("blah", "test.txt")
  
  # Ensure that the renv.lock file exists
  expect_true(file.exists("renv.lock"))

  # Test restoration
  result <- projr_renv_test(file = "test.txt", delete_lib = FALSE)
  
  # Check that the result is TRUE indicating success
  expect_false(result)
})

test_that("projr_renv_restore and projr_renv_update work with mixed repositories", {
  skip_if_offline()

  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Initialize renv and create a snapshot
  .projr_renv_rest_init()
  .projr_renv_rest_activate()

  # Create the complex lockfile
  .projr_renv_test_test_lockfile_create(
    file.path(dir_test, "renv.lock"),
    bad = FALSE
  )

  # Run restore and update
  # projr_renv_restore() and projr_renv_update() should complete without error
  projr_renv_restore()
  projr_renv_update()

  # If we want to assert success in a more direct way,
  # we could check if the packages are installed
  installed <- rownames(installed.packages())
  expect_true("tinytest" %in% installed)
})
