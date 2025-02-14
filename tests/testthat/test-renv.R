test_that(.renv_test successfully restores renv environment", {
  skip_if_offline()
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Initialize renv and create a snapshot
  .renv_rest_init()
  .renv_rest_activate()
  .renv_test_test_lockfile_create(
    file.path(dir_test, "renv.lock"),
    bad = FALSE
  )

  # Create a random file to copy across
  writeLines("blah", "test.txt")
  
  # Ensure that the renv.lock file exists
  expect_true(file.exists("renv.lock"))

  # Test restoration
  result <-.renv_test(file = "test.txt", delete_lib = FALSE)
  
  # Check that the result is TRUE indicating success
  expect_true(result)
})

test_that(.renv_test fails when it should", {
  skip_if_offline()
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)
  # Initialize renv and create a snapshot
  .renv_rest_init()
  .renv_rest_activate()
  .renv_test_test_lockfile_create(
    file.path(dir_test, "renv.lock"),
    bad = TRUE
  )

  # Create a random file to copy across
  writeLines("blah", "test.txt")
  
  # Ensure that the renv.lock file exists
  expect_true(file.exists("renv.lock"))

  # Test restoration
  result <-.renv_test(file = "test.txt", delete_lib = FALSE)
  
  # Check that the result is TRUE indicating success
  expect_false(result)
})

test_that(.renv_restore and.renv_update work with mixed repositories", {
  skip_if_offline()
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Initialize renv and create a snapshot
  .renv_rest_init()
  .renv_rest_activate()

  # Create the complex lockfile
  .renv_test_test_lockfile_create(
    file.path(dir_test, "renv.lock"),
    bad = FALSE
  )

  # Run restore and update
  #.renv_restore() and.renv_update() should complete without error
 .renv_restore()
 .renv_update()

  # If we want to assert success in a more direct way,
  # we could check if the packages are installed
  installed <- rownames(installed.packages())
  expect_true("tinytest" %in% installed)
})
