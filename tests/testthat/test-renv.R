test_that("projr_renv_test successfully restores renv environment", {
  skip_if(.is_test_cran())
  skip_if_offline()
  skip_if(.is_test_select())
  skip_if_not(
    .test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to enable renv integration tests"
  )
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
  result <- projr_renv_test(files_to_copy = "test.txt", delete_lib = FALSE)

  # Check that the result is TRUE indicating success
  expect_true(result)
})

test_that("projr_renv_test fails when it should", {
  skip_if(.is_test_cran())
  skip_if_offline()
  skip_if(.is_test_select())
  skip_if_not(
    .test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to enable renv integration tests"
  )
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
  result <- projr_renv_test(files_to_copy = "test.txt", delete_lib = FALSE)

  # Check that the result is TRUE indicating success
  expect_false(result)
})

test_that("projr_renv_restore and.renv_update work with mixed repositories", {
  skip_if(.is_test_cran())
  skip_if_offline()
  skip_if(.is_test_select())
  skip_if_not(
    .test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to enable renv integration tests"
  )

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
  # projr_renv_restore() and projr_renv_update() should complete without error
  projr_renv_restore()
  projr_renv_update()

  # If we want to assert success in a more direct way,
  # we could check if the packages are installed
  installed <- rownames(installed.packages())
  expect_true("tinytest" %in% installed)
})

# Tests for parameter validation
test_that("projr_renv_restore validates github parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Create a minimal lockfile (just needs to exist for validation)
  writeLines('{"R": {"Version": "4.0"}, "Packages": {}}', "renv.lock")

  # Test with non-logical github parameter
  expect_error(
    projr_renv_restore(github = "yes"),
    "'github' must be a single logical value"
  )

  # Test with multiple values
  expect_error(
    projr_renv_restore(github = c(TRUE, FALSE)),
    "'github' must be a single logical value"
  )
})

test_that("projr_renv_restore validates non_github parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Create a minimal lockfile
  writeLines('{"R": {"Version": "4.0"}, "Packages": {}}', "renv.lock")

  # Test with non-logical non_github parameter
  expect_error(
    projr_renv_restore(non_github = "yes"),
    "'non_github' must be a single logical value"
  )

  # Test with multiple values
  expect_error(
    projr_renv_restore(non_github = c(TRUE, FALSE)),
    "'non_github' must be a single logical value"
  )
})

test_that("projr_renv_restore validates biocmanager_install parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Create a minimal lockfile
  writeLines('{"R": {"Version": "4.0"}, "Packages": {}}', "renv.lock")

  # Test with non-logical biocmanager_install parameter
  expect_error(
    projr_renv_restore(biocmanager_install = "yes"),
    "'biocmanager_install' must be a single logical value"
  )

  # Test with multiple values
  expect_error(
    projr_renv_restore(biocmanager_install = c(TRUE, FALSE)),
    "'biocmanager_install' must be a single logical value"
  )
})

test_that("projr_renv_restore requires at least one package source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Create a minimal lockfile
  writeLines('{"R": {"Version": "4.0"}, "Packages": {}}', "renv.lock")

  # Test with both github and non_github set to FALSE
  expect_error(
    projr_renv_restore(github = FALSE, non_github = FALSE),
    "At least one of 'github' or 'non_github' must be TRUE"
  )
})

test_that("projr_renv_restore checks for lockfile existence", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Don't create a lockfile
  expect_error(
    projr_renv_restore(),
    "renv.lock file not found in the current directory"
  )
})

test_that("projr_renv_update validates parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Create a minimal lockfile
  writeLines('{"R": {"Version": "4.0"}, "Packages": {}}', "renv.lock")

  # Test with invalid parameters (same validation as restore)
  expect_error(
    projr_renv_update(github = "yes"),
    "'github' must be a single logical value"
  )

  expect_error(
    projr_renv_update(github = FALSE, non_github = FALSE),
    "At least one of 'github' or 'non_github' must be TRUE"
  )
})

test_that("projr_renv_restore_and_update validates parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit(unlink(dir_test, recursive = TRUE), add = TRUE)
  setwd(dir_test)

  # Create a minimal lockfile
  writeLines('{"R": {"Version": "4.0"}, "Packages": {}}', "renv.lock")

  # Test with invalid parameters
  expect_error(
    projr_renv_restore_and_update(github = "yes"),
    "'github' must be a single logical value"
  )

  expect_error(
    projr_renv_restore_and_update(github = FALSE, non_github = FALSE),
    "At least one of 'github' or 'non_github' must be TRUE"
  )
})
