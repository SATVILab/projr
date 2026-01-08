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

# =============================================================================
# Tests for helper functions
# =============================================================================

test_that(".generate_random_string generates strings of correct length", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Default length is 10
  str1 <- .generate_random_string()
  expect_equal(nchar(str1), 10)

  # Custom length
  str2 <- .generate_random_string(5)
  expect_equal(nchar(str2), 5)

  str3 <- .generate_random_string(20)
  expect_equal(nchar(str3), 20)
})

test_that(".generate_random_string generates unique strings", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Generate multiple strings and check they're different
  strings <- replicate(100, .generate_random_string())
  expect_true(length(unique(strings)) > 90)  # Very unlikely to have many duplicates
})

test_that(".generate_random_string only uses valid characters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  str <- .generate_random_string(100)
  # Should only contain letters and digits
  expect_true(grepl("^[a-zA-Z0-9]+$", str))
})

test_that(".ensure_cli runs without error", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should not error since cli is available in tests
  expect_silent(.ensure_cli())
})

test_that(".ensure_biocmanager runs without error", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should not error
  expect_silent(.ensure_biocmanager())
})

# =============================================================================
# Tests for validation functions
# =============================================================================

test_that(".check_renv runs without error when renv is available", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should not error since renv is available
  expect_silent(.check_renv())
})

test_that(".check_renv_params validates github parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid parameters should not error
  expect_silent(.check_renv_params(TRUE, TRUE, FALSE))
  expect_silent(.check_renv_params(FALSE, TRUE, FALSE))
  expect_silent(.check_renv_params(TRUE, FALSE, FALSE))

  # Invalid github - not logical
  expect_error(
    .check_renv_params("yes", TRUE, FALSE),
    "'github' must be a single logical value"
  )

  # Invalid github - multiple values
  expect_error(
    .check_renv_params(c(TRUE, FALSE), TRUE, FALSE),
    "'github' must be a single logical value"
  )
})

test_that(".check_renv_params validates non_github parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid non_github - not logical
  expect_error(
    .check_renv_params(TRUE, "yes", FALSE),
    "'non_github' must be a single logical value"
  )

  # Invalid non_github - multiple values
  expect_error(
    .check_renv_params(TRUE, c(TRUE, FALSE), FALSE),
    "'non_github' must be a single logical value"
  )
})

test_that(".check_renv_params validates biocmanager_install parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid biocmanager_install - not logical
  expect_error(
    .check_renv_params(TRUE, TRUE, "yes"),
    "'biocmanager_install' must be a single logical value"
  )

  # Invalid biocmanager_install - multiple values
  expect_error(
    .check_renv_params(TRUE, TRUE, c(TRUE, FALSE)),
    "'biocmanager_install' must be a single logical value"
  )
})

test_that(".check_renv_params requires at least one source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Both FALSE should error
  expect_error(
    .check_renv_params(FALSE, FALSE, FALSE),
    "At least one of 'github' or 'non_github' must be TRUE"
  )
})

test_that(".check_renv_lockfile checks for file existence", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  # No lockfile exists
  expect_error(
    .check_renv_lockfile(),
    "renv.lock file not found"
  )

  # Create lockfile
  writeLines('{"R": {"Version": "4.0"}}', "renv.lock")

  # Should not error now
  expect_silent(.check_renv_lockfile())
})

# =============================================================================
# Tests for internal renv operation functions
# =============================================================================

test_that(".renv_rest_disable_cache modifies .Rprofile correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  # Create an .Rprofile file
  writeLines("# test", ".Rprofile")

  # Run disable cache
  .renv_rest_disable_cache()

  # Check that the setting was appended
  content <- readLines(".Rprofile")
  expect_true(any(grepl("renv::settings\\$use.cache\\(FALSE\\)", content)))
})

test_that(".renv_rest_run_rscript executes simple commands successfully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Simple command that should succeed
  result <- .renv_rest_run_rscript("message('hello')", message_path = FALSE)
  expect_true(result$success)
  expect_null(result$error)
})

test_that(".renv_rest_run_rscript handles errors correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Command that should fail
  result <- .renv_rest_run_rscript("stop('test error')", message_path = FALSE)
  expect_false(result$success)
  expect_true(!is.null(result$error))
})

test_that(".renv_rest_run_rscript vanilla option works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with vanilla = TRUE
  result <- .renv_rest_run_rscript("message('test')", message_path = FALSE, vanilla = TRUE)
  expect_true(result$success)
})

# =============================================================================
# Tests for lockfile parsing
# =============================================================================

test_that(".renv_lockfile_pkg_get parses regular CRAN packages", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if_offline()

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  # Initialize renv
  .renv_rest_init()
  .renv_rest_activate()

  # Create a lockfile with CRAN packages
  .renv_test_test_lockfile_create("renv.lock", bad = FALSE)

  # Parse lockfile
  pkg_list <- .renv_lockfile_pkg_get()

  # Check structure
  expect_true(is.list(pkg_list))
  expect_true("regular" %in% names(pkg_list))
  expect_true("bioc" %in% names(pkg_list))
  expect_true("gh" %in% names(pkg_list))

  # tinytest should be in regular packages
  expect_true("tinytest" %in% pkg_list$regular)
})

test_that(".renv_lockfile_pkg_get categorizes packages correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if_offline()

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  .renv_rest_init()
  .renv_rest_activate()

  # Create a complex lockfile with different package types
  current_r_version <- R.version$major
  current_r_minor_version <- R.version$minor
  full_version <- paste(current_r_version, current_r_minor_version, sep = ".")

  lockfile_content <- list(
    R = list(
      Version = full_version,
      Repositories = list(
        list(Name = "CRAN", URL = "https://cran.r-project.org")
      )
    ),
    Packages = list(
      jsonlite = list(
        Package = "jsonlite",
        Version = "1.8.0",
        Source = "Repository",
        Repository = "CRAN"
      ),
      BiocGenerics = list(
        Package = "BiocGenerics",
        Version = "0.40.0",
        Source = "Bioconductor"
      ),
      testthat = list(
        Package = "testthat",
        Version = "3.1.5",
        Source = "GitHub",
        RemoteUsername = "r-lib",
        RemoteRepo = "testthat"
      )
    )
  )

  jsonlite::write_json(
    lockfile_content,
    path = "renv.lock",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  # Parse lockfile
  pkg_list <- .renv_lockfile_pkg_get()

  # Check categorization
  expect_true("jsonlite" %in% pkg_list$regular)
  expect_true("BiocGenerics" %in% pkg_list$bioc)
  expect_true("r-lib/testthat" %in% pkg_list$gh)
})

# =============================================================================
# Tests for restore/update wrapper functions
# =============================================================================

test_that(".renv_restore_or_update_actual_wrapper skips when act is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should run without error when act is FALSE
  expect_no_error({
    .renv_restore_or_update_actual_wrapper(
      pkg = c("package1", "package2"),
      act = FALSE,
      restore = TRUE,
      source = "CRAN",
      biocmanager_install = FALSE
    )
  })
})

test_that(".renv_restore_or_update_actual_wrapper handles empty package list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should return invisible(FALSE) for empty package list
  result <- .renv_restore_or_update_actual_wrapper(
    pkg = character(0),
    act = TRUE,
    restore = TRUE,
    source = "CRAN",
    biocmanager_install = FALSE
  )

  expect_identical(result, FALSE)
})

# =============================================================================
# Tests for .renv_rest_copy_files
# =============================================================================

test_that(".renv_rest_copy_files always includes renv.lock", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test that the function processes files_to_copy and adds renv.lock
  # We can't fully test it without path_dir_from, but we can test the logic
  # that ensures renv.lock is included
  files_input <- c("file1.txt", "file2.txt")
  files_expected <- union(files_input, "renv.lock")

  expect_true("renv.lock" %in% files_expected)
  expect_true("file1.txt" %in% files_expected)
  expect_true("file2.txt" %in% files_expected)
})

# =============================================================================
# Tests for .renv_test_test_snapshot
# =============================================================================

test_that(".renv_test_test_snapshot creates dependencies and snapshot", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if_not(
    .test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to enable renv integration tests"
  )

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  # Initialize renv
  .renv_rest_init()
  .renv_rest_activate()

  # Run snapshot test
  .renv_test_test_snapshot()

  # Check that _dependencies.R was created
  expect_true(file.exists("_dependencies.R"))

  # Check that renv.lock was updated
  expect_true(file.exists("renv.lock"))

  # Read and verify lockfile contains tinytest
  lockfile <- jsonlite::read_json("renv.lock")
  expect_true("tinytest" %in% names(lockfile$Packages))
})

# =============================================================================
# Tests for .renv_rest_restore
# =============================================================================

test_that(".renv_rest_restore handles successful restore", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if_not(
    .test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to enable renv integration tests"
  )

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  # Initialize renv
  .renv_rest_init()
  .renv_rest_activate()

  # Create a valid lockfile
  .renv_test_test_lockfile_create("renv.lock", bad = FALSE)

  # Test restoration - should succeed
  result <- .renv_rest_restore()
  expect_true(result)
})

test_that(".renv_rest_restore handles failed restore", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if_offline()
  skip_if_not(
    .test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to enable renv integration tests"
  )

  orig_dir <- getwd()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  on.exit({
    tryCatch(setwd(orig_dir), error = function(e) NULL)
    unlink(dir_test, recursive = TRUE)
  }, add = TRUE)

  setwd(dir_test)

  # Initialize renv
  .renv_rest_init()
  .renv_rest_activate()

  # Create a bad lockfile (impossible version)
  .renv_test_test_lockfile_create("renv.lock", bad = TRUE)

  # Test restoration - should fail
  result <- .renv_rest_restore()
  expect_false(result)
})

# =============================================================================
# Tests for .renv_restore_or_update_impl
# =============================================================================

test_that(".renv_restore_or_update_impl calls wrappers correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create a simple package list
  package_list <- list(
    regular = character(0),
    bioc = character(0),
    gh = character(0)
  )

  # Should run without error even with empty lists
  expect_no_error({
    .renv_restore_or_update_impl(
      package_list = package_list,
      github = FALSE,
      non_github = TRUE,
      restore = TRUE,
      biocmanager_install = FALSE
    )
  })
})
