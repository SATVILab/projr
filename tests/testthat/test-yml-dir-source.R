test_that(".yml_dir_get_source works with no source configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with no source configured
      source_val <- .yml_dir_get_source("output", "default")
      expect_true(is.null(source_val))

      source_val <- .yml_dir_get_source("raw-data", "default")
      expect_true(is.null(source_val))

      source_val <- .yml_dir_get_source("cache", "default")
      expect_true(is.null(source_val))
    }
  )
})

test_that(".yml_dir_get_source works with configured source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up source configuration
      yml_source <- list(
        github = list(
          github = list(
            id = "v0.0.1",
            structure = "archive"
          )
        )
      )

      .yml_dir_set_source(yml_source, "output", "default")

      # Test retrieval
      source_val <- .yml_dir_get_source("output", "default")
      expect_false(is.null(source_val))
      expect_true("github" %in% names(source_val))
      expect_identical(source_val$github$github$id, "v0.0.1")
    }
  )
})

test_that(".yml_dir_get_source returns NULL when yml_label is NULL", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add a label then remove it
      .yml_dir_add_label("_test", label = "output-test", profile = "default")
      .yml_dir_set_label(NULL, "output-test", "default")

      # Try to get source from removed label
      source_val <- .yml_dir_get_source("output-test", "default")
      expect_true(is.null(source_val))
    }
  )
})

test_that(".yml_dir_set_source sets source configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up source with GitHub configuration
      yml_source <- list(
        github = list(
          github = list(
            id = "v0.0.2",
            structure = "latest"
          )
        )
      )

      .yml_dir_set_source(yml_source, "raw-data", "default")

      # Verify it was set
      source_val <- .yml_dir_get_source("raw-data", "default")
      expect_false(is.null(source_val))
      expect_identical(source_val$github$github$id, "v0.0.2")
      expect_identical(source_val$github$github$structure, "latest")
    }
  )
})

test_that(".yml_dir_set_source removes source when set to NULL", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # First set a source
      yml_source <- list(
        osf = list(
          osf = list(
            id = "abc123"
          )
        )
      )

      .yml_dir_set_source(yml_source, "cache", "default")
      source_val <- .yml_dir_get_source("cache", "default")
      expect_false(is.null(source_val))

      # Now set to NULL to remove
      .yml_dir_set_source(NULL, "cache", "default")
      source_val <- .yml_dir_get_source("cache", "default")
      expect_true(is.null(source_val))
    }
  )
})

test_that(".yml_dir_set_source removes source when set to empty list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # First set a source
      yml_source <- list(
        local = list(
          local = list(
            path = "/path/to/data"
          )
        )
      )

      .yml_dir_set_source(yml_source, "output", "default")
      source_val <- .yml_dir_get_source("output", "default")
      expect_false(is.null(source_val))

      # Now set to empty list to remove
      .yml_dir_set_source(list(), "output", "default")
      source_val <- .yml_dir_get_source("output", "default")
      expect_true(is.null(source_val))
    }
  )
})

test_that(".yml_dir_set_source works with multiple remote types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up source with multiple remote types
      yml_source <- list(
        github = list(
          github = list(
            id = "v1.0.0"
          )
        ),
        osf = list(
          osf = list(
            id = "xyz789"
          )
        ),
        local = list(
          local = list(
            path = "/backup/data"
          )
        )
      )

      .yml_dir_set_source(yml_source, "raw-data", "default")

      # Verify all types are set
      source_val <- .yml_dir_get_source("raw-data", "default")
      expect_true("github" %in% names(source_val))
      expect_true("osf" %in% names(source_val))
      expect_true("local" %in% names(source_val))
    }
  )
})

test_that(".yml_dir_get_source_type works with valid github type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up GitHub source
      yml_source <- list(
        github = list(
          github = list(
            id = "v0.0.3",
            structure = "archive"
          )
        )
      )

      .yml_dir_set_source(yml_source, "output", "default")

      # Get source type
      source_type <- .yml_dir_get_source_type("github", "output", "default")
      expect_false(is.null(source_type))
      expect_identical(source_type$id, "v0.0.3")
      expect_identical(source_type$structure, "archive")
    }
  )
})

test_that(".yml_dir_get_source_type works with valid osf type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up OSF source
      yml_source <- list(
        osf = list(
          osf = list(
            id = "def456",
            structure = "latest"
          )
        )
      )

      .yml_dir_set_source(yml_source, "raw-data", "default")

      # Get source type
      source_type <- .yml_dir_get_source_type("osf", "raw-data", "default")
      expect_false(is.null(source_type))
      expect_identical(source_type$id, "def456")
      expect_identical(source_type$structure, "latest")
    }
  )
})

test_that(".yml_dir_get_source_type works with valid local type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up local source
      yml_source <- list(
        local = list(
          local = list(
            path = "/data/archive",
            structure = "archive"
          )
        )
      )

      .yml_dir_set_source(yml_source, "cache", "default")

      # Get source type
      source_type <- .yml_dir_get_source_type("local", "cache", "default")
      expect_false(is.null(source_type))
      expect_identical(source_type$path, "/data/archive")
      expect_identical(source_type$structure, "archive")
    }
  )
})

test_that(".yml_dir_get_source_type returns NULL when source not configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No source configured
      source_type <- .yml_dir_get_source_type("github", "output", "default")
      expect_true(is.null(source_type))

      source_type <- .yml_dir_get_source_type("osf", "raw-data", "default")
      expect_true(is.null(source_type))

      source_type <- .yml_dir_get_source_type("local", "cache", "default")
      expect_true(is.null(source_type))
    }
  )
})

test_that(".yml_dir_get_source_type returns NULL when type not in source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up source with only GitHub
      yml_source <- list(
        github = list(
          github = list(
            id = "v0.0.4"
          )
        )
      )

      .yml_dir_set_source(yml_source, "output", "default")

      # Request different types
      source_type <- .yml_dir_get_source_type("osf", "output", "default")
      expect_true(is.null(source_type))

      source_type <- .yml_dir_get_source_type("local", "output", "default")
      expect_true(is.null(source_type))
    }
  )
})

test_that(".yml_dir_get_source_type validates type parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Invalid type
      expect_error(
        .yml_dir_get_source_type("invalid", "output", "default"),
        "must be one of"
      )

      # Multiple values for type
      expect_error(
        .yml_dir_get_source_type(c("github", "osf"), "output", "default"),
        "must be a non-empty string"
      )

      # NULL type
      expect_error(
        .yml_dir_get_source_type(NULL, "output", "default"),
        "must be given"
      )
    }
  )
})

test_that(".yml_dir_get_source_type validates label parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Invalid label
      expect_error(
        .yml_dir_get_source_type("github", "invalid-label", "default"),
        "must be one of"
      )

      # Multiple values for label
      expect_error(
        .yml_dir_get_source_type("github", c("output", "cache"), "default"),
        "must be a non-empty string"
      )

      # NULL label
      expect_error(
        .yml_dir_get_source_type("github", NULL, "default"),
        "must be given"
      )
    }
  )
})

test_that(".yml_dir_get_source handles various inputs without validation", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Note: The second definition of .yml_dir_get_source (lines 35-41)
      # does not have validation, so it will call .yml_dir_get_label
      # which may handle invalid inputs differently

      # Test that it returns NULL for non-existent labels
      # (Note: .yml_dir_get_label may handle this)
      result <- .yml_dir_get_source("output", "default")
      expect_true(is.null(result))

      # Test with configured source
      yml_source <- list(github = list(github = list(id = "v1.0.0")))
      .yml_dir_set_source(yml_source, "output", "default")
      result <- .yml_dir_get_source("output", "default")
      expect_false(is.null(result))
    }
  )
})

test_that(".yml_dir_get_source works with different profiles", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test profile
      projr_profile_create("test-profile")

      # Set source in test profile
      yml_source <- list(
        github = list(
          github = list(
            id = "v1.0.0-test"
          )
        )
      )

      .yml_dir_set_source(yml_source, "output", "test-profile")

      # Verify in test profile
      source_val <- .yml_dir_get_source("output", "test-profile")
      expect_identical(source_val$github$github$id, "v1.0.0-test")

      # Verify default profile still has no source
      source_val_default <- .yml_dir_get_source("output", "default")
      expect_true(is.null(source_val_default))
    }
  )
})

test_that(".yml_dir_set_source works with different profiles", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test profile
      projr_profile_create("test-profile")

      # Set different sources in different profiles
      yml_source_default <- list(
        github = list(
          github = list(id = "v1.0.0")
        )
      )

      yml_source_test <- list(
        osf = list(
          osf = list(id = "test123")
        )
      )

      .yml_dir_set_source(yml_source_default, "raw-data", "default")
      .yml_dir_set_source(yml_source_test, "raw-data", "test-profile")

      # Verify sources are different in each profile
      source_default <- .yml_dir_get_source("raw-data", "default")
      expect_true("github" %in% names(source_default))
      expect_false("osf" %in% names(source_default))

      source_test <- .yml_dir_get_source("raw-data", "test-profile")
      expect_false("github" %in% names(source_test))
      expect_true("osf" %in% names(source_test))
    }
  )
})

test_that(".yml_dir_get_source_type works with different profiles", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test profile
      projr_profile_create("test-profile")

      # Set source in test profile
      yml_source <- list(
        local = list(
          local = list(
            path = "/test/path"
          )
        )
      )

      .yml_dir_set_source(yml_source, "cache", "test-profile")

      # Get source type from test profile
      source_type <- .yml_dir_get_source_type("local", "cache", "test-profile")
      expect_identical(source_type$path, "/test/path")

      # Verify default profile has no source
      source_type_default <- .yml_dir_get_source_type("local", "cache", "default")
      expect_true(is.null(source_type_default))
    }
  )
})

test_that(".yml_dir_set_source preserves other label properties", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set some properties on output label
      .yml_dir_nm_set_hash(TRUE, "output", "default")
      .yml_dir_set_license("MIT", "output", "default")

      # Now set source
      yml_source <- list(
        github = list(
          github = list(id = "v1.0.0")
        )
      )

      .yml_dir_set_source(yml_source, "output", "default")

      # Verify source was set
      source_val <- .yml_dir_get_source("output", "default")
      expect_false(is.null(source_val))

      # Verify other properties preserved
      hash_val <- .yml_dir_get_hash("output", "default")
      expect_identical(hash_val, TRUE)

      license_val <- .yml_dir_get_license("output", "default")
      expect_identical(license_val, "MIT")
    }
  )
})

test_that(".yml_dir_get_source_type handles empty source structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set up source with empty inner structure
      yml_source <- list(
        github = list()
      )

      .yml_dir_set_source(yml_source, "output", "default")

      # Get source type - should return NULL since github list is empty
      source_type <- .yml_dir_get_source_type("github", "output", "default")
      expect_true(is.null(source_type))
    }
  )
})
