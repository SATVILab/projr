# Tests for version function validation and edge cases

test_that(".version_check_error_free returns correct values", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Valid versions should return TRUE
      expect_true(.version_check_error_free("1.2.3-4"))
      expect_true(.version_check_error_free("0.0.0"))
      expect_true(.version_check_error_free("v1.2.3"))

      # Invalid versions should return FALSE
      expect_false(.version_check_error_free("invalid"))
      expect_false(.version_check_error_free(NULL))
      expect_false(.version_check_error_free(NA))
      expect_false(.version_check_error_free(""))
      expect_false(.version_check_error_free(123))
      expect_false(.version_check_error_free(c("1.2.3", "4.5.6")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".version_v_rm validates input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid input
  expect_identical(.version_v_rm("v1.2.3"), "1.2.3")
  expect_identical(.version_v_rm("1.2.3"), "1.2.3")
  expect_identical(.version_v_rm("V1.2.3"), "1.2.3")
  expect_identical(.version_v_rm("vv1.2.3"), "1.2.3")

  # Invalid input should error
  expect_error(.version_v_rm(NULL))
  expect_error(.version_v_rm(NA))
  expect_error(.version_v_rm(123))
  expect_error(.version_v_rm(c("v1.2.3", "v4.5.6")))
})

test_that(".version_v_add validates input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid input
  expect_identical(.version_v_add("1.2.3"), "v1.2.3")
  expect_identical(.version_v_add("v1.2.3"), "v1.2.3")

  # Invalid input should error
  expect_error(.version_v_add(NULL))
  expect_error(.version_v_add(NA))
  expect_error(.version_v_add(123))
  expect_error(.version_v_add(c("1.2.3", "4.5.6")))
})

test_that(".version_get_earliest validates input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid input
  expect_identical(
    as.character(.version_get_earliest(c("v1.2.3", "v2.3.4", "v0.1.0"))),
    "0.1.0"
  )
  expect_identical(
    as.character(.version_get_earliest("v5.0.0")),
    "5.0.0"
  )

  # Invalid input should error
  expect_error(.version_get_earliest(NULL))
  expect_error(.version_get_earliest(character(0)))
  expect_error(.version_get_earliest(123))
})

test_that(".version_get_latest validates input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid input
  expect_identical(
    as.character(.version_get_latest(c("v1.2.3", "v2.3.4", "v0.1.0"))),
    "2.3.4"
  )
  expect_identical(
    as.character(.version_get_latest("v5.0.0")),
    "5.0.0"
  )

  # Invalid input should error
  expect_error(.version_get_latest(NULL))
  expect_error(.version_get_latest(character(0)))
  expect_error(.version_get_latest(123))
})

test_that(".version_append validates input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("1.2.3")

      # Valid input
      expect_identical(.version_append("/some/path"), "/some/path/v1.2.3")

      # Invalid input should error
      expect_error(.version_append(NULL))
      expect_error(.version_append(123))
      expect_error(.version_append(c("/path1", "/path2")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".version_current_vec_get_init_file validates VERSION file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove DESCRIPTION to force VERSION file usage
      if (file.exists("DESCRIPTION")) {
        file.remove("DESCRIPTION")
      }

      # Missing VERSION file
      if (file.exists("VERSION")) {
        file.remove("VERSION")
      }
      expect_error(.version_current_vec_get_init_file(), "VERSION file not found")

      # Empty VERSION file (writeLines with empty string creates a file with empty line)
      writeLines("", "VERSION")
      expect_error(.version_current_vec_get_init_file(), "VERSION file contains invalid content")

      # VERSION file with only whitespace (should be treated as invalid)
      writeLines("   ", "VERSION")
      expect_error(.version_current_vec_get_init_file(), "VERSION file contains invalid content")

      # Valid VERSION file
      writeLines("v1.2.3-4", "VERSION")
      result <- .version_current_vec_get_init_file()
      expect_identical(result, c("1", "2", "3", "4"))

      # Valid VERSION file without 'v'
      writeLines("2.3.4", "VERSION")
      result <- .version_current_vec_get_init_file()
      expect_identical(result, c("2", "3", "4"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".version_concat validates input and handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid inputs with numeric vectors (common usage)
  expect_identical(.version_concat(c(1, 2, 3), c(".", ".")), "1.2.3")
  expect_identical(.version_concat(c(1, 2, 3, 4), c(".", ".", "-")), "1.2.3-4")

  # Valid inputs with character vectors
  expect_identical(.version_concat(c("1", "2", "3"), c(".", ".")), "1.2.3")

  # Single element version (no separator needed)
  expect_identical(.version_concat("5", character(0)), "5")
  expect_identical(.version_concat(5, character(0)), "5")

  # Invalid inputs should error
  expect_error(.version_concat(character(0), c(".", ".")), "version_vec must have at least one element")
  expect_error(.version_concat(c("1", "2"), character(0)), "split_vec must have at least one element")
})

test_that(".version_format_list_get validates version format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Valid format (default)
      result <- .version_format_list_get(NULL)
      expect_true(is.list(result))
      expect_true("component" %in% names(result))
      expect_true("sep" %in% names(result))

      # Set a different valid format
      .yml_metadata_set_version_format("major.minor-dev", NULL)
      result <- .version_format_list_get(NULL)
      expect_identical(result$component, c("major", "minor", "dev"))
      expect_identical(result$sep, c(".", "-"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("version functions handle edge cases in version parsing", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with different version formats
      .yml_metadata_set_version_format("major.dev", NULL)
      projr_version_set("5.1")
      expect_identical(projr_version_get(), "5.1")

      .yml_metadata_set_version_format("major.minor.patch-dev", NULL)
      projr_version_set("0.0.0-1")
      expect_identical(projr_version_get(), "0.0.0-1")

      # Test version with leading 'v'
      projr_version_set("v2.3.4")
      expect_identical(projr_version_get(), "2.3.4")

      # Test version bump after set
      projr_version_set("1.0.0")
      expect_identical(projr_version_get(), "1.0.0")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_version_format_set_check validates version formats correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid formats with "dev" suffix should pass
  expect_true(.yml_version_format_set_check("major.minor.patch-dev"))
  expect_true(.yml_version_format_set_check("major.minor.patch.dev"))
  expect_true(.yml_version_format_set_check("major.minor-dev"))
  expect_true(.yml_version_format_set_check("major.minor.dev"))
  expect_true(.yml_version_format_set_check("major-dev"))
  expect_true(.yml_version_format_set_check("major.dev"))

  # Valid formats with "9000" suffix should pass
  expect_true(.yml_version_format_set_check("major.minor.patch-9000"))
  expect_true(.yml_version_format_set_check("major.minor.patch.9000"))
  expect_true(.yml_version_format_set_check("major.minor-9000"))
  expect_true(.yml_version_format_set_check("major.minor.9000"))
  expect_true(.yml_version_format_set_check("major-9000"))
  expect_true(.yml_version_format_set_check("major.9000"))

  # Valid formats with "1" suffix should pass
  expect_true(.yml_version_format_set_check("major.minor.patch-1"))
  expect_true(.yml_version_format_set_check("major.minor.patch.1"))
  expect_true(.yml_version_format_set_check("major.minor-1"))
  expect_true(.yml_version_format_set_check("major.minor.1"))
  expect_true(.yml_version_format_set_check("major-1"))
  expect_true(.yml_version_format_set_check("major.1"))

  # NULL should return invisible TRUE
  expect_true(.yml_version_format_set_check(NULL))

  # Empty character vector should return invisible TRUE
  expect_true(.yml_version_format_set_check(character(0)))

  # Invalid formats should error
  expect_error(.yml_version_format_set_check("invalid.format"))
  expect_error(.yml_version_format_set_check("major.minor.patch"))
  expect_error(.yml_version_format_set_check("major.minor.patch-dev.extra"))
  expect_error(.yml_version_format_set_check("patch.minor.major-dev"))
  expect_error(.yml_version_format_set_check("major-minor-dev"))
  expect_error(.yml_version_format_set_check("major_minor_patch-dev"))
  expect_error(.yml_version_format_set_check("major.minor.patch-2"))
  expect_error(.yml_version_format_set_check("major.minor.patch-99"))

  # Non-string inputs should error
  expect_error(.yml_version_format_set_check(123))
  expect_error(.yml_version_format_set_check(TRUE))
  expect_error(.yml_version_format_set_check(list("major.minor.patch-dev")))

  # Multiple strings (vector length > 1) should error
  expect_error(.yml_version_format_set_check(c("major.minor.patch-dev", "major.dev")))
  expect_error(.yml_version_format_set_check(c("major.dev", "major.minor-dev")))
})

test_that(".version_format_list_get handles error cases correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set a format with invalid separators (multiple consecutive dots)
      yml_projr <- .yml_get_default_raw()
      yml_projr[["metadata"]][["version-format"]] <- "major..minor-dev"
      .yml_set(yml_projr)
      expect_error(
        .version_format_list_get(NULL),
        "is not valid"
      )

      # Set a format with text between components (not just punctuation)
      yml_projr[["metadata"]][["version-format"]] <- "majorXminor-dev"
      .yml_set(yml_projr)
      expect_error(
        .version_format_list_get(NULL),
        "is not valid"
      )

      # Set a format with whitespace separator
      yml_projr[["metadata"]][["version-format"]] <- "major minor-dev"
      .yml_set(yml_projr)
      expect_error(
        .version_format_list_get(NULL),
        "is not valid"
      )

      # Reset to valid format
      .yml_metadata_set_version_format("major.minor.patch-dev", NULL)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".version_normalize pads versions correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Single component versions should be padded to major.minor
  expect_identical(.version_normalize("1"), "1.0")
  expect_identical(.version_normalize("5"), "5.0")
  expect_identical(.version_normalize("10"), "10.0")

  # Two component versions should remain unchanged
  expect_identical(.version_normalize("1.2"), "1.2")
  expect_identical(.version_normalize("5.0"), "5.0")
  expect_identical(.version_normalize("10.95"), "10.95")

  # Three component versions should remain unchanged
  expect_identical(.version_normalize("1.2.3"), "1.2.3")
  expect_identical(.version_normalize("0.0.1"), "0.0.1")
  expect_identical(.version_normalize("10.20.30"), "10.20.30")

  # Four component versions (with dev) should convert dashes to dots
  expect_identical(.version_normalize("1.2.3-4"), "1.2.3.4")
  expect_identical(.version_normalize("0.0.1-1"), "0.0.1.1")

  # Mixed separators should be normalized to dots
  expect_identical(.version_normalize("1-2"), "1.2")
  expect_identical(.version_normalize("5-0"), "5.0")

  # Invalid input should error
  expect_error(.version_normalize(NULL))
  expect_error(.version_normalize(123))
  expect_error(.version_normalize(c("1.2", "3.4")))
})

test_that(".version_get_earliest works with variable-length versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Mix of single, two, and three component versions
  expect_identical(
    as.character(.version_get_earliest(c("v2", "v1.5", "v1.2.3"))),
    "1.2.3"
  )

  # Single component should work
  expect_identical(
    as.character(.version_get_earliest(c("v2", "v1", "v3"))),
    "1.0"
  )

  # Two component versions
  expect_identical(
    as.character(.version_get_earliest(c("v1.2", "v1.5", "v0.9"))),
    "0.9"
  )

  # Mix including dev versions
  expect_identical(
    as.character(.version_get_earliest(c("v1.2.3-4", "v1.2", "v1"))),
    "1.0"
  )
})

test_that(".version_get_latest works with variable-length versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Mix of single, two, and three component versions
  expect_identical(
    as.character(.version_get_latest(c("v2", "v1.5", "v1.2.3"))),
    "2.0"
  )

  # Single component should work
  expect_identical(
    as.character(.version_get_latest(c("v2", "v1", "v3"))),
    "3.0"
  )

  # Two component versions
  expect_identical(
    as.character(.version_get_latest(c("v1.2", "v1.5", "v0.9"))),
    "1.5"
  )

  # Mix including dev versions
  expect_identical(
    as.character(.version_get_latest(c("v1.2.3-4", "v1.2", "v1"))),
    "1.2.3.4"
  )
})

test_that(".version_is_earlier works with variable-length versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Single component vs two component
  expect_true(.version_is_earlier("v1", "v1.5"))
  expect_true(.version_is_earlier("v1", "v2"))
  expect_false(.version_is_earlier("v2", "v1.5"))

  # Single component vs three component
  expect_true(.version_is_earlier("v1", "v1.0.1"))
  expect_false(.version_is_earlier("v2", "v1.2.3"))

  # Two component vs three component
  expect_true(.version_is_earlier("v1.2", "v1.2.1"))
  expect_false(.version_is_earlier("v1.3", "v1.2.9"))

  # Equal versions of different lengths (1.2.0 == 1.2)
  expect_false(.version_is_earlier("v1.2.0", "v1.2"))
  expect_false(.version_is_earlier("v1.2", "v1.2.0"))

  # Dev versions
  expect_true(.version_is_earlier("v1.2.3-1", "v1.2.3-2"))
  expect_true(.version_is_earlier("v1.2-1", "v1.2.3"))
})
