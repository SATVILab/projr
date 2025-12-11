test_that(".yml_dir_label_class_detect functions work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test raw detection
  expect_true(.yml_dir_label_class_detect_raw("raw-data"))
  expect_true(.yml_dir_label_class_detect_raw("raw_data"))
  expect_true(.yml_dir_label_class_detect_raw("rawdata"))
  expect_false(.yml_dir_label_class_detect_raw("output"))
  expect_false(.yml_dir_label_class_detect_raw("cache"))

  # Test cache detection
  expect_true(.yml_dir_label_class_detect_cache("cache"))
  expect_true(.yml_dir_label_class_detect_cache("cache-temp"))
  expect_false(.yml_dir_label_class_detect_cache("raw-data"))
  expect_false(.yml_dir_label_class_detect_cache("output"))

  # Test output detection
  expect_true(.yml_dir_label_class_detect_output("output"))
  expect_true(.yml_dir_label_class_detect_output("output-results"))
  expect_false(.yml_dir_label_class_detect_output("raw-data"))
  expect_false(.yml_dir_label_class_detect_output("cache"))

  # Test project detection
  expect_true(.yml_dir_label_class_detect_project("project"))
  expect_false(.yml_dir_label_class_detect_project("project-data"))
  expect_false(.yml_dir_label_class_detect_project("output"))

  # Test code detection
  expect_true(.yml_dir_label_class_detect_code("code"))
  expect_false(.yml_dir_label_class_detect_code("code-archive"))
  expect_false(.yml_dir_label_class_detect_code("output"))

  # Test data detection
  expect_true(.yml_dir_label_class_detect_data("data"))
  expect_false(.yml_dir_label_class_detect_data("data-raw"))
  expect_false(.yml_dir_label_class_detect_data("output"))

  # Test docs detection
  expect_true(.yml_dir_label_class_detect_docs("docs"))
  expect_false(.yml_dir_label_class_detect_docs("docs-archive"))
  expect_false(.yml_dir_label_class_detect_docs("output"))
})

test_that(".yml_dir_label_class_get_ind works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  expect_identical(.yml_dir_label_class_get_ind("raw-data"), "raw")
  expect_identical(.yml_dir_label_class_get_ind("cache"), "cache")
  expect_identical(.yml_dir_label_class_get_ind("output"), "output")
  expect_identical(.yml_dir_label_class_get_ind("project"), "project")
  expect_identical(.yml_dir_label_class_get_ind("code"), "code")
  expect_identical(.yml_dir_label_class_get_ind("data"), "data")
  expect_identical(.yml_dir_label_class_get_ind("docs"), "docs")

  # Test error for invalid label
  expect_error(
    .yml_dir_label_class_get_ind("invalid"),
    "not valid"
  )
})

test_that(".yml_dir_label_class_get works with vectors", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  labels <- c("raw-data", "cache", "output", "docs")
  classes <- .yml_dir_label_class_get(labels)

  expect_identical(unname(classes), c("raw", "cache", "output", "docs"))
  expect_identical(length(classes), 4L)
})

test_that(".yml_dir_label_class_get_match works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  labels <- c("raw-data", "cache", "output", "docs")
  matches <- .yml_dir_label_class_get_match(labels, c("raw", "cache"))

  expect_identical(matches, c(TRUE, TRUE, FALSE, FALSE))
})

test_that(".yml_dir_complete_hash functions work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test output hash completion (NULL -> TRUE)
  expect_identical(.yml_dir_complete_hash_output(NULL), TRUE)
  expect_identical(.yml_dir_complete_hash_output(FALSE), FALSE)
  expect_identical(.yml_dir_complete_hash_output(TRUE), TRUE)

  # Test raw hash completion (NULL -> TRUE)
  expect_identical(.yml_dir_complete_hash_raw(NULL), TRUE)
  expect_identical(.yml_dir_complete_hash_raw(FALSE), FALSE)
  expect_identical(.yml_dir_complete_hash_raw(TRUE), TRUE)

  # Test cache hash completion (NULL -> FALSE)
  expect_identical(.yml_dir_complete_hash_cache(NULL), FALSE)
  expect_identical(.yml_dir_complete_hash_cache(TRUE), TRUE)
  expect_identical(.yml_dir_complete_hash_cache(FALSE), FALSE)
})

test_that(".yml_dir_complete_hash dispatches correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test dispatching to different label types
  expect_identical(.yml_dir_complete_hash(NULL, "raw-data"), TRUE)
  expect_identical(.yml_dir_complete_hash(NULL, "cache"), FALSE)
  expect_identical(.yml_dir_complete_hash(NULL, "output"), TRUE)
  expect_identical(.yml_dir_complete_hash(NULL, "docs"), TRUE)

  # Test with explicit values
  expect_identical(.yml_dir_complete_hash(FALSE, "output"), FALSE)
  expect_identical(.yml_dir_complete_hash(TRUE, "cache"), TRUE)

  # Test error for invalid label
  expect_error(
    .yml_dir_complete_hash(NULL, "invalid-label"),
    "not valid"
  )
})

test_that(".yml_dir_get_path_default works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test default paths
  expect_identical(.yml_dir_get_path_default("output"), "_output")
  expect_identical(.yml_dir_get_path_default("raw"), "_raw_data")
  expect_identical(.yml_dir_get_path_default("rawdata"), "_raw_data")
  expect_identical(.yml_dir_get_path_default("cache"), "_tmp")

  # Test non-default labels
  expect_identical(.yml_dir_get_path_default("docs"), NULL)
  expect_identical(.yml_dir_get_path_default("project"), NULL)
  expect_identical(.yml_dir_get_path_default("custom-label"), NULL)
})

test_that(".yml_dir_get_label_nm functions work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add some custom labels
      .yml_dir_add_label("_output2", label = "output-results", profile = "default")
      .yml_dir_add_label("_raw2", label = "raw-data-extra", profile = "default")

      # Test output labels
      output_labels <- .yml_dir_get_label_output("default")
      expect_true("output" %in% output_labels)
      expect_true("output-results" %in% output_labels)

      # Test raw labels
      raw_labels <- .yml_dir_get_label_raw("default")
      expect_true("raw-data" %in% raw_labels)
      expect_true("raw-data-extra" %in% raw_labels)

      # Test cache labels
      cache_labels <- .yml_dir_get_label_cache("default")
      expect_true("cache" %in% cache_labels)
    }
  )
})

test_that(".yml_dir_get_label_docs works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Default should return "docs"
      docs_labels <- .yml_dir_get_label_docs("default")
      expect_identical(docs_labels, "docs")

      # Add a custom docs label
      .yml_dir_add_label("_docs2", label = "docs-archive", profile = "default")
      docs_labels <- .yml_dir_get_label_docs("default")
      expect_true("docs-archive" %in% docs_labels)
    }
  )
})

test_that(".yml_dir_get_label_in and _out work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Input labels (raw + cache)
      in_labels <- .yml_dir_get_label_in("default")
      expect_true("raw-data" %in% in_labels)
      expect_true("cache" %in% in_labels)
      expect_false("output" %in% in_labels)

      # Output labels (output + docs + data)
      out_labels <- .yml_dir_get_label_out("default")
      expect_true("output" %in% out_labels)
      expect_true("docs" %in% out_labels)
      expect_true("data" %in% out_labels)
      expect_false("raw-data" %in% out_labels)
      expect_false("cache" %in% out_labels)
    }
  )
})

test_that(".yml_dir_get_label_artefact works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Artefact labels (in + out)
      artefact_labels <- .yml_dir_get_label_artefact("default")
      expect_true("raw-data" %in% artefact_labels)
      expect_true("cache" %in% artefact_labels)
      expect_true("output" %in% artefact_labels)
      expect_true("docs" %in% artefact_labels)
      expect_true("data" %in% artefact_labels)
    }
  )
})

test_that(".yml_dir_get_hash and _set_hash work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initially no hash setting
      hash_val <- .yml_dir_get_hash("output", "default")
      expect_true(is.null(hash_val))

      # Set hash to FALSE
      .yml_dir_nm_set_hash(FALSE, "output", "default")
      hash_val <- .yml_dir_get_hash("output", "default")
      expect_identical(hash_val, FALSE)

      # Set hash to TRUE
      .yml_dir_nm_set_hash(TRUE, "cache", "default")
      hash_val <- .yml_dir_get_hash("cache", "default")
      expect_identical(hash_val, TRUE)
    }
  )
})

test_that(".yml_dir_get_hash_complete works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with NULL (uses defaults)
      hash_complete <- .yml_dir_get_hash_complete("output", "default")
      expect_identical(hash_complete, TRUE)

      hash_complete <- .yml_dir_get_hash_complete("cache", "default")
      expect_identical(hash_complete, FALSE)

      # Test with explicit setting
      .yml_dir_nm_set_hash(FALSE, "output", "default")
      hash_complete <- .yml_dir_get_hash_complete("output", "default")
      expect_identical(hash_complete, FALSE)
    }
  )
})

test_that(".yml_dir_get_license and _set_license work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initially no license
      license_val <- .yml_dir_get_license("output", "default")
      expect_true(is.null(license_val))

      # Set simple license
      .yml_dir_set_license("MIT", "output", "default")
      license_val <- .yml_dir_get_license("output", "default")
      expect_identical(license_val, "MIT")

      # Set complex license
      license_obj <- list(type = "CC-BY", authors = c("Author"), year = 2024)
      .yml_dir_set_license(license_obj, "raw-data", "default")
      license_val <- .yml_dir_get_license("raw-data", "default")
      expect_identical(license_val$type, "CC-BY")
      expect_identical(license_val$authors, c("Author"))
      expect_identical(license_val$year, 2024)
    }
  )
})

test_that(".yml_dir_get_pkg and _set_pkg work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initially no package setting
      pkg_val <- .yml_dir_get_pkg("output", "default")
      expect_true(is.null(pkg_val))

      # Set package
      .yml_dir_set_pkg(c("pkg1", "pkg2"), "output", "default")
      pkg_val <- .yml_dir_get_pkg("output", "default")
      expect_identical(pkg_val, c("pkg1", "pkg2"))
    }
  )
})

test_that(".yml_dir_complete_pkg works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  label <- c("output", "raw-data", "cache")
  pkg <- c(TRUE, FALSE, TRUE)
  result <- .yml_dir_complete_pkg(pkg, label)

  expect_identical(result, c("output", "cache"))
})

test_that(".yml_dir_get_pkg_complete works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Non-output label should return empty
      pkg_complete <- .yml_dir_get_pkg_complete("raw-data", "default")
      expect_identical(pkg_complete, character())

      # Output label with no package setting
      pkg_complete <- .yml_dir_get_pkg_complete("output", "default")
      expect_identical(length(pkg_complete), 0L)

      # Set package and test
      .yml_dir_set_pkg(c("pkg1"), "output", "default")
      pkg_complete <- .yml_dir_get_pkg_complete("output", "default")
      expect_true(length(pkg_complete) > 0)
    }
  )
})

test_that(".yml_dir_get_output_nm and _complete work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add another output directory
      .yml_dir_add_label("_output2", label = "output-results", profile = "default")

      # Set output dependency for output
      yml_dir <- .yml_dir_get_label("output", "default")
      yml_dir$output <- "output-results"
      .yml_dir_set_label(yml_dir, "output", "default")

      # Get output name
      output_nm <- .yml_dir_get_output_nm("output", "default")
      expect_identical(output_nm, "output-results")

      # Get complete output (should exclude self)
      output_complete <- .yml_dir_get_output_nm_complete("output", "default")
      expect_true("output-results" %in% output_complete)
      expect_false("output" %in% output_complete)
    }
  )
})

test_that(".yml_dir_complete_output_lgl works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add output directories
      .yml_dir_add_label("_output2", label = "output-results", profile = "default")
      .yml_dir_add_label("_output3", label = "output-data", profile = "default")

      # Test FALSE returns empty
      result <- .yml_dir_complete_output_lgl(FALSE, "output", "default")
      expect_identical(result, character())

      # Test TRUE returns all other output labels
      result <- .yml_dir_complete_output_lgl(TRUE, "output", "default")
      expect_true("output-results" %in% result)
      expect_true("output-data" %in% result)
      expect_false("output" %in% result)
    }
  )
})

test_that(".yml_dir_complete_output_chr works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add output directories
      .yml_dir_add_label("_output2", label = "output-results", profile = "default")
      .yml_dir_add_label("_output3", label = "output-data", profile = "default")

      # Test character vector filtering
      output_vec <- c("output-results", "output-data", "non-existent")
      result <- .yml_dir_complete_output_chr(output_vec, "output", "default")

      expect_true("output-results" %in% result)
      expect_true("output-data" %in% result)
      expect_false("output" %in% result)
      expect_false("non-existent" %in% result)
    }
  )
})

test_that(".yml_dir_get_path and _raw work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test default path
      path <- .yml_dir_get_path("output", "default")
      expect_identical(path, "_output")

      # Test custom path
      .yml_dir_set_path("_custom_output", "output", "default")
      path_raw <- .yml_dir_get_path_raw("output", "default")
      expect_identical(path_raw, "_custom_output")

      path <- .yml_dir_get_path("output", "default")
      expect_identical(path, "_custom_output")
    }
  )
})

test_that(".yml_dir_add_label works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add simple label
      .yml_dir_add_label("_new_output", label = "output-new", profile = "default")
      yml_dir <- .yml_dir_get_label("output-new", "default")
      expect_identical(yml_dir$path, "_new_output")

      # Add label with package
      .yml_dir_add_label(
        "_pkg_output",
        package = c("pkg1", "pkg2"),
        label = "output-pkg",
        profile = "default"
      )
      yml_dir <- .yml_dir_get_label("output-pkg", "default")
      expect_identical(yml_dir$path, "_pkg_output")
      expect_identical(yml_dir$package, c("pkg1", "pkg2"))

      # Add label with output dependency
      .yml_dir_add_label(
        "_dep_output",
        output = "output-new",
        label = "output-dep",
        profile = "default"
      )
      yml_dir <- .yml_dir_get_label("output-dep", "default")
      expect_identical(yml_dir$output, "output-new")
    }
  )
})

test_that(".yml_dir_get_complete_label works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with empty yml_dir
  empty_yml <- list()
  result <- .yml_dir_get_complete_label(empty_yml)
  expect_true("raw-data" %in% names(result))
  expect_true("cache" %in% names(result))
  expect_true("output" %in% names(result))

  # Test with partial yml_dir
  partial_yml <- list(
    "output" = list(path = "_output")
  )
  result <- .yml_dir_get_complete_label(partial_yml)
  expect_true("raw-data" %in% names(result))
  expect_true("cache" %in% names(result))
  expect_true("output" %in% names(result))

  # Test with all required labels present
  full_yml <- list(
    "raw-data" = list(path = "_raw"),
    "cache" = list(path = "_cache"),
    "output" = list(path = "_out")
  )
  result <- .yml_dir_get_complete_label(full_yml)
  expect_identical(length(result), 3L)
  expect_identical(result$output$path, "_out")
})

test_that(".yml_dir_set_label_empty works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add a custom label
      .yml_dir_add_label("_custom", label = "output-custom", profile = "default")
      yml_dir <- .yml_dir_get_label("output-custom", "default")
      expect_false(is.null(yml_dir))

      # Remove it using empty setter
      .yml_dir_set_label_empty("output-custom", "default")
      yml_dir <- .yml_dir_get_label("output-custom", "default")
      expect_true(is.null(yml_dir))
    }
  )
})

test_that(".yml_dir_set_label_non_empty works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create yml object
      yml_new <- list(
        path = "_new_path",
        hash = TRUE,
        license = "MIT"
      )

      # Set it
      .yml_dir_set_label_non_empty(yml_new, "output", "default")
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir$path, "_new_path")
      expect_identical(yml_dir$hash, TRUE)
      expect_identical(yml_dir$license, "MIT")
    }
  )
})

test_that(".yml_dir_set_label dispatcher works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add a label
      .yml_dir_add_label("_test", label = "output-test", profile = "default")

      # Set to NULL (should remove)
      .yml_dir_set_label(NULL, "output-test", "default")
      yml_dir <- .yml_dir_get_label("output-test", "default")
      expect_true(is.null(yml_dir))

      # Set to list (should add)
      yml_new <- list(path = "_new")
      .yml_dir_set_label(yml_new, "output", "default")
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir$path, "_new")
    }
  )
})

test_that(".yml_dir_get and _set work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Get current yml_dir
      yml_dir <- .yml_dir_get("default")
      expect_true("raw-data" %in% names(yml_dir))
      expect_true("cache" %in% names(yml_dir))
      expect_true("output" %in% names(yml_dir))

      # Modify and set
      yml_dir$`output-new` <- list(path = "_new")
      .yml_dir_set(yml_dir, "default")

      # Verify change
      yml_dir_new <- .yml_dir_get("default")
      expect_true("output-new" %in% names(yml_dir_new))
      expect_identical(yml_dir_new$`output-new`$path, "_new")
    }
  )
})

test_that(".yml_dir_set_docs works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set docs path
      .yml_dir_set_docs("_documentation", "default")
      docs_path <- .yml_dir_get_path("docs", "default")
      expect_identical(docs_path, "_documentation")
    }
  )
})

test_that(".yml_dir_get_docs_rel_if_within_cache works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test path outside cache
      external_path <- "_docs"
      result <- .yml_dir_get_docs_rel_if_within_cache(external_path, "default")
      expect_identical(result, external_path)

      # Test path inside cache
      cache_dir <- .dir_get_cache_auto_version(profile = "default")
      internal_path <- file.path(cache_dir, "docs")
      result <- .yml_dir_get_docs_rel_if_within_cache(internal_path, "default")
      expect_identical(as.character(result), "docs")
    }
  )
})
