test_that(".license_type_normalize works", {
  skip_if(.is_test_select())

  # Test common variations
  expect_identical(.license_type_normalize("ccby"), "CC-BY")
  expect_identical(.license_type_normalize("cc-by"), "CC-BY")
  expect_identical(.license_type_normalize("CC-BY"), "CC-BY")

  expect_identical(.license_type_normalize("cc0"), "CC0")
  expect_identical(.license_type_normalize("CC0"), "CC0")

  expect_identical(.license_type_normalize("apache"), "Apache-2.0")
  expect_identical(.license_type_normalize("apache-2.0"), "Apache-2.0")
  expect_identical(.license_type_normalize("Apache-2.0"), "Apache-2.0")
  expect_identical(.license_type_normalize("Apache 2.0"), "Apache-2.0")

  expect_identical(.license_type_normalize("mit"), "MIT")
  expect_identical(.license_type_normalize("MIT"), "MIT")

  expect_identical(.license_type_normalize("proprietary"), "Proprietary")
  expect_identical(.license_type_normalize("Proprietary"), "Proprietary")

  # Test unknown type
  expect_error(.license_type_normalize("unknown"), "Unknown license type")
})

test_that(".license_template_get_path returns valid paths", {
  skip_if(.is_test_select())

  # Test that templates exist
  path_ccby <- .license_template_get_path("CC-BY")
  expect_true(file.exists(path_ccby))

  path_cc0 <- .license_template_get_path("CC0")
  expect_true(file.exists(path_cc0))

  path_apache <- .license_template_get_path("Apache-2.0")
  expect_true(file.exists(path_apache))

  path_mit <- .license_template_get_path("MIT")
  expect_true(file.exists(path_mit))

  path_prop <- .license_template_get_path("Proprietary")
  expect_true(file.exists(path_prop))
})

test_that(".license_template_fill replaces placeholders", {
  skip_if(.is_test_select())

  template <- c(
    "Copyright {{YEAR}} {{AUTHORS}}",
    "License text with {{AUTHORS}} and {{YEAR}}"
  )

  filled <- .license_template_fill(template, c("John Doe", "Jane Smith"), 2024)

  expect_true(all(grepl("2024", filled)))
  expect_true(all(grepl("John Doe, Jane Smith", filled)))
  expect_false(any(grepl("\\{\\{AUTHORS\\}\\}", filled)))
  expect_false(any(grepl("\\{\\{YEAR\\}\\}", filled)))
})

test_that(".license_config_parse handles simple string", {
  skip_if(.is_test_select())

  config <- .license_config_parse("CC-BY")

  expect_identical(config$type, "CC-BY")
  expect_true(!is.null(config$authors))
  expect_true(!is.null(config$year))
  expect_true(is.integer(config$year))
})

test_that(".license_config_parse handles full config", {
  skip_if(.is_test_select())

  config <- .license_config_parse(list(
    type = "MIT",
    authors = c("Alice", "Bob"),
    year = 2023
  ))

  expect_identical(config$type, "MIT")
  expect_identical(config$authors, c("Alice", "Bob"))
  expect_identical(config$year, 2023)
})

test_that(".license_config_parse handles partial config", {
  skip_if(.is_test_select())

  config <- .license_config_parse(list(
    type = "Apache-2.0",
    authors = c("Charlie")
  ))

  expect_identical(config$type, "Apache-2.0")
  expect_identical(config$authors, "Charlie")
  expect_true(is.integer(config$year))
  expect_true(config$year >= 2024)
})

test_that(".license_dir_write creates license file", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directory
      test_dir <- file.path(dir_test, "test_output")
      dir.create(test_dir)

      # Write license
      license_config <- list(
        type = "MIT",
        authors = c("Test Author"),
        year = 2024
      )

      license_path <- .license_dir_write(test_dir, license_config)

      # Verify file was created
      expect_true(file.exists(license_path))
      expect_identical(basename(license_path), "LICENSE")

      # Read and check content
      content <- readLines(license_path, warn = FALSE)
      expect_true(any(grepl("MIT License", content)))
      expect_true(any(grepl("Test Author", content)))
      expect_true(any(grepl("2024", content)))
    }
  )
})

test_that(".license_dir_create works with yml config", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license config in yml
      .yml_dir_set_license(
        list(type = "CC-BY", authors = c("Project Team"), year = 2024),
        "output",
        "default"
      )

      # Create output directory
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      # Generate license
      result <- .license_dir_create("output", safe = FALSE, "default")

      # Verify license was created
      license_path <- file.path(output_dir, "LICENSE")
      expect_true(file.exists(license_path))

      # Check content
      content <- readLines(license_path, warn = FALSE)
      expect_true(any(grepl("Creative Commons Attribution", content)))
      expect_true(any(grepl("Project Team", content)))
    }
  )
})

test_that(".license_dir_create returns FALSE when no config", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create output directory without license config
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      # Should return FALSE
      result <- .license_dir_create("output", safe = FALSE, "default")
      expect_false(result)

      # LICENSE should not exist
      license_path <- file.path(output_dir, "LICENSE")
      expect_false(file.exists(license_path))
    }
  )
})

test_that(".build_label_get_dir_exc excludes LICENSE", {
  skip_if(.is_test_select())

  # Test that LICENSE is excluded for various labels
  exc_output <- .build_label_get_dir_exc("output")
  expect_true("LICENSE" %in% exc_output)

  exc_raw <- .build_label_get_dir_exc("raw-data")
  expect_true("LICENSE" %in% exc_raw)

  exc_cache <- .build_label_get_dir_exc("cache")
  expect_true("LICENSE" %in% exc_cache)
  expect_true("projr" %in% exc_cache)
})

test_that("LICENSE files excluded from manifest", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license config and create output
      .yml_dir_set_license("CC-BY", "output", "default")
      output_dir <- projr_path_get_dir("output", safe = TRUE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      # Create license and some content
      .license_dir_create("output", safe = TRUE, "default")
      writeLines("test content", file.path(output_dir, "test.txt"))

      # Hash the directory
      manifest <- .manifest_hash_label("output", FALSE)

      # LICENSE should not be in manifest
      expect_false(any(grepl("LICENSE", manifest$fn)))

      # test.txt should be in manifest
      expect_true(any(grepl("test.txt", manifest$fn)))
    }
  )
})
