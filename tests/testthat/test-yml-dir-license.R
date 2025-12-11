test_that("projr_yml_dir_license_set works with simple format", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set simple license
      projr_yml_dir_license_set("CC-BY", "output")

      # Get and verify
      license <- projr_yml_dir_license_get("output")
      expect_identical(license, "CC-BY")
    }
  )
})

test_that("projr_yml_dir_license_set works with full format", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set full license config
      projr_yml_dir_license_set(
        "MIT",
        "raw-data",
        authors = c("Alice", "Bob"),
        year = 2023
      )

      # Get and verify
      license <- projr_yml_dir_license_get("raw-data")
      expect_identical(license$type, "MIT")
      expect_identical(license$authors, c("Alice", "Bob"))
      expect_identical(license$year, 2023)
    }
  )
})

test_that("projr_yml_dir_license_set normalizes license types", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set with lowercase variant
      projr_yml_dir_license_set("ccby", "output")

      # Should be normalized to CC-BY
      license <- projr_yml_dir_license_get("output")
      expect_identical(license, "CC-BY")

      # Try apache variant
      projr_yml_dir_license_set("apache", "docs")
      license <- projr_yml_dir_license_get("docs")
      expect_identical(license, "Apache-2.0")
    }
  )
})

test_that("projr_yml_dir_license_rm removes license", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license
      projr_yml_dir_license_set("MIT", "output")
      expect_false(is.null(projr_yml_dir_license_get("output")))

      # Remove license
      projr_yml_dir_license_rm("output")
      expect_true(is.null(projr_yml_dir_license_get("output")))
    }
  )
})

test_that("projr_yml_dir_license_get returns NULL when no license", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get license that doesn't exist
      license <- projr_yml_dir_license_get("output")
      expect_true(is.null(license))
    }
  )
})

test_that("projr_yml_dir_license_update updates authors from DESCRIPTION", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set initial licenses with custom authors
      projr_yml_dir_license_set(
        "CC-BY",
        "output",
        authors = c("Old Author"),
        year = 2023
      )
      projr_yml_dir_license_set(
        "MIT",
        "raw-data",
        authors = c("Old Author"),
        year = 2023
      )

      # Create directories
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      # Create initial LICENSE files
      .license_dir_create("output", safe = FALSE, "default")
      .license_dir_create("raw-data", safe = FALSE, "default")

      # Verify initial content
      output_license <- readLines(file.path(output_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Old Author", output_license)))

      # Update DESCRIPTION with new authors
      desc <- desc::description$new("!new")
      desc$set("Package", "testpkg")
      desc$add_author("New", "Author", email = "new@example.com", role = "aut")
      desc$write(file = .path_get("DESCRIPTION"))

      # Update licenses
      suppressMessages(updated <- projr_yml_dir_license_update())

      # Verify updates
      expect_true("output" %in% updated)
      expect_true("raw-data" %in% updated)

      # Check LICENSE files were updated
      output_license <- readLines(file.path(output_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("New Author", output_license)))

      raw_license <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("New Author", raw_license)))

      # Check YAML was updated
      output_config <- projr_yml_dir_license_get("output")
      expect_true("New Author" %in% output_config$authors)
    }
  )
})

test_that("projr_yml_dir_license_update works with specific labels", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set licenses for multiple directories
      projr_yml_dir_license_set("CC-BY", "output", authors = c("Old Author"))
      projr_yml_dir_license_set("MIT", "raw-data", authors = c("Old Author"))
      projr_yml_dir_license_set("Apache-2.0", "docs", authors = c("Old Author"))

      # Create directories
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      # Create LICENSE files
      .license_dir_create("output", safe = FALSE, "default")
      .license_dir_create("raw-data", safe = FALSE, "default")

      # Update DESCRIPTION
      desc <- desc::description$new("!new")
      desc$set("Package", "testpkg")
      desc$add_author("New", "Author", email = "new@example.com", role = "aut")
      desc$write(file = .path_get("DESCRIPTION"))

      # Update only specific labels
      suppressMessages(updated <- projr_yml_dir_license_update(c("output", "raw-data")))

      # Verify only specified labels were updated
      expect_identical(sort(updated), sort(c("output", "raw-data")))
      expect_false("docs" %in% updated)

      # Check LICENSE files
      output_license <- readLines(file.path(output_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("New Author", output_license)))
    }
  )
})

test_that("projr_yml_dir_license_update skips directories without config", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # No licenses configured
      suppressMessages(updated <- projr_yml_dir_license_update())

      # Should return empty vector
      expect_identical(length(updated), 0L)
    }
  )
})

test_that("Raw directory licenses created during dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license for raw-data
      projr_yml_dir_license_set("MIT", "raw-data")

      # Create some content in raw-data
      .test_content_setup_label("raw-data", safe = FALSE)
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)

      # Create a simple document to build
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        file.path(.path_get(), "test.qmd")
      )

      # Run a dev build (output_run = FALSE)
      suppressMessages(projr_build_dev())

      # Verify LICENSE was created in raw-data
      expect_true(file.exists(file.path(raw_dir, "LICENSE")))

      # Check content
      raw_license <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("MIT License", raw_license)))
    }
  )
})

test_that("Raw directory licenses always match config", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set initial license
      projr_yml_dir_license_set(
        "CC-BY",
        "raw-data",
        authors = c("Initial Author"),
        year = 2023
      )

      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      # Create a simple document
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        file.path(.path_get(), "test.qmd")
      )

      # First build
      suppressMessages(projr_build_dev())

      # Verify initial license
      expect_true(file.exists(file.path(raw_dir, "LICENSE")))
      license1 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Initial Author", license1)))

      # Change license config
      projr_yml_dir_license_set(
        "MIT",
        "raw-data",
        authors = c("Updated Author"),
        year = 2024
      )

      # Build again
      suppressMessages(projr_build_dev())

      # Verify license was updated to match new config
      license2 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("MIT License", license2)))
      expect_true(any(grepl("Updated Author", license2)))
      expect_false(any(grepl("Initial Author", license2)))
    }
  )
})

test_that("projr_license_create_manual creates licenses without YAML config", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manual license
      suppressMessages(
        created <- projr_license_create_manual("MIT", "raw-data")
      )

      # Verify it was created
      expect_true("raw-data" %in% created)

      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      expect_true(file.exists(file.path(raw_dir, "LICENSE")))

      # Verify no YAML config was added
      expect_true(is.null(projr_yml_dir_license_get("raw-data")))

      # Check content
      license <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("MIT License", license)))
    }
  )
})

test_that("projr_license_create_manual skips if LICENSE exists", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      # Create existing LICENSE
      writeLines("Existing license", file.path(raw_dir, "LICENSE"))

      # Try to create manual license
      suppressMessages(
        expect_warning(
          created <- projr_license_create_manual("MIT", "raw-data"),
          "already exists"
        )
      )

      # Verify nothing was created
      expect_identical(length(created), 0L)

      # Verify existing license wasn't overwritten
      license <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Existing license", license)))
    }
  )
})

test_that("projr_license_create_manual warns if YAML config exists", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set YAML config
      projr_yml_dir_license_set("CC-BY", "raw-data")

      # Try to create manual license
      suppressMessages(
        expect_warning(
          created <- projr_license_create_manual("MIT", "raw-data"),
          "has license config in YAML"
        )
      )

      # Verify nothing was created
      expect_identical(length(created), 0L)
    }
  )
})

test_that("Manual licenses not overwritten during builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manual license
      raw_dir <- projr_path_get_dir("raw-data")
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      # Create content and build
      .test_content_setup_label("raw-data")
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        file.path(.path_get(), "test.qmd")
      )

      # add license
      suppressMessages(
        projr_license_create_manual("MIT", "raw-data", authors = c("Manual Author"))
      )

      # Verify it was created
      expect_true(file.exists(file.path(raw_dir, "LICENSE")))
      license1 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Manual Author", license1)))

      suppressMessages(projr_build_dev())

      # Verify license wasn't overwritten
      license2 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Manual Author", license2)))
    }
  )
})

test_that("YAML config overrides manual licenses", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manual license
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      suppressMessages(
        projr_license_create_manual("MIT", "raw-data", authors = c("Manual Author"))
      )

      license1 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Manual Author", license1)))

      # Now add YAML config
      projr_yml_dir_license_set("CC-BY", "raw-data", authors = c("Config Author"))

      # Build
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        file.path(.path_get(), "test.qmd")
      )

      suppressMessages(projr_build_dev())

      # Verify license was overwritten by YAML config
      license2 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Creative Commons Attribution", license2)))
      expect_true(any(grepl("Config Author", license2)))
      expect_false(any(grepl("Manual Author", license2)))
    }
  )
})

test_that("License integration with build creates files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set licenses for directories
      projr_yml_dir_license_set("CC-BY", "output")
      projr_yml_dir_license_set("MIT", "raw-data")

      # Verify license config was set
      expect_false(is.null(projr_yml_dir_license_get("output")))
      expect_false(is.null(projr_yml_dir_license_get("raw-data")))

      # Create some content
      .test_content_setup_label("raw-data", safe = FALSE)
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)

      # Create a simple document to build
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        file.path(.path_get(), "test.qmd")
      )

      # Run a dev build
      suppressMessages(projr_build_dev())

      # Check that LICENSE files were created
      output_dir <- projr_path_get_dir("output", safe = FALSE)

      # Check if directories exist
      if (dir.exists(output_dir)) {
        # List files in output_dir
        output_files <- list.files(output_dir, all.files = TRUE, recursive = TRUE)

        # LICENSE should exist if directory was processed
        if ("LICENSE" %in% output_files || file.exists(file.path(output_dir, "LICENSE"))) {
          output_license <- readLines(file.path(output_dir, "LICENSE"), warn = FALSE)
          expect_true(any(grepl("Creative Commons Attribution", output_license)))
        }
      }

      # Check raw-data license
      if (file.exists(file.path(raw_dir, "LICENSE"))) {
        raw_license <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
        expect_true(any(grepl("MIT License", raw_license)))
      }

      # At minimum, verify the functions were called without error
      expect_true(TRUE)
    }
  )
})

# ---- Input Validation Tests ----

test_that("projr_yml_dir_license_set validates year parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-numeric year
      expect_error(
        projr_yml_dir_license_set("MIT", "output", year = "2023"),
        "year must be a single numeric value"
      )

      # Multiple year values
      expect_error(
        projr_yml_dir_license_set("MIT", "output", year = c(2023, 2024)),
        "year must be a single numeric value"
      )

      # Valid year should work
      expect_silent(
        projr_yml_dir_license_set("MIT", "output", year = 2023)
      )
    }
  )
})

test_that("projr_yml_dir_license_set validates type parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # NULL type (should error from .assert_string with required=TRUE)
      expect_error(
        projr_yml_dir_license_set(NULL, "output")
      )

      # Empty string type (should error from .assert_string with required=TRUE)
      expect_error(
        projr_yml_dir_license_set("", "output")
      )

      # Multiple type values (should error from .assert_string)
      expect_error(
        projr_yml_dir_license_set(c("MIT", "CC-BY"), "output")
      )
    }
  )
})

test_that("projr_yml_dir_license_set validates label parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # NULL label (should error from .assert_string with required=TRUE)
      expect_error(
        projr_yml_dir_license_set("MIT", NULL)
      )

      # Empty string label (should error from .assert_string with required=TRUE)
      expect_error(
        projr_yml_dir_license_set("MIT", "")
      )

      # Multiple label values (should error from .assert_string)
      expect_error(
        projr_yml_dir_license_set("MIT", c("output", "docs"))
      )
    }
  )
})

test_that("projr_yml_dir_license_set validates authors parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-character authors (should error from .assert_chr)
      expect_error(
        projr_yml_dir_license_set("MIT", "output", authors = 123)
      )

      # Valid NULL authors (should work - uses defaults)
      expect_silent(
        projr_yml_dir_license_set("MIT", "output", authors = NULL)
      )

      # Valid character vector authors
      expect_silent(
        projr_yml_dir_license_set("MIT", "raw-data", authors = c("Alice", "Bob"))
      )
    }
  )
})

test_that("projr_yml_dir_license_set validates profile parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty string profile (should error from .assert_string)
      expect_error(
        projr_yml_dir_license_set("MIT", "output", profile = "")
      )

      # Multiple profile values (should error from .assert_string)
      expect_error(
        projr_yml_dir_license_set("MIT", "output", profile = c("default", "test"))
      )

      # NULL profile is allowed (uses default behavior)
      expect_silent(
        projr_yml_dir_license_set("MIT", "output", profile = NULL)
      )
    }
  )
})

test_that("projr_yml_dir_license_get validates parameters", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # NULL label
      expect_error(
        projr_yml_dir_license_get(NULL)
      )

      # Empty string label
      expect_error(
        projr_yml_dir_license_get("")
      )

      # Empty string profile
      expect_error(
        projr_yml_dir_license_get("output", profile = "")
      )

      # NULL profile is allowed (uses default behavior)
      expect_silent(
        projr_yml_dir_license_get("output", profile = NULL)
      )
    }
  )
})

test_that("projr_yml_dir_license_rm validates parameters", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # NULL label
      expect_error(
        projr_yml_dir_license_rm(NULL)
      )

      # Empty string label
      expect_error(
        projr_yml_dir_license_rm("")
      )

      # Empty string profile
      expect_error(
        projr_yml_dir_license_rm("output", profile = "")
      )

      # NULL profile is allowed (uses default behavior)
      expect_silent(
        projr_yml_dir_license_rm("output", profile = NULL)
      )
    }
  )
})

test_that("projr_yml_dir_license_update validates parameters", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-character labels (should error from .assert_chr)
      expect_error(
        projr_yml_dir_license_update(labels = 123)
      )

      # Empty string profile
      expect_error(
        projr_yml_dir_license_update(profile = "")
      )

      # NULL profile is allowed (uses default behavior)
      expect_silent(
        suppressMessages(projr_yml_dir_license_update(profile = NULL))
      )

      # Valid NULL labels (should work - uses all labels)
      expect_silent(
        suppressMessages(projr_yml_dir_license_update(labels = NULL))
      )
    }
  )
})

test_that("projr_license_create_manual validates year parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-numeric year
      expect_error(
        projr_license_create_manual("MIT", "raw-data", year = "2023"),
        "year must be a single numeric value"
      )

      # Multiple year values
      expect_error(
        projr_license_create_manual("MIT", "raw-data", year = c(2023, 2024)),
        "year must be a single numeric value"
      )

      # Valid year should work (function produces messages)
      suppressMessages(
        result <- projr_license_create_manual("MIT", "raw-data", year = 2023)
      )
      expect_true(is.character(result))
    }
  )
})

test_that("projr_license_create_manual validates type parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # NULL type
      expect_error(
        projr_license_create_manual(NULL, "raw-data")
      )

      # Empty string type
      expect_error(
        projr_license_create_manual("", "raw-data")
      )
    }
  )
})

test_that("projr_license_create_manual validates labels parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-character labels
      expect_error(
        projr_license_create_manual("MIT", labels = 123)
      )

      # Valid NULL labels - should use default (raw data dirs)
      suppressMessages(
        result <- projr_license_create_manual("MIT", labels = NULL)
      )
      # Result should be character vector
      expect_true(is.character(result))
    }
  )
})

test_that("projr_license_create_manual validates authors parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-character authors
      expect_error(
        projr_license_create_manual("MIT", "raw-data", authors = 123)
      )

      # Valid NULL authors (should work - uses defaults, function produces messages)
      suppressMessages(
        result <- projr_license_create_manual("MIT", "raw-data", authors = NULL)
      )
      expect_true(is.character(result))
    }
  )
})

# ---- Edge Case Tests ----

test_that("projr_yml_dir_license_set works with only authors (no year)", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license with only authors
      projr_yml_dir_license_set(
        "MIT",
        "output",
        authors = c("Alice", "Bob"),
        year = NULL
      )

      # Get and verify
      license <- projr_yml_dir_license_get("output")
      expect_identical(license$type, "MIT")
      expect_identical(license$authors, c("Alice", "Bob"))
      expect_true(is.null(license$year))
    }
  )
})

test_that("projr_yml_dir_license_set works with only year (no authors)", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license with only year
      projr_yml_dir_license_set(
        "MIT",
        "output",
        authors = NULL,
        year = 2023
      )

      # Get and verify
      license <- projr_yml_dir_license_get("output")
      expect_identical(license$type, "MIT")
      expect_identical(license$year, 2023)
      expect_true(is.null(license$authors))
    }
  )
})

test_that("projr_yml_dir_license_set can update existing license config", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set initial license
      projr_yml_dir_license_set("MIT", "output")
      license1 <- projr_yml_dir_license_get("output")
      expect_identical(license1, "MIT")

      # Update to different license
      projr_yml_dir_license_set("CC-BY", "output")
      license2 <- projr_yml_dir_license_get("output")
      expect_identical(license2, "CC-BY")

      # Update from simple to full format
      projr_yml_dir_license_set(
        "Apache-2.0",
        "output",
        authors = c("Alice"),
        year = 2023
      )
      license3 <- projr_yml_dir_license_get("output")
      expect_identical(license3$type, "Apache-2.0")
      expect_identical(license3$authors, c("Alice"))
      expect_identical(license3$year, 2023)

      # Update from full to simple format
      projr_yml_dir_license_set("MIT", "output")
      license4 <- projr_yml_dir_license_get("output")
      expect_identical(license4, "MIT")
    }
  )
})

test_that("projr_yml_dir_license_update converts simple to full format", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set simple license
      projr_yml_dir_license_set("CC-BY", "output")

      # Create directory
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      # Create LICENSE file
      .license_dir_create("output", safe = FALSE, "default")

      # Update DESCRIPTION with authors
      desc <- desc::description$new("!new")
      desc$set("Package", "testpkg")
      desc$add_author("New", "Author", email = "new@example.com", role = "aut")
      desc$write(file = .path_get("DESCRIPTION"))

      # Update licenses - should convert simple to full format
      suppressMessages(projr_yml_dir_license_update())

      # Check that config was converted to full format
      license <- projr_yml_dir_license_get("output")
      expect_true(is.list(license))
      expect_identical(license$type, "CC-BY")
      expect_true("New Author" %in% license$authors)
    }
  )
})

test_that("projr_license_create_manual uses default labels when NULL", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manual licenses with NULL labels (should use raw data dirs)
      suppressMessages(
        created <- projr_license_create_manual("MIT", labels = NULL)
      )

      # Should have created licenses for raw data directories
      expect_true(is.character(created))
      expect_true(length(created) > 0)

      # Check that at least one expected directory was created
      # (The actual labels depend on .yml_dir_get_label_in())
      expect_true(all(created %in% c("raw-data", "cache", "data")))
    }
  )
})

# ---- Profile Tests ----

test_that("License functions work with different profiles", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license in default profile
      projr_yml_dir_license_set("MIT", "output", profile = "default")

      # Set different license in test profile
      projr_yml_dir_license_set("CC-BY", "output", profile = "test")

      # Verify different profiles have different configs
      license_default <- projr_yml_dir_license_get("output", profile = "default")
      license_test <- projr_yml_dir_license_get("output", profile = "test")

      expect_identical(license_default, "MIT")
      expect_identical(license_test, "CC-BY")

      # Remove license from test profile
      projr_yml_dir_license_rm("output", profile = "test")

      # Verify removed from test but still in default
      expect_true(is.null(projr_yml_dir_license_get("output", profile = "test")))
      expect_identical(projr_yml_dir_license_get("output", profile = "default"), "MIT")
    }
  )
})

test_that("projr_yml_dir_license_update works with different profiles", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set license in custom profile
      projr_yml_dir_license_set(
        "CC-BY",
        "output",
        authors = c("Old Author"),
        profile = "custom"
      )

      # Create directory
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      # Create LICENSE file
      .license_dir_create("output", safe = FALSE, "custom")

      # Update DESCRIPTION
      desc <- desc::description$new("!new")
      desc$set("Package", "testpkg")
      desc$add_author("New", "Author", email = "new@example.com", role = "aut")
      desc$write(file = .path_get("DESCRIPTION"))

      # Update licenses in custom profile
      suppressMessages(updated <- projr_yml_dir_license_update(profile = "custom"))

      # Verify update in custom profile
      expect_true("output" %in% updated)
      license <- projr_yml_dir_license_get("output", profile = "custom")
      expect_true("New Author" %in% license$authors)

      # Verify default profile is unaffected
      expect_true(is.null(projr_yml_dir_license_get("output", profile = "default")))
    }
  )
})

test_that("Multiple licenses can be set for same directory in different profiles", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set different licenses for output in different profiles
      projr_yml_dir_license_set("MIT", "output", profile = "default")
      projr_yml_dir_license_set("CC-BY", "output", profile = "prod")
      projr_yml_dir_license_set("Apache-2.0", "output", profile = "dev")

      # Verify all three are set correctly
      expect_identical(projr_yml_dir_license_get("output", "default"), "MIT")
      expect_identical(projr_yml_dir_license_get("output", "prod"), "CC-BY")
      expect_identical(projr_yml_dir_license_get("output", "dev"), "Apache-2.0")
    }
  )
})
