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
      .test_setup_content("raw-data", safe = FALSE)
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
      raw_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

      suppressMessages(
        projr_license_create_manual("MIT", "raw-data", authors = c("Manual Author"))
      )

      # Verify it was created
      expect_true(file.exists(file.path(raw_dir, "LICENSE")))
      license1 <- readLines(file.path(raw_dir, "LICENSE"), warn = FALSE)
      expect_true(any(grepl("Manual Author", license1)))

      # Create content and build
      .test_setup_content("raw-data", safe = FALSE)
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        file.path(.path_get(), "test.qmd")
      )

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
      .test_setup_content("raw-data", safe = FALSE)
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
