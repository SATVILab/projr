# Tests for .build_copy_docs with file parameter
# =================================================

test_that(".build_copy_docs uses file parameter to detect engine (qmd)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a bookdown project configuration (_bookdown.yml)
      # This would normally make .engine_get() return "bookdown"
      writeLines(c('book_filename: "mybook"'), "_bookdown.yml")

      # Create a .qmd file that we want to build
      qmd_file <- "test.qmd"
      writeLines(c(
        "---",
        "title: Test",
        "format: html",
        "---",
        "",
        "# Test Content"
      ), qmd_file)

      # Simulate building this qmd file (create output file)
      writeLines("<html><body>Test</body></html>", "test.html")

      # Call .build_copy_docs with file parameter
      # Should use quarto_document engine, not bookdown
      expect_silent(.build_copy_docs(output_run = FALSE, file = qmd_file))

      # Check that the HTML file was copied to docs directory
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs uses file parameter to detect engine (Rmd)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a _quarto.yml file
      # This would normally make .engine_get() return "quarto_project"
      writeLines(c(
        "project:",
        "  type: website"
      ), "_quarto.yml")

      # Create an .Rmd file that we want to build
      rmd_file <- "test.Rmd"
      writeLines(c(
        "---",
        "title: Test",
        "output: html_document",
        "---",
        "",
        "# Test Content"
      ), rmd_file)

      # Simulate building this Rmd file (create output file)
      writeLines("<html><body>Test</body></html>", "test.html")

      # Call .build_copy_docs with file parameter
      # Should use rmd engine, not quarto_project
      expect_silent(.build_copy_docs(output_run = FALSE, file = rmd_file))

      # Check that the HTML file was copied to docs directory
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs with NULL file uses project configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a bookdown project
      writeLines(c('book_filename: "mybook"'), "_bookdown.yml")

      # Call .build_copy_docs with NULL file (default behavior)
      # Should use bookdown engine from project configuration
      # This should work without error even if no files exist
      expect_silent(.build_copy_docs(output_run = FALSE, file = NULL))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs with empty file vector uses project configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a quarto project
      writeLines(c(
        "project:",
        "  type: website"
      ), "_quarto.yml")

      # Call .build_copy_docs with empty file vector
      # Should use quarto_project engine from project configuration
      expect_silent(.build_copy_docs(output_run = FALSE, file = character(0)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy propagates file parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a bookdown config but build a qmd file
      writeLines(c('book_filename: "mybook"'), "_bookdown.yml")

      # Create test files
      qmd_file <- "test.qmd"
      writeLines(c(
        "---",
        "title: Test",
        "format: html",
        "---",
        "",
        "# Test Content"
      ), qmd_file)
      writeLines("<html><body>Test</body></html>", "test.html")

      # Mock version list
      version_run_on_list <- list()

      # Call .build_copy with file parameter
      # Should pass file to .build_copy_docs
      expect_silent(.build_copy(
        output_run = FALSE,
        bump_component = "dev",
        version_run_on_list = version_run_on_list,
        file = qmd_file
      ))

      # Check that HTML file was copied using quarto engine
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})
