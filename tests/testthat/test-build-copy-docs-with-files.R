# Tests for .build_copy_docs with scripts parameter
# =================================================

test_that(".build_copy_docs uses scripts parameter to detect engine (qmd)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a bookdown project configuration (_bookdown.yml)
      # This would normally make .engine_get_auto() return "bookdown"
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

      # Call .build_copy_docs with scripts parameter
      # Should use quarto_document engine, not bookdown
      expect_silent(.build_copy_docs(output_run = FALSE, engines = .engines_get(qmd_file)))

      # Check that the HTML file was copied to docs directory
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs uses scripts parameter to detect engine (Rmd)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      unlink("_bookdown.yml")
      # Create a _quarto.yml file
      # This would normally make .engine_get_auto() return "quarto_project"
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

      # Call .build_copy_docs with scripts parameter
      # Should use rmd engine, not quarto_project
      expect_silent(.build_copy_docs(output_run = FALSE, engines = .engines_get(rmd_file)))

      # Check that the HTML file was copied to docs directory
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs with NULL scripts uses project configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a bookdown project
      writeLines(c('book_filename: "mybook"'), "_bookdown.yml")

      # Call .build_copy_docs with NULL scripts (default behavior)
      # Should use bookdown engine from project configuration
      # This should work without error even if no files exist
      expect_silent(.build_copy_docs(output_run = FALSE))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs with empty scripts vector skips document collection", {
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

      # Call .build_copy_docs with empty scripts vector
      # Should skip document collection
      expect_silent(.build_copy_docs(output_run = FALSE, engines = list()))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy propagates scripts parameter correctly", {
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

      # Call .build_copy with scripts parameter
      # Should pass scripts to .build_copy_docs
      expect_silent(.build_copy(
        output_run = FALSE,
        bump_component = "dev",
        version_run_on_list = version_run_on_list,
        engines = .engines_get(qmd_file)
      ))

      # Check that HTML file was copied using quarto engine
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("R Markdown collection uses the renderer's actual output path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(dir_test,
    {
      unlink("_output.yml")
      writeLines(c(
        "---", "output:", "  html_document:", "    self_contained: false", "---",
        "", "# Test"
      ), "report.Rmd")
      rendered <- .build_engines(
        .engines_get("report.Rmd"),
        version_run_on_list = .version_run_onwards_get(NULL),
        args_engine = list(output_file = "renamed.html", output_dir = "rendered", quiet = TRUE)
      )
      .build_copy_docs(output_run = FALSE, engines = rendered)

      docs <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs, "renamed.html")))
      expect_true(dir.exists(file.path(docs, "renamed_files")))
      expect_false(file.exists("rendered/renamed.html"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("documents sharing a basename keep their companion assets in separate folders", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(dir_test, {
    outputs <- lapply(c("first", "second"), function(folder) {
      dir.create(file.path(folder, "report_files"), recursive = TRUE)
      writeLines(folder, file.path(folder, "report.html"))
      writeLines(folder, file.path(folder, "report_files", "plot.txt"))
      file.path(folder, c("report.html", "report_files"))
    })
    .build_copy_docs(FALSE, stats::setNames(outputs, c("document_output", "document_output")))
    docs <- projr_path_get_dir("docs", safe = TRUE)
    for (folder in c("first", "second")) {
      expect_identical(readLines(file.path(docs, folder, "report.html")), folder)
      expect_identical(readLines(file.path(docs, folder, "report_files", "plot.txt")), folder)
    }

    writeLines("project page", file.path(docs, "index.html"))
    writeLines("standalone", "second/index.html")
    .build_copy_docs(FALSE, list(quarto_project = NULL, document_output = "second/index.html"))
    expect_identical(readLines(file.path(docs, "index.html")), "project page")
    expect_identical(readLines(file.path(docs, "second", "index.html")), "standalone")

    writeLines("conflicting root page", "index.html")
    expect_error(
      .build_copy_docs(FALSE, list(quarto_project = NULL, document_output = "index.html")),
      "distinct output filenames"
    )
    expect_identical(readLines(file.path(docs, "index.html")), "project page")
  })
})

test_that("post-build clearing preserves a document rendered into its final directory", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(rm_engine = TRUE)
  usethis::with_project(dir_test, {
    docs <- projr_path_get_dir("docs", safe = FALSE)
    output <- file.path(docs, "rendered.html")
    writeLines("in place", output)
    .build_clear_post_safe_docs(TRUE, "document_output", output)
    .build_copy_docs(TRUE, list(document_output = output))
    expect_identical(readLines(output), "in place")
  })
})
