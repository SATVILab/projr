# Test file for R/build-engine.R functions
# Increases coverage for build engine functions

# Test .build_engine main dispatcher function
# ===========================================

test_that(".build_engine works with bookdown projects", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create version_run_on_list (required parameter)
      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      # Bookdown is the default for test projects
      # Test that .build_engine successfully renders bookdown
      result <- .build_engine(
        file = NULL,
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )

      # Check that bookdown rendered successfully
      expect_true(result)
      expect_true(dir.exists("docs"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine works with quarto documents", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      if (file.exists("index.Rmd")) file.remove("index.Rmd")

      # Create a Quarto document
      writeLines(
        c(
          "---",
          "title: \"Test Quarto\"",
          "format: html",
          "---",
          "",
          "# Introduction",
          "",
          "This is a test."
        ),
        "index.qmd"
      )

      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      result <- .build_engine(
        file = NULL,
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )

      expect_true(result)
      expect_true(file.exists("index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine works with RMarkdown documents", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files to test standalone RMarkdown
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")

      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      # index.Rmd should exist from test setup
      result <- .build_engine(
        file = NULL,
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )

      expect_true(result)
      expect_true(file.exists("index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine works with quarto projects", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      if (file.exists("index.Rmd")) file.remove("index.Rmd")

      # Setup quarto project
      .test_setup_project_lit_docs("quarto_project")

      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      result <- .build_engine(
        file = NULL,
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )

      expect_true(result)
      # Quarto projects output to _site by default
      expect_true(dir.exists("_site"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine respects file parameter for qmd documents", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      if (file.exists("index.Rmd")) file.remove("index.Rmd")

      # Create specific quarto files
      writeLines(
        c("---", "title: \"Doc1\"", "---", "", "# Doc 1"),
        "doc1.qmd"
      )
      writeLines(
        c("---", "title: \"Doc2\"", "---", "", "# Doc 2"),
        "doc2.qmd"
      )

      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      # Only render doc1.qmd
      result <- .build_engine(
        file = "doc1.qmd",
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )

      expect_true(result)
      expect_true(file.exists("doc1.html"))
      expect_false(file.exists("doc2.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine respects file parameter for rmd documents", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      if (file.exists("index.Rmd")) file.remove("index.Rmd")

      # Create specific RMarkdown files
      writeLines(
        c("---", "title: \"Report1\"", "---", "", "# Report 1"),
        "report1.Rmd"
      )
      writeLines(
        c("---", "title: \"Report2\"", "---", "", "# Report 2"),
        "report2.Rmd"
      )

      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      # Only render report1.Rmd
      result <- .build_engine(
        file = "report1.Rmd",
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )

      expect_true(result)
      expect_true(file.exists("report1.html"))
      expect_false(file.exists("report2.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine handles no documents error", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all document files
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      if (file.exists("index.Rmd")) file.remove("index.Rmd")

      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      # Should error when no documents exist
      expect_error(
        .build_engine(
          file = NULL,
          version_run_on_list = version_run_on_list,
          args_engine = list()
        ),
        regexp = "No Quarto or RMarkdown documents found"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test individual engine functions
# =================================

test_that(".build_engine_bookdown succeeds with valid project", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test project has bookdown setup by default
      result <- .build_engine_bookdown(
        args_engine = list()
      )

      # Should return NULL on success
      expect_null(result)
      expect_true(dir.exists("docs"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_bookdown returns error message on failure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a broken Rmd file
      writeLines(
        c(
          "---",
          "title: \"Broken\"",
          "---",
          "",
          "```{r}",
          "stop('Intentional error')",
          "```"
        ),
        "index.Rmd"
      )

      result <- .build_engine_bookdown(
        args_engine = list()
      )

      # Should return error message string
      expect_type(result, "character")
      expect_true(grepl("Error rendering bookdown", result))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_quarto_project succeeds with valid project", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files
      if (file.exists("_bookdown.yml")) file.remove("_bookdown.yml")
      if (file.exists("_output.yml")) file.remove("_output.yml")
      if (file.exists("index.Rmd")) file.remove("index.Rmd")

      # Setup quarto project
      .test_setup_project_lit_docs("quarto_project")

      result <- .build_engine_quarto_project(
        args_engine = list()
      )

      # Should return NULL on success
      expect_null(result)
      # Quarto projects output to _site by default
      expect_true(dir.exists("_site"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_qmd succeeds with valid document", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple quarto document
      writeLines(
        c(
          "---",
          "title: \"Test\"",
          "format: html",
          "---",
          "",
          "# Test"
        ),
        "test.qmd"
      )

      result <- .build_engine_qmd(
        file = "test.qmd",
        args_engine = list()
      )

      # Should return NULL on success
      expect_null(result)
      expect_true(file.exists("test.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_qmd returns error message on failure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a broken quarto document
      writeLines(
        c(
          "---",
          "title: \"Broken\"",
          "---",
          "",
          "```{r}",
          "stop('Intentional error')",
          "```"
        ),
        "broken.qmd"
      )

      result <- .build_engine_qmd(
        file = "broken.qmd",
        args_engine = list()
      )

      # Should return error message string
      expect_type(result, "character")
      expect_true(grepl("Error rendering Quarto document", result))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_qmd renders multiple files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create multiple quarto documents
      writeLines(
        c("---", "title: \"Doc1\"", "---", "", "# Doc 1"),
        "doc1.qmd"
      )
      writeLines(
        c("---", "title: \"Doc2\"", "---", "", "# Doc 2"),
        "doc2.qmd"
      )

      result <- .build_engine_qmd(
        file = c("doc1.qmd", "doc2.qmd"),
        args_engine = list()
      )

      # Should return NULL on success
      expect_null(result)
      expect_true(file.exists("doc1.html"))
      expect_true(file.exists("doc2.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_qmd stops on first error when rendering multiple files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create one good and one broken document
      writeLines(
        c("---", "title: \"Good\"", "---", "", "# Good"),
        "good.qmd"
      )
      writeLines(
        c(
          "---",
          "title: \"Broken\"",
          "---",
          "",
          "```{r}",
          "stop('Error')",
          "```"
        ),
        "broken.qmd"
      )
      writeLines(
        c("---", "title: \"Never\"", "---", "", "# Never"),
        "never.qmd"
      )

      # Render in order: good, broken, never
      result <- .build_engine_qmd(
        file = c("good.qmd", "broken.qmd", "never.qmd"),
        args_engine = list()
      )

      # Should stop at first error
      expect_type(result, "character")
      expect_true(file.exists("good.html"))
      expect_false(file.exists("never.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_rmd succeeds with valid document", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple RMarkdown document
      writeLines(
        c(
          "---",
          "title: \"Test\"",
          "output: html_document",
          "---",
          "",
          "# Test"
        ),
        "test.Rmd"
      )

      result <- .build_engine_rmd(
        file = "test.Rmd",
        args_engine = list()
      )

      # Should return NULL on success
      expect_null(result)
      expect_true(file.exists("test.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_rmd returns error message on failure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a broken RMarkdown document
      writeLines(
        c(
          "---",
          "title: \"Broken\"",
          "---",
          "",
          "```{r}",
          "stop('Intentional error')",
          "```"
        ),
        "broken.Rmd"
      )

      result <- .build_engine_rmd(
        file = "broken.Rmd",
        args_engine = list()
      )

      # Should return error message string
      expect_type(result, "character")
      expect_true(grepl("Error rendering RMarkdown document", result))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_rmd renders multiple files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create multiple RMarkdown documents
      writeLines(
        c("---", "title: \"Report1\"", "---", "", "# Report 1"),
        "report1.Rmd"
      )
      writeLines(
        c("---", "title: \"Report2\"", "---", "", "# Report 2"),
        "report2.Rmd"
      )

      result <- .build_engine_rmd(
        file = c("report1.Rmd", "report2.Rmd"),
        args_engine = list()
      )

      # Should return NULL on success
      expect_null(result)
      # With bookdown, files get renamed to report-1.html and report-2.html
      expect_true(file.exists("report-1.html") || file.exists("report1.html"))
      expect_true(file.exists("report-2.html") || file.exists("report2.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_rmd stops on first error when rendering multiple files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create one good and one broken document
      writeLines(
        c("---", "title: \"Good\"", "---", "", "# Good"),
        "good.Rmd"
      )
      writeLines(
        c(
          "---",
          "title: \"Broken\"",
          "---",
          "",
          "```{r}",
          "stop('Error')",
          "```"
        ),
        "broken.Rmd"
      )
      writeLines(
        c("---", "title: \"Never\"", "---", "", "# Never"),
        "never.Rmd"
      )

      # Render in order: good, broken, never
      result <- .build_engine_rmd(
        file = c("good.Rmd", "broken.Rmd", "never.Rmd"),
        args_engine = list()
      )

      # Should stop at first error
      expect_type(result, "character")
      expect_true(file.exists("good.html"))
      expect_false(file.exists("never.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test .build_engine_error
# =========================

test_that(".build_engine_error returns TRUE when no error", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  version_run_on_list <- list(
    "desc" = c(
      "run" = "0.0.0-1",
      "failure" = "0.0.0-1",
      "success" = "0.0.1-1"
    )
  )

  result <- .build_engine_error(
    build_error = NULL,
    version_run_on_list = version_run_on_list
  )

  expect_true(result)
})

test_that(".build_engine_error stops with error message when error present", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      version_run_on_list <- list(
        "desc" = c(
          "run" = "0.0.0-1",
          "failure" = "0.0.0-1",
          "success" = "0.0.1-1"
        )
      )

      # Should throw error with the provided message
      expect_error(
        .build_engine_error(
          build_error = "Test error message",
          version_run_on_list = version_run_on_list
        ),
        regexp = "Test error message"
      )

      # Version should be set to failure version
      expect_identical(projr_version_get(), "0.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test args_engine parameter passing
# ===================================

test_that(".build_engine_qmd passes args_engine correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a quarto document
      writeLines(
        c(
          "---",
          "title: \"Test\"",
          "format: html",
          "---",
          "",
          "# Test"
        ),
        "test.qmd"
      )

      # Test with custom args_engine (quiet mode)
      result <- .build_engine_qmd(
        file = "test.qmd",
        args_engine = list(quiet = TRUE)
      )

      expect_null(result)
      expect_true(file.exists("test.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_engine_rmd passes args_engine correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create an RMarkdown document
      writeLines(
        c(
          "---",
          "title: \"Test\"",
          "output: html_document",
          "---",
          "",
          "# Test"
        ),
        "test.Rmd"
      )

      # Test with custom args_engine (quiet mode)
      result <- .build_engine_rmd(
        file = "test.Rmd",
        args_engine = list(quiet = TRUE)
      )

      expect_null(result)
      expect_true(file.exists("test.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})
