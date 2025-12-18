test_that("projr_engine_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(.engine_get(), "bookdown")
      unlink("_bookdown.yml")
      expect_identical(.engine_get(), "rmd")
      unlink(list.files(pattern = "\\.Rmd$|\\.rmd$"))
      # When no documents exist, .engine_get() returns character(1L)
      expect_identical(.engine_get(), character(1L))
      file.create("_quarto.yml")
      expect_identical(.engine_get(), "quarto_project")
      unlink("_quarto.yml")
      invisible(file.create("index.qmd"))
      expect_identical(.engine_get(), "quarto_document")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine_doc_fn_get_error shows helpful message when no files found automatically", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Test for Quarto documents - auto-detect with no files
      expect_error(
        .build_engine_doc_fn_get_error(character(0), "qmd", NULL),
        regexp = "No Quarto documents found in the project directory.*Please create a qmd file",
        fixed = FALSE
      )

      # Test for RMarkdown documents - auto-detect with no files
      expect_error(
        .build_engine_doc_fn_get_error(character(0), "rmd", NULL),
        regexp = "No RMarkdown documents found in the project directory.*Please create a rmd file",
        fixed = FALSE
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine_doc_fn_get_error shows helpful message when specified files not found", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Test for Quarto documents - specified files don't exist
      expect_error(
        .build_engine_doc_fn_get_error(character(0), "qmd", c("doc1.qmd", "doc2.qmd"), c("doc1.qmd", "doc2.qmd")),
        regexp = "The following Quarto document\\(s\\) could not be found: doc1.qmd, doc2.qmd.*Please check that the file\\(s\\) exist",
        fixed = FALSE
      )

      # Test for RMarkdown documents - specified files don't exist
      expect_error(
        .build_engine_doc_fn_get_error(character(0), "rmd", c("report.Rmd"), c("report.Rmd")),
        regexp = "The following RMarkdown document\\(s\\) could not be found: report.Rmd.*Please check that the file\\(s\\) exist",
        fixed = FALSE
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine_doc_fn_get returns files when they exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test files
      writeLines("# Test", "test1.qmd")
      writeLines("# Test", "test2.Rmd")
      writeLines("# README", "README.Rmd")

      # Test auto-detect for qmd files (should exclude README.Rmd)
      result_qmd <- .build_engine_doc_fn_get(NULL, "qmd")
      expect_identical(result_qmd, "test1.qmd")

      # Test auto-detect for Rmd files (should exclude README.Rmd)
      result_rmd <- .build_engine_doc_fn_get(NULL, "rmd")
      expect_identical(result_rmd, "test2.Rmd")

      # Test explicit file specification
      result_explicit <- .build_engine_doc_fn_get("test1.qmd", "qmd")
      expect_identical(result_explicit, "test1.qmd")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine detects when no documents exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal _projr.yml
      writeLines("build:\n  cache: []", "_projr.yml")

      # Test that .engine_get returns empty character when no documents exist
      engine <- .engine_get()
      expect_identical(engine, character(1L))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine_doc_fn_get reports only missing files when some are specified", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create only one of three files
      writeLines("# Test", "doc1.qmd")

      # Test that only missing files are reported in error
      expect_error(
        .build_engine_doc_fn_get(c("doc1.qmd", "doc2.qmd", "doc3.qmd"), "qmd"),
        regexp = "The following Quarto document\\(s\\) could not be found: doc2.qmd, doc3.qmd",
        fixed = FALSE
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})
