# Keep real citation conversion/writing while excluding remote enrichment.
.test_cite_local_metadata <- function(env = parent.frame()) {
  original_cff_create <- cffr::cff_create
  local_mocked_bindings(
    cff_create = function(...) {
      original_cff_create(..., gh_keywords = FALSE, dependencies = FALSE)
    },
    .package = "cffr",
    .env = env
  )
}

test_that("projr_yml_cite_ functions work works", {
  skip_if(.is_test_cran())
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_cite_set_default()
      expect_identical(
        .yml_cite_get("default"),
        NULL
      )
      # set one to TRUE
      projr_yml_cite_set(cff = TRUE)
      expect_identical(
        .yml_cite_get("default"),
        list(cff = TRUE)
      )
      # set two to TRUE
      projr_yml_cite_set(codemeta = TRUE)
      expect_identical(
        .yml_cite_get("default"),
        list(codemeta = TRUE, cff = TRUE)
      )
      # set three to TRUE, no simplify identical
      projr_yml_cite_set(inst_citation = TRUE, simplify_identical = FALSE)
      expect_identical(
        .yml_cite_get("default"),
        list(codemeta = TRUE, cff = TRUE, `inst-citation` = TRUE)
      )
      # set three to TRUE, simplify identical
      projr_yml_cite_set(inst_citation = TRUE)
      expect_identical(
        .yml_cite_get("default"),
        TRUE
      )
      # set three to FALSE, no simplify default
      projr_yml_cite_set(all = FALSE, simplify_default = FALSE)
      expect_identical(
        .yml_cite_get("default"),
        FALSE
      )
      # set three to FALSE, simplify default
      projr_yml_cite_set(all = FALSE)
      expect_identical(
        .yml_cite_get("default"),
        NULL
      )
      # use meaningful default
      projr_yml_cite_set(cff = TRUE)
      projr_yml_cite_set_default()
      expect_identical(
        .yml_cite_get("default"),
        NULL
      )
    }
  )
})

test_that(".cite_ functions work works", {
  skip_if(.is_test_cran())
  # setup
  skip_if(.is_test_select())
  .test_cite_local_metadata()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .cite_citation_set()
      expect_true(
        file.exists(
          file.path("inst", "CITATION")
        )
      )
      expect_true(is.character(.cite_bibtex_get()))
    }
  )
})

test_that(".cite_citation_set respects create parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  .test_cite_local_metadata()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test create = FALSE when file doesn't exist
      result <- .cite_citation_set(create = FALSE)
      expect_false(result)
      expect_false(file.exists(file.path("inst", "CITATION")))

      # Test create = TRUE creates the file
      result <- .cite_citation_set(create = TRUE)
      expect_true(result)
      expect_true(file.exists(file.path("inst", "CITATION")))
    }
  )
})

test_that(".cite_citation_inst_write creates valid CITATION file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  .test_cite_local_metadata()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create inst directory if needed
      if (!dir.exists("inst")) dir.create("inst")

      # Write CITATION file
      .cite_citation_inst_write()

      # Verify file exists
      citation_path <- file.path("inst", "CITATION")
      expect_true(file.exists(citation_path))

      # Verify file has content
      citation_content <- readLines(citation_path, warn = FALSE)
      expect_true(length(citation_content) > 0)

      # Verify it contains bibentry
      expect_true(any(grepl("bibentry", citation_content, ignore.case = TRUE)))
    }
  )
})

test_that(".cite_citation_inst_add_header adds header correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  .test_cite_local_metadata()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # First create the CITATION file
      if (!dir.exists("inst")) dir.create("inst")
      .cite_citation_inst_write()

      # Get initial content
      citation_path <- file.path("inst", "CITATION")
      content_before <- readLines(citation_path, warn = FALSE)
      initial_lines <- length(content_before)

      # Add header
      .cite_citation_inst_add_header()

      # Verify header was added
      content_after <- readLines(citation_path, warn = FALSE)
      expect_true(length(content_after) > initial_lines)

      # Verify first line contains citHeader
      expect_true(grepl("citHeader", content_after[1]))

      # Verify package name is in header
      pkg_name <- .pkg_nm_get()
      expect_true(grepl(pkg_name, content_after[1]))

      # Verify original content is preserved
      expect_identical(content_after[-1], content_before)
    }
  )
})

test_that(".cite_codemeta_set respects create parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  paths <- list()
  local_mocked_bindings(
    write_codemeta = function(path) {
      paths[[length(paths) + 1L]] <<- path
    },
    .package = "codemeta"
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Test create = FALSE when file doesn't exist
      result <- .cite_codemeta_set(create = FALSE)
      expect_false(result)
      expect_false(file.exists("codemeta.json"))

      expect_length(paths, 0L)

      # Creating metadata delegates to the writer with the project path.
      result <- .cite_codemeta_set(create = TRUE)
      expect_true(result)
      expect_length(paths, 1L)
      expect_identical(as.character(paths[[1]]), dir_test)
    }
  )
})

test_that(".cite_cff_set respects create parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  calls <- list()
  local_mocked_bindings(
    cff_write = function(...) {
      calls[[length(calls) + 1L]] <<- list(args = list(...), path = getwd())
    },
    .package = "cffr"
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Test create = FALSE when file doesn't exist
      result <- .cite_cff_set(create = FALSE)
      expect_false(result)
      expect_false(file.exists("CITATION.cff"))

      expect_length(calls, 0L)

      # The default CFF writer runs in the temporary project.
      result <- .cite_cff_set(create = TRUE)
      expect_true(result)
      expect_identical(calls, list(list(args = list(), path = dir_test)))
    }
  )
})

test_that(".cite_bibtex_get returns valid bibtex format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  .test_cite_local_metadata()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      bibtex <- .cite_bibtex_get()

      # Check it returns character vector
      expect_true(is.character(bibtex))
      expect_true(length(bibtex) > 0)

      # Check it contains expected sections
      bibtex_text <- paste(bibtex, collapse = "\n")
      expect_true(grepl("## Citation", bibtex_text, fixed = TRUE))
      expect_true(grepl("To cite", bibtex_text))

      # Check package name is included
      pkg_name <- .pkg_nm_get()
      expect_true(grepl(pkg_name, bibtex_text))
    }
  )
})

test_that(".cite_citation_set can be called multiple times", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  .test_cite_local_metadata()
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # First call
      .cite_citation_set()
      citation_path <- file.path("inst", "CITATION")
      expect_true(file.exists(citation_path))
      content1 <- readLines(citation_path, warn = FALSE)

      # Second call should work and overwrite
      .cite_citation_set()
      expect_true(file.exists(citation_path))
      content2 <- readLines(citation_path, warn = FALSE)

      # Content should be identical
      expect_identical(content1, content2)
    }
  )
})
