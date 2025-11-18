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
      .cite_codemeta_set()
      expect_true(file.exists("codemeta.json"))
      expect_true(is.character(.cite_bibtex_get()))
      .cite_cff_set()
      expect_true(file.exists("CITATION.cff"))
    }
  )
})
