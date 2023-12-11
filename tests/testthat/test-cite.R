test_that("projr_yml_cite_ functions work works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_cite_set_default()
      expect_identical(
        .projr_yml_cite_get("default"),
        NULL
      )
      # set one to FALSE
      projr_yml_cite_set(cff = FALSE)
      expect_identical(
        .projr_yml_cite_get("default"),
        list(cff = FALSE)
      )
      # set two to FALSE
      projr_yml_cite_set(codemeta = FALSE)
      expect_identical(
        .projr_yml_cite_get("default"),
        list(codemeta = FALSE, cff = FALSE)
      )
      # set three to FALSE, no simplify identical
      projr_yml_cite_set(inst_citation = FALSE, simplify_identical = FALSE)
      expect_identical(
        .projr_yml_cite_get("default"),
        list(codemeta = FALSE, cff = FALSE, `inst-citation` = FALSE)
      )
      # set three to FALSE, simplify identical
      projr_yml_cite_set(inst_citation = FALSE)
      expect_identical(
        .projr_yml_cite_get("default"),
        FALSE
      )
      # set three to TRUE, no simplify default
      projr_yml_cite_set(all = TRUE, simplify_default = FALSE)
      expect_identical(
        .projr_yml_cite_get("default"),
        TRUE
      )
      # set three to TRUE, simplify default
      projr_yml_cite_set(all = TRUE)
      expect_identical(
        .projr_yml_cite_get("default"),
        NULL
      )
      # use meaningful default
      projr_yml_cite_set(cff = FALSE)
      projr_yml_cite_set_default()
      expect_identical(
        .projr_yml_cite_get("default"),
        NULL
      )
    }
  )
})

test_that(".projr_cite_ functions work works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_cite_citation_set()
      expect_true(
        file.exists(
          file.path("inst", "CITATION")
        )
      )
      .projr_cite_codemeta_set()
      expect_true(file.exists("codemeta.json"))
      expect_true(is.character(.projr_cite_bibtex_get()))
      .projr_cite_cff_set()
      expect_true(file.exists("CITATION.cff"))
    }
  )
})
