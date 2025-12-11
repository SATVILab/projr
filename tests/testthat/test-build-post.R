# Tests for build-post.R functions (LITE mode compatible)
# ==========================================================

# Roxygen tests
# -------------

test_that(".build_roxygen_check_fn detects roxygen comments", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_roxygen")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create file with roxygen comments
      writeLines(c(
        "#' @title Test function",
        "#' @export",
        "test_fn <- function() {}"
      ), "with_roxygen.R")

      # Create file without roxygen comments
      writeLines(c(
        "# Regular comment",
        "test_fn <- function() {}"
      ), "without_roxygen.R")

      # Test detection
      expect_true(.build_roxygen_check_fn("with_roxygen.R"))
      expect_false(.build_roxygen_check_fn("without_roxygen.R"))
      expect_false(.build_roxygen_check_fn("nonexistent.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_roxygenise_check returns FALSE when output_run is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_roxygenise_check(output_run = FALSE)
      expect_false(result)
    }
  )
})

test_that(".build_roxygenise_check returns FALSE when R directory missing", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_no_r")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")

      # No R directory
      result <- .build_roxygenise_check(output_run = TRUE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_roxygenise_check detects roxygen in R files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_roxygen_check")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")

      # Create R directory with roxygen file
      dir.create("R")
      writeLines(c(
        "#' @title Test",
        "#' @export",
        "test <- function() {}"
      ), "R/test.R")

      result <- .build_roxygenise_check(output_run = TRUE)
      expect_true(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_roxygenise returns FALSE when output_run is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_roxygenise(output_run = FALSE)
      expect_false(result)
    }
  )
})

# Citation tests
# --------------

test_that(".build_cite_get_yml returns empty when cite is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_cite_set(all = FALSE)
      result <- .build_cite_get_yml()
      expect_identical(result, character(0))
    }
  )
})

test_that(".build_cite_get_yml returns all when cite is TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_cite_set(all = TRUE)
      result <- .build_cite_get_yml()
      expect_identical(result, c("cff", "codemeta", "inst-citation"))
    }
  )
})

test_that(".build_cite_get_yml handles NULL input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # When cite is NULL (not set), should return empty character
      projr_yml_cite_set_default()
      result <- .build_cite_get_yml()
      expect_identical(result, character(0))
    }
  )
})

test_that(".build_cite returns FALSE when output_run is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_cite(output_run = FALSE)
      expect_false(result)
    }
  )
})

test_that(".build_cite works with empty cite config", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_cite_set(all = FALSE)
      result <- .build_cite(output_run = TRUE)
      expect_true(result)
    }
  )
})

test_that(".build_cite_cff returns FALSE when file doesn't exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_cite_cff")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      result <- .build_cite_cff()
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_cite_cff_update_file updates file without errors", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create CITATION.cff with preferred-citation section
      # (using double colon format to match current implementation)
      writeLines(c(
        "cff-version: 1.2.0",
        "version:: \"0.0.0\"",
        "title: Test",
        "preferred-citation:",
        "  type: software",
        "  version: \"0.0.0\"",
        "  title: Test Software"
      ), "CITATION.cff")

      # Should update without error
      expect_silent(.build_cite_cff_update_file())

      # Check file was written
      expect_true(file.exists(.path_get("CITATION.cff")))
    }
  )
})

test_that(".build_cite_inst_citation returns FALSE when file doesn't exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_cite_inst")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      result <- .build_cite_inst_citation()
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_cite_inst_citation_update_file updates version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create inst/CITATION with old version
      dir.create("inst", showWarnings = FALSE)
      writeLines(c(
        "bibentry(",
        "  version = \"0.0.0\",",
        "  title = \"Test\"",
        ")"
      ), "inst/CITATION")

      # Update to current version
      .build_cite_inst_citation_update_file()

      # Check updated
      citation_content <- readLines("inst/CITATION")
      version_line <- grep("version", citation_content, value = TRUE)
      expect_true(grepl(projr_version_get(), version_line))
    }
  )
})

test_that(".build_cite_codemeta returns FALSE when file doesn't exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_cite_codemeta")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      result <- .build_cite_codemeta()
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_cite_codemeta_update_file updates version in codemeta.json", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create codemeta.json with old version
      writeLines(c(
        "{",
        "  \"version\": \"0.0.0\",",
        "  \"name\": \"test\"",
        "}"
      ), "codemeta.json")

      # Update to current version
      .build_cite_codemeta_update_file()

      # Check updated
      codemeta_content <- readLines("codemeta.json")
      version_line <- grep("\"version\"", codemeta_content, value = TRUE)
      expect_true(grepl(projr_version_get(), version_line))
    }
  )
})

# README tests
# ------------

test_that(".build_readme_rmd_render_check returns FALSE when output_run is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_readme_rmd_render_check(output_run = FALSE)
      expect_false(result)
    }
  )
})

test_that(".build_readme_rmd_render_check returns FALSE when README.Rmd missing", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_no_readme")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      result <- .build_readme_rmd_render_check(output_run = TRUE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_readme_rmd_render_check returns TRUE when README.Rmd exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_readme_exists")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      writeLines(c("---", "title: Test", "---", "# Test"), "README.Rmd")
      result <- .build_readme_rmd_render_check(output_run = TRUE)
      expect_true(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_readme_rmd_render_detect_pkg_use detects library calls", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create README.Rmd with library call
      pkg_name <- projr_name_get()
      writeLines(c(
        "---",
        "title: Test",
        "---",
        paste0("library(", pkg_name, ")")
      ), "README.Rmd")

      result <- .build_readme_rmd_render_detect_pkg_use()
      expect_true(result)
    }
  )
})

test_that(".build_readme_rmd_render_detect_pkg_use detects :: usage", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create README.Rmd with :: usage
      pkg_name <- projr_name_get()
      writeLines(c(
        "---",
        "title: Test",
        "---",
        paste0(pkg_name, "::test_function()")
      ), "README.Rmd")

      result <- .build_readme_rmd_render_detect_pkg_use()
      expect_true(result)
    }
  )
})

test_that(".build_readme_rmd_render_detect_pkg_use returns FALSE without pkg usage", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create README.Rmd without package usage
      writeLines(c(
        "---",
        "title: Test",
        "---",
        "# Test content"
      ), "README.Rmd")

      result <- .build_readme_rmd_render_detect_pkg_use()
      expect_false(result)
    }
  )
})

test_that(".build_readme_rmd_render returns FALSE when check fails", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- tempfile("test_readme_no_file")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")
      result <- .build_readme_rmd_render(output_run = TRUE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Version revert tests
# --------------------

test_that(".build_version_set_post returns FALSE when success is TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      version_list <- list(desc = list(failure = "0.0.0"))
      result <- .build_version_set_post(version_list, success = TRUE)
      expect_false(result)
    }
  )
})

test_that(".build_version_set_post reverts version on failure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set initial version
      initial_version <- "0.0.0"
      projr_version_set(initial_version)

      # Change version to simulate build
      projr_version_set("0.0.1")
      expect_identical(projr_version_get(), "0.0.1")

      # Simulate failure and revert
      version_list <- list(desc = list(failure = initial_version))
      result <- .build_version_set_post(version_list, success = FALSE)

      # Should revert to initial version
      expect_true(result)
      expect_identical(projr_version_get(), initial_version)
    }
  )
})
