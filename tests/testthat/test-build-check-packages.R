test_that("projr_build_check_packages returns correct structure when all packages available", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple project without special requirements
      # This should pass since core packages are already installed
      result <- projr_build_check_packages()

      # Check structure
      expect_type(result, "list")
      expect_named(result, c("available", "missing", "install_cmds", "message"))

      # Check types
      expect_type(result$available, "logical")
      expect_type(result$missing, "character")
      expect_type(result$install_cmds, "character")
      expect_type(result$message, "character")

      # Check content
      expect_true(result$available)
      expect_length(result$missing, 0)
      expect_length(result$install_cmds, 0)
      expect_match(result$message, "All required packages")
    }
  )
})

test_that("projr_build_check_packages detects missing quarto package", {
  skip_if(.is_test_select())
  skip_if(requireNamespace("quarto", quietly = TRUE))

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a quarto file
      writeLines(
        c(
          "---",
          "title: Test",
          "---",
          "",
          "# Test",
          "Test content"
        ),
        "test.qmd"
      )

      # Set build scripts to include the quarto file
      projr::projr_yml_scripts_set_build("test.qmd")

      result <- projr_build_check_packages()

      # Should detect missing quarto
      expect_false(result$available)
      expect_true("quarto" %in% result$missing)
      expect_length(result$install_cmds, 1)
      expect_match(result$install_cmds[1], "quarto")
    }
  )
})

test_that("projr_build_check_packages detects missing rmarkdown package", {
  skip_if(.is_test_select())
  skip_if(requireNamespace("rmarkdown", quietly = TRUE))
  skip_if(file.exists(.path_get("_bookdown.yml")))

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create an Rmd file without bookdown config
      writeLines(
        c(
          "---",
          "title: Test",
          "---",
          "",
          "# Test",
          "Test content"
        ),
        "test.Rmd"
      )

      # Set build scripts
      projr::projr_yml_scripts_set_build("test.Rmd")

      result <- projr_build_check_packages()

      # Should detect missing rmarkdown (unless bookdown is present)
      if (!requireNamespace("bookdown", quietly = TRUE)) {
        expect_false(result$available)
        expect_true("rmarkdown" %in% result$missing)
      }
    }
  )
})

test_that("projr_build_check_packages handles GitHub remote packages", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add a GitHub remote using the exported function
      projr::projr_yml_dest_add_github(
        title = "archive",
        content = "output",
        structure = "archive",
        send_cue = "always"
      )

      result <- projr_build_check_packages()

      # Check if piggyback and gh are in required or available
      # They might be installed or missing depending on environment
      expect_type(result$available, "logical")

      if (!result$available) {
        # If missing, should include at least one of piggyback or gh
        expect_true(
          any(c("piggyback", "gh") %in% result$missing)
        )
      }
    }
  )
})

test_that("projr_build_check_packages returns proper install commands for CRAN packages", {
  skip_if(.is_test_select())

  # Test the install command format directly
  cmds <- .dep_get_install_cmds(c("somepackage"))
  expect_length(cmds, 1)
  expect_match(cmds[1], 'install\\.packages\\("somepackage"\\)')

  # Multiple packages
  cmds <- .dep_get_install_cmds(c("pkg1", "pkg2"))
  expect_length(cmds, 1)
  expect_match(cmds[1], 'install\\.packages\\(c\\("pkg1", "pkg2"\\)\\)')
})

test_that("projr_build_check_packages returns proper install commands for GitHub packages", {
  skip_if(.is_test_select())

  # GitHub package format
  cmds <- .dep_get_install_cmds(c("user/repo"))
  expect_length(cmds, 1)
  expect_match(cmds[1], 'remotes::install_github\\("user/repo"\\)')
})

test_that("projr_build_check_packages handles mixed CRAN and GitHub packages", {
  skip_if(.is_test_select())

  # Mixed packages
  cmds <- .dep_get_install_cmds(c("cranpkg", "user/repo"))
  expect_length(cmds, 2)
  expect_match(cmds[1], 'install\\.packages\\("cranpkg"\\)')
  expect_match(cmds[2], 'remotes::install_github\\("user/repo"\\)')
})

test_that("projr_build_check_packages message is informative", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      result <- projr_build_check_packages()

      # Message should be informative
      expect_type(result$message, "character")
      expect_gt(nchar(result$message), 0)
    }
  )
})

test_that(".build_check_packages_available uses new function", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # For output run with no missing packages, should succeed
      result <- .build_check_packages_available(output_run = TRUE)
      expect_true(result)

      # For non-output run, should return FALSE immediately
      result <- .build_check_packages_available(output_run = FALSE)
      expect_false(result)
    }
  )
})

test_that("projr_build_check_packages works with profile parameter", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with NULL profile (default)
      result <- projr_build_check_packages(profile = NULL)
      expect_type(result, "list")
      expect_named(result, c("available", "missing", "install_cmds", "message"))
    }
  )
})
