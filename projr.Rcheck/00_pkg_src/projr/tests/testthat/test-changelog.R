test_that(".changelog_add works", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .changelog_add(
        msg = "Test",
        bump_component = "patch",
        version_run_on_list = list(
          desc = list(
            success = "0.0.1"
          )
        )
      )
      .changelog_add(
        msg = "Test",
        bump_component = "minor",
        version_run_on_list = list(
          desc = list(
            success = "0.1.0"
          )
        )
      )
      .changelog_add(
        msg = "Test",
        bump_component = "major",
        version_run_on_list = list(
          desc = list(
            success = "1.0.0"
          )
        )
      )
      expect_identical(
        .changelog_read() |> sub("\\):.*$", "\\)", x = _),
        c(
          "# CHANGELOG", "", "- **Major** (v1.0.0): Miguel Julio Rodo (14:39:59)", # nolint
          "  - Test", "", "___", "", "- *Minor* (v0.1.0): Miguel Julio Rodo (14:39:59)", # nolint
          "  - Test", "", "- Patch (v0.0.1): Miguel Julio Rodo (14:39:59)",
          "  - Test", ""
        ) |>
          sub("\\):.*$", "\\)", x = _)
      )
      # Test that projr_cat_changelog works (just ensure it doesn't error)
      expect_no_error(projr_cat_changelog(n_row = 5))
    }
  )
})

test_that(".changelog_read works with empty file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Should return default header if file doesn't exist
      result <- .changelog_read()
      expect_identical(result, c("# CHANGELOG", ""))
    }
  )
})

test_that(".changelog_write creates file correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      test_content <- c("# CHANGELOG", "", "- Test entry")
      .changelog_write(test_content)
      
      # Verify file exists
      path_changelog <- .path_get("CHANGELOG.md")
      expect_true(file.exists(path_changelog))
      
      # Verify content
      result <- readLines(path_changelog, warn = FALSE)
      expect_identical(result, test_content)
    }
  )
})

test_that(".changelog_get_version works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      version_list <- list(desc = list(success = "1.2.3"))
      result <- .changelog_get_version(version_list)
      expect_identical(result, "v1.2.3")
    }
  )
})

test_that(".changelog_get_bump_component formats correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(.changelog_get_bump_component("patch"), "Patch")
      expect_identical(.changelog_get_bump_component("minor"), "*Minor*")
      expect_identical(.changelog_get_bump_component("major"), "**Major**")
    }
  )
})

test_that(".changelog_get_entry formats entries correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      test_line <- c("- Test line", "  - Details")
      
      # Patch: just the line
      patch_result <- .changelog_get_entry(test_line, "patch")
      expect_identical(patch_result, test_line)
      
      # Minor: line + blank line
      minor_result <- .changelog_get_entry(test_line, "minor")
      expect_identical(minor_result, c(test_line, ""))
      
      # Major: line + blank + separator + blank
      major_result <- .changelog_get_entry(test_line, "major")
      expect_identical(major_result, c(test_line, "", "___", ""))
    }
  )
})

test_that("changelog preserves existing entries", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Add first entry
      .changelog_add(
        msg = "First entry",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.1"))
      )
      
      # Add second entry
      .changelog_add(
        msg = "Second entry",
        bump_component = "patch",
        version_run_on_list = list(desc = list(success = "0.0.2"))
      )
      
      content <- .changelog_read()
      
      # Both entries should be present
      expect_true(any(grepl("First entry", content)))
      expect_true(any(grepl("Second entry", content)))
      
      # Second entry should come before first (newest first)
      first_pos <- which(grepl("First entry", content))[1]
      second_pos <- which(grepl("Second entry", content))[1]
      expect_true(second_pos < first_pos)
    }
  )
})
