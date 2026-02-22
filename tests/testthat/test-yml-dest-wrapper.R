# Tests for R/yml-dest-wrapper.R - wrapper functions for destination configuration

# =============================================================================
# projr_yml_dest_add_local tests
# =============================================================================
test_that("projr_yml_dest_add_local validates required parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should error when title is not a string
      expect_error(
        projr_yml_dest_add_local(
          title = c("title1", "title2"),
          content = "output",
          path = "_archive"
        )
      )

      # Should error when content is not in valid directory labels
      expect_error(
        projr_yml_dest_add_local(
          title = "test",
          content = "invalid-label",
          path = "_archive"
        )
      )

      # Should error when path is not a string
      expect_error(
        projr_yml_dest_add_local(
          title = "test",
          content = "output",
          path = c("path1", "path2")
        )
      )
    }
  )
})

test_that("projr_yml_dest_add_local creates local destination with all parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "test-local-full",
        content = "output",
        path = "/archive/path",
        structure = "latest",
        send_cue = "always",
        send_strategy = "upload-all",
        send_inspect = "none"
      )

      yml_title <- .yml_dest_get_title("test-local-full", "local", "default")
      expect_identical(yml_title[["path"]], "/archive/path")
      expect_identical(yml_title[["structure"]], "latest")
      expect_identical(yml_title[["send"]][["cue"]], "always")
      expect_identical(yml_title[["send"]][["strategy"]], "upload-all")
      expect_identical(yml_title[["send"]][["inspect"]], "none")
    }
  )
})

test_that("projr_yml_dest_add_local with relative path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "test-local-relative",
        content = "raw-data",
        path = "_archive"
      )

      yml_title <- .yml_dest_get_title("test-local-relative", "local", "default")
      expect_identical(yml_title[["path"]], "_archive")
    }
  )
})

test_that("projr_yml_dest_add_local with multiple content labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "test-local-multi",
        content = c("output", "raw-data"),
        path = "_archive"
      )

      yml_title <- .yml_dest_get_title("test-local-multi", "local", "default")
      expect_true("output" %in% yml_title[["content"]])
      expect_true("raw-data" %in% yml_title[["content"]])
    }
  )
})

test_that("projr_yml_dest_add_local respects overwrite parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add first destination
      projr_yml_dest_add_local(
        title = "test-local-ow",
        content = "output",
        path = "_archive",
        overwrite = FALSE
      )

      # Try to add again without overwrite - should error
      expect_error(
        projr_yml_dest_add_local(
          title = "test-local-ow",
          content = "raw-data",
          path = "_new-archive",
          overwrite = FALSE
        ),
        "already exists"
      )

      # Add again with overwrite - should succeed
      projr_yml_dest_add_local(
        title = "test-local-ow",
        content = "raw-data",
        path = "_new-archive",
        overwrite = TRUE
      )

      yml_title <- .yml_dest_get_title("test-local-ow", "local", "default")
      expect_identical(yml_title[["path"]], "_new-archive")
    }
  )
})

test_that("projr_yml_dest_add_local with profile parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "test-local-profile",
        content = "output",
        path = "_archive",
        profile = "test-profile"
      )

      yml_title <- .yml_dest_get_title("test-local-profile", "local", "test-profile")
      expect_identical(yml_title[["content"]], "output")
    }
  )
})

# =============================================================================
# projr_yml_dest_add_github tests
# =============================================================================

test_that("projr_yml_dest_add_github validates required parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should error when title is not a string
      expect_error(
        projr_yml_dest_add_github(
          title = c("title1", "title2"),
          content = "output"
        )
      )

      # Should error when content is not in valid directory labels
      expect_error(
        projr_yml_dest_add_github(
          title = "test",
          content = "invalid-label"
        )
      )
    }
  )
})

test_that("projr_yml_dest_add_github replaces spaces with hyphens in title", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_github(
        title = "test github release",
        content = "output"
      )

      yml_title <- .yml_dest_get_title("test-github-release", "github", "default")
      expect_identical(yml_title[["content"]], "output")
    }
  )
})

test_that("projr_yml_dest_add_github with all parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # send_cue must be "if-change" or "always" (not "never")
      projr_yml_dest_add_github(
        title = "test-github-full",
        content = "output",
        structure = "latest",
        send_cue = "always",
        send_strategy = "upload-missing",
        send_inspect = "manifest"
      )

      yml_title <- .yml_dest_get_title("test-github-full", "github", "default")
      expect_identical(yml_title[["structure"]], "latest")
      expect_identical(yml_title[["send"]][["cue"]], "always")
      expect_identical(yml_title[["send"]][["strategy"]], "upload-missing")
      expect_identical(yml_title[["send"]][["inspect"]], "manifest")
    }
  )
})

test_that("projr_yml_dest_add_github with code content label", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_github(
        title = "test-github-code",
        content = "code"
      )

      yml_title <- .yml_dest_get_title("test-github-code", "github", "default")
      expect_identical(yml_title[["content"]], "code")
    }
  )
})

test_that("projr_yml_dest_add_github with multiple content labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_github(
        title = "test-github-multi",
        content = c("output", "code")
      )

      yml_title <- .yml_dest_get_title("test-github-multi", "github", "default")
      expect_true("output" %in% yml_title[["content"]])
      expect_true("code" %in% yml_title[["content"]])
    }
  )
})

test_that("projr_yml_dest_add_github respects overwrite parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add first destination
      projr_yml_dest_add_github(
        title = "test-github-ow",
        content = "output",
        overwrite = FALSE
      )

      # Try to add again without overwrite - should error
      expect_error(
        projr_yml_dest_add_github(
          title = "test-github-ow",
          content = "code",
          overwrite = FALSE
        ),
        "already exists"
      )

      # Add again with overwrite - should succeed
      projr_yml_dest_add_github(
        title = "test-github-ow",
        content = "code",
        overwrite = TRUE
      )

      yml_title <- .yml_dest_get_title("test-github-ow", "github", "default")
      expect_identical(yml_title[["content"]], "code")
    }
  )
})

test_that("projr_yml_dest_add_github with profile parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_github(
        title = "test-github-profile",
        content = "output",
        profile = "test-profile"
      )

      yml_title <- .yml_dest_get_title("test-github-profile", "github", "test-profile")
      expect_identical(yml_title[["content"]], "output")
    }
  )
})
