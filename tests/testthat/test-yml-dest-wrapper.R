# Tests for R/yml-dest-wrapper.R - wrapper functions for destination configuration

# =============================================================================
# projr_yml_dest_add_osf tests
# =============================================================================

test_that("projr_yml_dest_add_osf creates OSF destination with basic parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add OSF destination with minimal parameters, category is required
      projr_yml_dest_add_osf(
        title = "test-osf",
        content = "output",
        category = "data",
        public = FALSE
      )

      yml_title <- .yml_dest_get_title("test-osf", "osf", "default")
      expect_identical(yml_title[["content"]], "output")
      expect_false(yml_title[["public"]])
      expect_identical(yml_title[["category"]], "data")
    }
  )
})

test_that("projr_yml_dest_add_osf creates OSF destination with structure parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf(
        title = "test-osf-latest",
        content = "raw-data",
        category = "analysis",
        structure = "latest",
        public = TRUE
      )

      yml_title <- .yml_dest_get_title("test-osf-latest", "osf", "default")
      expect_identical(yml_title[["structure"]], "latest")
      expect_true(yml_title[["public"]])
    }
  )
})

test_that("projr_yml_dest_add_osf creates OSF destination with send parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf(
        title = "test-osf-send",
        content = "output",
        category = "data",
        send_cue = "always",
        send_strategy = "sync-purge",
        send_inspect = "file"
      )

      yml_title <- .yml_dest_get_title("test-osf-send", "osf", "default")
      expect_identical(yml_title[["send"]][["cue"]], "always")
      expect_identical(yml_title[["send"]][["strategy"]], "sync-purge")
      expect_identical(yml_title[["send"]][["inspect"]], "file")
    }
  )
})

test_that("projr_yml_dest_add_osf creates OSF destination with category and description", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf(
        title = "test-osf-meta",
        content = "output",
        category = "Analysis",
        description = "Test OSF node"
      )

      yml_title <- .yml_dest_get_title("test-osf-meta", "osf", "default")
      # Category should be converted to lowercase
      expect_identical(yml_title[["category"]], "analysis")
      expect_identical(yml_title[["description"]], "Test OSF node")
    }
  )
})

test_that("projr_yml_dest_add_osf respects overwrite parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add first destination
      projr_yml_dest_add_osf(
        title = "test-overwrite",
        content = "output",
        category = "data",
        overwrite = FALSE
      )

      # Try to add again without overwrite - should error
      expect_error(
        projr_yml_dest_add_osf(
          title = "test-overwrite",
          content = "raw-data",
          category = "analysis",
          overwrite = FALSE
        ),
        "already exists"
      )

      # Add again with overwrite - should succeed
      projr_yml_dest_add_osf(
        title = "test-overwrite",
        content = "raw-data",
        category = "analysis",
        overwrite = TRUE
      )

      yml_title <- .yml_dest_get_title("test-overwrite", "osf", "default")
      expect_identical(yml_title[["content"]], "raw-data")
    }
  )
})

test_that("projr_yml_dest_add_osf with path parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf(
        title = "test-osf-path",
        content = "output",
        category = "data",
        path = "custom/path"
      )

      yml_title <- .yml_dest_get_title("test-osf-path", "osf", "default")
      expect_identical(yml_title[["path"]], "custom/path")
    }
  )
})

test_that("projr_yml_dest_add_osf with id and id_parent parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf(
        title = "test-osf-ids",
        content = "output",
        category = "data",
        id = "abcde",
        id_parent = "fghij"
      )

      yml_title <- .yml_dest_get_title("test-osf-ids", "osf", "default")
      expect_identical(yml_title[["id"]], "abcde")
      expect_identical(yml_title[["id-parent"]], "fghij")
    }
  )
})

# =============================================================================
# projr_yml_dest_add_osf_proj tests
# =============================================================================

test_that("projr_yml_dest_add_osf_proj creates OSF project with required parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf_proj(
        title = "test-project",
        content = "output"
      )

      yml_title <- .yml_dest_get_title("test-project", "osf", "default")
      expect_identical(yml_title[["content"]], "output")
      expect_identical(yml_title[["category"]], "project")
      expect_false(yml_title[["public"]])
    }
  )
})

test_that("projr_yml_dest_add_osf_proj creates public OSF project", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_osf_proj(
        title = "test-public-project",
        description = "A public project",
        content = "output",
        public = TRUE
      )

      yml_title <- .yml_dest_get_title("test-public-project", "osf", "default")
      expect_true(yml_title[["public"]])
      expect_identical(yml_title[["description"]], "A public project")
    }
  )
})

test_that("projr_yml_dest_add_osf_proj with id parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid OSF id must be 5 characters
      projr_yml_dest_add_osf_proj(
        title = "test-project-id",
        content = "output",
        id = "xyz12"
      )

      yml_title <- .yml_dest_get_title("test-project-id", "osf", "default")
      expect_identical(yml_title[["id"]], "xyz12")
    }
  )
})

test_that("projr_yml_dest_add_osf_proj requires content parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Content is required, should error with NULL
      expect_error(
        projr_yml_dest_add_osf_proj(
          title = "test-project-no-content",
          content = NULL
        ),
        "content must be given"
      )
    }
  )
})

# =============================================================================
# projr_yml_dest_add_osf_comp tests
# =============================================================================

test_that("projr_yml_dest_add_osf_comp requires id_parent", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should error when id_parent is missing
      expect_error(
        projr_yml_dest_add_osf_comp(
          title = "test-component",
          content = "output"
        ),
        "id_parent must be specified"
      )
    }
  )
})

test_that("projr_yml_dest_add_osf_comp creates OSF component with id_parent", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # id_parent must be 5 characters and category is required
      projr_yml_dest_add_osf_comp(
        title = "test-component",
        content = "output",
        category = "data",
        id_parent = "paren"
      )

      yml_title <- .yml_dest_get_title("test-component", "osf", "default")
      expect_identical(yml_title[["content"]], "output")
      expect_identical(yml_title[["id-parent"]], "paren")
    }
  )
})

test_that("projr_yml_dest_add_osf_comp with category parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # id_parent must be 5 characters
      projr_yml_dest_add_osf_comp(
        title = "test-comp-category",
        content = "output",
        category = "data",
        id_parent = "prnt5"
      )

      yml_title <- .yml_dest_get_title("test-comp-category", "osf", "default")
      expect_identical(yml_title[["category"]], "data")
    }
  )
})

test_that("projr_yml_dest_add_osf_comp with all parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  skip_if(!nzchar(Sys.getenv("OSF_PAT")), "OSF_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Both id and id_parent must be 5 characters
      projr_yml_dest_add_osf_comp(
        title = "test-comp-full",
        description = "Full component",
        content = "raw-data",
        public = TRUE,
        category = "analysis",
        id_parent = "abcde",
        id = "fghij"
      )

      yml_title <- .yml_dest_get_title("test-comp-full", "osf", "default")
      expect_identical(yml_title[["description"]], "Full component")
      expect_true(yml_title[["public"]])
      expect_identical(yml_title[["category"]], "analysis")
      expect_identical(yml_title[["id-parent"]], "abcde")
      expect_identical(yml_title[["id"]], "fghij")
    }
  )
})

test_that("projr_yml_dest_add_osf_comp requires content parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Content is required, should error with NULL
      expect_error(
        projr_yml_dest_add_osf_comp(
          title = "test-comp-no-content",
          content = NULL,
          id_parent = "abcde"
        ),
        "content must be given"
      )
    }
  )
})

# =============================================================================
# projr_yml_dest_add_local tests
# =============================================================================

test_that("projr_yml_dest_add_local validates required parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
  skip_if(.is_test_osf())
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
