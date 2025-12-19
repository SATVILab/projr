# Tests for R/yml-dest.R

# =============================================================================
# Basic getter and setter functions
# =============================================================================

test_that(".yml_dest_opt_vec returns expected remote types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  expect_identical(.yml_dest_opt_vec(), c("local", "github"))
})

test_that(".yml_dest_set_type and .yml_dest_get_type work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set a local destination
      yml_type <- list(
        "test-dest" = list(
          content = "raw-data",
          path = "_archive"
        )
      )
      .yml_dest_set_type(yml_type, "local", "default")

      # Get it back
      result <- .yml_dest_get_type("local", "default")
      expect_identical(result[["test-dest"]][["content"]], "raw-data")
      expect_identical(result[["test-dest"]][["path"]], "_archive")

      # Test with invalid type
      expect_error(.yml_dest_set_type(yml_type, "invalid", "default"))
      expect_error(.yml_dest_get_type("invalid", "default"))
    }
  )
})

test_that(".yml_dest_get_title retrieves correct title configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "my-archive",
        content = "output",
        path = "_archive"
      )

      result <- .yml_dest_get_title("my-archive", "local", "default")
      expect_identical(result[["content"]], "output")
      expect_identical(result[["path"]], "_archive")

      # Non-existent title returns NULL
      result_null <- .yml_dest_get_title("non-existent", "local", "default")
      expect_null(result_null)
    }
  )
})

test_that(".yml_dest_set_title works with overwrite parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      yml <- list(content = "raw-data", path = "_archive")

      # First time should work
      .yml_dest_set_title(yml, "test", "local", FALSE, "default")
      result <- .yml_dest_get_title("test", "local", "default")
      expect_identical(result[["path"]], "_archive")

      # Second time without overwrite should fail
      yml_new <- list(content = "output", path = "_new-archive")
      expect_error(
        .yml_dest_set_title(yml_new, "test", "local", FALSE, "default"),
        "already exists"
      )

      # With overwrite should succeed
      .yml_dest_set_title(yml_new, "test", "local", TRUE, "default")
      result <- .yml_dest_get_title("test", "local", "default")
      expect_identical(result[["path"]], "_new-archive")
    }
  )
})

# =============================================================================
# Removal functions
# =============================================================================

test_that(".yml_dest_rm_title removes specific title", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add two destinations
      projr_yml_dest_add_local(
        title = "archive1",
        content = "output",
        path = "_archive1"
      )
      projr_yml_dest_add_local(
        title = "archive2",
        content = "raw-data",
        path = "_archive2"
      )

      # Remove one
      .yml_dest_rm_title("archive1", "local", "default")

      # Check only archive2 remains
      yml_type <- .yml_dest_get_type("local", "default")
      expect_null(yml_type[["archive1"]])
      expect_identical(yml_type[["archive2"]][["path"]], "_archive2")
    }
  )
})

test_that(".yml_dest_rm_type removes entire type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add destinations of different types
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive"
      )
      projr_yml_dest_add_github(
        title = "release",
        content = "output"
      )

      # Remove local type
      .yml_dest_rm_type("local", "default")

      # Check local is gone but github remains
      expect_null(.yml_dest_get_type("local", "default"))
      expect_true(!is.null(.yml_dest_get_type("github", "default")))
    }
  )
})

test_that(".yml_dest_rm_type_all removes all destination types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add multiple types
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive"
      )
      projr_yml_dest_add_github(
        title = "release",
        content = "output"
      )

      # Remove all
      .yml_dest_rm_type_all("default")

      # Check all are gone
      expect_null(.yml_dest_get_type("local", "default"))
      expect_null(.yml_dest_get_type("github", "default"))
    }
  )
})

test_that(".yml_dest_used_get returns currently used types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_dest_rm_type_all("default")

      # No destinations
      expect_length(.yml_dest_used_get("default"), 0)

      # Add local
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive"
      )
      used <- .yml_dest_used_get("default")
      expect_true("local" %in% used)
      expect_false("github" %in% used)

      # Add github
      projr_yml_dest_add_github(
        title = "release",
        content = "output"
      )
      used <- .yml_dest_used_get("default")
      expect_true("local" %in% used)
      expect_true("github" %in% used)
    }
  )
})

# =============================================================================
# List building functions
# =============================================================================

test_that(".yml_dest_add_get_list_add builds correct list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  result <- .yml_dest_add_get_list_add(
    content = "raw-data",
    path = "/test/path",
    path_append_label = TRUE,
    structure = "archive",
    get_list = list(strategy = "sync"),
    send_list = list(cue = "always")
  )

  expect_identical(result[["content"]], "raw-data")
  expect_identical(result[["path"]], "/test/path")
  expect_identical(result[["path-append-label"]], TRUE)
  expect_identical(result[["structure"]], "archive")
  expect_identical(result[["get"]][["strategy"]], "sync")
  expect_identical(result[["send"]][["cue"]], "always")
})

test_that(".yml_dest_add_get_list_add_extra switches on type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # For non-OSF types, should return list unchanged
  list_add <- list(content = "output", path = "/path")
  result <- .yml_dest_add_get_list_add_extra(
    list_add,
    type = "local",
    id = NULL,
    id_parent = NULL,
    title = "test",
    description = NULL
  )
  expect_identical(result, list_add)

  # For GitHub type
  result <- .yml_dest_add_get_list_add_extra(
    list_add,
    type = "github",
    id = NULL,
    id_parent = NULL,
    title = "test",
    description = NULL
  )
  expect_identical(result, list_add)
})

# =============================================================================
# Completion functions
# =============================================================================

test_that(".yml_dest_complete_title_structure sets default structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml <- list()
  result <- .yml_dest_complete_title_structure(yml, "local")
  expect_identical(result[["structure"]], "archive")

  # Existing structure should be preserved
  yml <- list(structure = "latest")
  result <- .yml_dest_complete_title_structure(yml, "local")
  expect_identical(result[["structure"]], "latest")
})

test_that(".yml_dest_complete_title_cue sets default cue", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml <- list()
  result <- .yml_dest_complete_title_cue(yml, "local")
  expect_identical(result[["cue"]], "if-change")

  # Existing cue should be preserved
  yml <- list(cue = "always")
  result <- .yml_dest_complete_title_cue(yml, "local")
  expect_identical(result[["cue"]], "always")
})

test_that(".yml_dest_complete_title_upload_inspect sets default inspect", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml <- list()
  result <- .yml_dest_complete_title_upload_inspect(yml, "local")
  expect_identical(result[["inspect"]], "manifest")

  # Existing inspect should be preserved
  yml <- list(inspect = "file")
  result <- .yml_dest_complete_title_upload_inspect(yml, "local")
  expect_identical(result[["inspect"]], "file")
})

test_that(".yml_dest_complete_title_strategy_default sets appropriate strategy", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # When strategy is NULL and inspect is "none", should be "upload-all"
  result <- .yml_dest_complete_title_strategy_default(NULL, "none")
  expect_identical(result, "upload-all")

  # When strategy is NULL and inspect is not "none", should be "sync-diff"
  result <- .yml_dest_complete_title_strategy_default(NULL, "manifest")
  expect_identical(result, "sync-diff")

  # When strategy is already set, should be preserved
  result <- .yml_dest_complete_title_strategy_default("sync-purge", "manifest")
  expect_identical(result, "sync-purge")
})

test_that(".yml_dest_complete_title_upload_strategy completes strategy for different types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Local type
  yml <- list(inspect = "manifest")
  result <- .yml_dest_complete_title_upload_strategy(yml, "local")
  expect_identical(result[["strategy"]], "sync-diff")

  # GitHub type
  yml <- list(inspect = "none")
  result <- .yml_dest_complete_title_upload_strategy(yml, "github")
  expect_identical(result[["strategy"]], "upload-all")

  # Existing strategy preserved
  yml <- list(strategy = "upload-missing", inspect = "manifest")
  result <- .yml_dest_complete_title_upload_strategy(yml, "local")
  expect_identical(result[["strategy"]], "upload-missing")
})

test_that(".yml_dest_complete_title_path_append_label sets default", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml <- list()
  result <- .yml_dest_complete_title_path_append_label(yml, "local")
  expect_identical(result[["path-append-label"]], TRUE)
})

test_that(".yml_dest_complete_title_path sets default path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml <- list()
  result <- .yml_dest_complete_title_path(yml, "local")
  expect_null(result[["path"]])

  # Existing path preserved
  yml <- list(path = "/custom/path")
  result <- .yml_dest_complete_title_path(yml, "local")
  expect_identical(result[["path"]], "/custom/path")
})

test_that(".yml_dest_complete_title_id completes ID for different types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Local type - id should be set to path
  yml <- list(path = "/test/path")
  result <- .yml_dest_complete_title_id(yml, "local", "title")
  expect_identical(result[["id"]], "/test/path")

  # GitHub type - id should be set to title
  yml <- list()
  result <- .yml_dest_complete_title_id(yml, "github", "my-release")
  expect_identical(result[["id"]], "my-release")

})

test_that(".yml_dest_complete_title applies all completions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Local destination
  yml_title <- list(
    title = "test",
    type = "local",
    content = "output",
    path = "/archive"
  )
  result <- .yml_dest_complete_title(yml_title, "test", "local")

  expect_identical(result[["structure"]], "archive")
  expect_identical(result[["send"]][["cue"]], "if-change")
  expect_identical(result[["send"]][["inspect"]], "manifest")
  expect_identical(result[["send"]][["strategy"]], "sync-diff")
  expect_identical(result[["path-append-label"]], TRUE)
  expect_identical(result[["id"]], "/archive")
})

# =============================================================================
# Strategy and cue setter functions
# =============================================================================

test_that(".yml_dest_set_send_strategy updates strategy", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive",
        send_strategy = "sync-diff"
      )

      # Change strategy
      .yml_dest_set_send_strategy(
        "upload-all",
        "archive",
        "local",
        "default"
      )

      yml_title <- .yml_dest_get_title("archive", "local", "default")
      expect_identical(yml_title[["send"]][["strategy"]], "upload-all")

      # Invalid strategy should error
      expect_error(
        .yml_dest_set_send_strategy("invalid", "archive", "local", "default")
      )
    }
  )
})

test_that(".yml_dest_set_send_cue updates cue", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive",
        send_cue = "if-change"
      )

      # Change cue
      .yml_dest_set_send_cue("always", "archive", "local", "default")

      yml_title <- .yml_dest_get_title("archive", "local", "default")
      expect_identical(yml_title[["cue"]], "always")

      # Invalid cue should error
      expect_error(
        .yml_dest_set_send_cue("invalid", "archive", "local", "default")
      )
    }
  )
})

# =============================================================================
# Title completion with parameters
# =============================================================================

test_that(".yml_dest_get_title_complete_param_init creates correct structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # When title is "archive" and archive_type is "raw-data", content should be "raw-data"
      result <- .yml_dest_get_title_complete_param_init(
        "archive",
        "local",
        "raw-data"
      )

      expect_identical(result[["title"]], "archive")
      expect_identical(result[["type"]], "local")
      expect_true("raw-data" %in% result[["content"]])
    }
  )
})

test_that(".yml_dest_get_title_complete_param_force sets send cue", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_title <- list(send = list())

  # always_archive TRUE
  result <- .yml_dest_get_title_complete_param_force(yml_title, TRUE)
  expect_identical(result[["send"]][["cue"]], "always")

  # always_archive FALSE
  result <- .yml_dest_get_title_complete_param_force(yml_title, FALSE)
  expect_identical(result[["send"]][["cue"]], "if-change")
})

test_that(".yml_dest_get_title_complete works with parameter-based config", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_dest_add_local(
        title = "archive",
        content = "output",
        path = "_archive"
      )

      # Get completed title with parameter override
      result <- .yml_dest_get_title_complete(
        "archive",
        "local",
        "default",
        archive_type = "output",
        always_archive = TRUE
      )

      expect_identical(result[["send"]][["cue"]], "always")
    }
  )
})


# =============================================================================
# Integration tests with exported functions
# =============================================================================

test_that(".yml_dest_add_impl creates proper destination structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_dest_add_impl(
        type = "local",
        title = "my-archive",
        content = "output",
        structure = "archive",
        path = "/test/archive",
        path_append_label = TRUE,
        get_list = NULL,
        send_list = list(
          cue = "always",
          strategy = "sync-diff",
          inspect = "manifest"
        ),
        overwrite = FALSE,
        description = NULL,
        id = NULL,
        id_parent = NULL,
        profile = "default"
      )

      yml_title <- .yml_dest_get_title("my-archive", "local", "default")
      expect_identical(yml_title[["content"]], "output")
      expect_identical(yml_title[["path"]], "/test/archive")
      expect_identical(yml_title[["structure"]], "archive")
    }
  )
})

test_that(".yml_dest_add works through the full pipeline", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with local destination
      .yml_dest_add(
        role = "destination",
        type = "local",
        title = "test-local",
        content = "output",
        structure = "archive",
        path = "/test/path",
        path_append_label = TRUE,
        overwrite = FALSE,
        description = NULL,
        id = NULL,
        id_parent = NULL,
        get_strategy = NULL,
        get_conflict = NULL,
        send_cue = "always",
        send_strategy = "sync-diff",
        send_inspect = "manifest",
        profile = "default"
      )

      yml_title <- .yml_dest_get_title("test-local", "local", "default")
      expect_identical(yml_title[["content"]], "output")
      expect_identical(yml_title[["structure"]], "archive")
      expect_identical(yml_title[["send"]][["cue"]], "always")
      expect_identical(yml_title[["send"]][["strategy"]], "sync-diff")
    }
  )
})
