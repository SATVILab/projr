# Unit Tests for R/dest-send-prepare.R
# =====================================
#
# Tests for internal functions in dest-send-prepare.R
# All tests run in LITE mode (no skip_if(.is_test_lite()))
# Tests focus on core logic without requiring full builds

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# .dest_github_tags_needed() tests
# =============================================================================

test_that(".dest_github_tags_needed returns empty when no GitHub destinations", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all GitHub destinations
      .yml_dest_rm_type_all("default")

      # Should return empty character vector
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      expect_identical(tags, character(0L))
      expect_identical(length(tags), 0L)
    }
  )
})

test_that(".dest_github_tags_needed returns tags from YAML configuration", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations first
      .yml_dest_rm_type_all("default")

      # Add GitHub destination with specific title (id is auto-set to title)
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )

      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      expect_true(is.character(tags))
      expect_true(length(tags) > 0)
      expect_true("v1.0.0" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed adds archive when archive_github is TRUE", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # No GitHub destinations in YAML, but archive_github = TRUE
      tags <- .dest_github_tags_needed(
        archive_github = TRUE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should have at least the archive tag
      expect_true(is.character(tags))
      expect_true(length(tags) > 0)
    }
  )
})

test_that(".dest_github_tags_needed does not duplicate archive title", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations first
      .yml_dest_rm_type_all("default")

      # Add GitHub destination with title "archive"
      projr_yml_dest_add_github(
        title = "archive",
        content = "output"
      )

      # archive_github = TRUE should not add duplicate
      tags <- .dest_github_tags_needed(
        archive_github = TRUE,
        always_archive = FALSE,
        profile = NULL
      )

      expect_true(is.character(tags))
      # Should only have one unique tag even if archive appears twice
      expect_identical(length(tags), length(unique(tags)))
    }
  )
})

test_that(".dest_github_tags_needed handles @version substitution", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add GitHub destination with @version placeholder as title
      projr_yml_dest_add_github(
        title = "@version",
        content = "output"
      )

      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      expect_true(is.character(tags))
      expect_true(length(tags) > 0)

      # Should have replaced @version with actual version (prefixed with "v")
      current_version <- projr_version_get()
      current_version_v <- paste0("v", current_version)
      expect_true(current_version_v %in% tags)
      expect_false("@version" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed returns unique tags", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add first destination with a specific title
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )
      # Add second destination with same title, overwriting the first
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "raw-data",
        overwrite = TRUE
      )
      # Add third destination with different title
      projr_yml_dest_add_github(
        title = "v1.0.1",
        content = "docs"
      )

      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      expect_true(is.character(tags))
      # Should have only unique tags (function deduplicates)
      expect_identical(tags, unique(tags))
      # Should have both v1.0.0 and v1.0.1
      expect_true("v1.0.0" %in% tags)
      expect_true("v1.0.1" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed handles NULL archive_github", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add a GitHub destination (title becomes id)
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )

      # archive_github = NULL should not cause errors
      tags <- .dest_github_tags_needed(
        archive_github = NULL,
        always_archive = FALSE,
        profile = NULL
      )

      expect_true(is.character(tags))
      expect_true("v1.0.0" %in% tags)
    }
  )
})

# =============================================================================
# .dest_prepare_github_releases() tests
# =============================================================================

test_that(".dest_prepare_github_releases exits early when not output build", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # bump_component = "dev" means not an output build
      result <- .dest_prepare_github_releases(
        bump_component = "dev",
        archive_github = TRUE,
        archive_local = FALSE,
        always_archive = FALSE,
        strict = TRUE,
        output_level = "none"
      )

      expect_identical(result, FALSE)
    }
  )
})

test_that(".dest_prepare_github_releases skips when no GitHub destinations", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Only add local destination, no GitHub
      projr_yml_dest_add_local(
        title = "local-archive",
        content = "output",
        path = "_archive"
      )

      # Should skip when no GitHub destinations
      result <- .dest_prepare_github_releases(
        bump_component = "patch",
        archive_github = FALSE,
        archive_local = TRUE,
        always_archive = FALSE,
        strict = TRUE,
        output_level = "none"
      )

      expect_identical(result, FALSE)
    }
  )
})

test_that(".dest_prepare_github_releases skips when no tags needed", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # archive_github = FALSE means no tags needed
      result <- .dest_prepare_github_releases(
        bump_component = "patch",
        archive_github = FALSE,
        archive_local = FALSE,
        always_archive = FALSE,
        strict = TRUE,
        output_level = "none"
      )

      expect_identical(result, FALSE)
    }
  )
})

test_that(".dest_prepare_github_releases handles output_level parameter", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Test with different output levels - should not error
      expect_no_error({
        .dest_prepare_github_releases(
          bump_component = "patch",
          archive_github = FALSE,
          archive_local = FALSE,
          always_archive = FALSE,
          strict = TRUE,
          output_level = "none"
        )
      })

      expect_no_error({
        .dest_prepare_github_releases(
          bump_component = "patch",
          archive_github = FALSE,
          archive_local = FALSE,
          always_archive = FALSE,
          strict = TRUE,
          output_level = "std"
        )
      })

      expect_no_error({
        .dest_prepare_github_releases(
          bump_component = "patch",
          archive_github = FALSE,
          archive_local = FALSE,
          always_archive = FALSE,
          strict = TRUE,
          output_level = "debug"
        )
      })
    }
  )
})

# Note: Tests that would actually create GitHub releases are intentionally
# excluded from LITE mode to avoid requiring GitHub authentication and
# rate limits. These tests would be in a comprehensive test file if needed.
