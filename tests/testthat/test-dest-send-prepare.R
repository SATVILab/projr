# Unit Tests for R/dest-send-prepare.R
# =======================================
#
# Tests for internal functions in dest-send-prepare.R
# Tests focus on tag preparation and GitHub release creation logic
# Most tests run in LITE mode (no skip_if(.is_test_lite()))
# Tests that actually create GitHub releases skip in CRAN/LITE modes

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# .dest_github_tags_needed() - Core tag derivation logic
# =============================================================================

test_that(".dest_github_tags_needed returns empty for no destinations", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all GitHub destinations
      .yml_dest_rm_type_all("default")

      # With no destinations and no archive_github
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      expect_identical(tags, character(0L))
    }
  )
})

test_that(".dest_github_tags_needed returns tags from YAML configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations first
      .yml_dest_rm_type_all("default")

      # Add a GitHub destination - title becomes the tag/id
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )

      # Get tags
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should return the configured tag (formatted)
      expect_true(is.character(tags))
      expect_true(length(tags) > 0)
      expect_true("v1.0.0" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed adds archive tag from parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # With archive_github = TRUE and no "archive" title in YAML
      tags <- .dest_github_tags_needed(
        archive_github = TRUE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should return archive tag
      expect_true(is.character(tags))
      expect_true(length(tags) > 0)

      # archive title should be present (will use @version or similar)
      expect_true(length(tags) == 1)
    }
  )
})

test_that(".dest_github_tags_needed uses @version placeholder", {
  skip_if(.is_test_cran())
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

      # Get tags
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should replace @version with actual version
      current_version <- .version_get_v()
      expect_true(current_version %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed handles multiple destinations", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add multiple GitHub destinations - titles become tags
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )

      projr_yml_dest_add_github(
        title = "v2.0.0",
        content = "docs"
      )

      # Get tags
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should return both tags
      expect_true(length(tags) >= 2)
      expect_true("v1.0.0" %in% tags)
      expect_true("v2.0.0" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed removes duplicate tags", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add multiple destinations with same title/tag
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )

      # Note: Can't add another with same title due to overwrite behavior
      # This test verifies unique() in .dest_github_tags_needed
      # Get tags
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should return unique tags only
      expect_identical(length(tags), length(unique(tags)))
      expect_true("v1.0.0" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed handles archive_github character vector", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Use character vector for archive_github
      tags <- .dest_github_tags_needed(
        archive_github = c("output", "docs"),
        always_archive = FALSE,
        profile = NULL
      )

      # Should return a tag for archive
      expect_true(is.character(tags))
      expect_true(length(tags) > 0)
    }
  )
})

test_that(".dest_github_tags_needed does not add archive if already in YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add archive destination to YAML - title "archive" is special
      projr_yml_dest_add_github(
        title = "archive",
        content = "output"
      )

      # archive_github = TRUE should not add another archive
      tags <- .dest_github_tags_needed(
        archive_github = TRUE,
        always_archive = FALSE,
        profile = NULL
      )

      # Should only have one tag (archive)
      expect_true(length(tags) == 1)
      expect_true("archive" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed handles NULL archive_github", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add a YAML destination - title becomes tag
      projr_yml_dest_add_github(
        title = "v1.0.0",
        content = "output"
      )

      # archive_github = NULL should not add archive
      tags <- .dest_github_tags_needed(
        archive_github = NULL,
        always_archive = FALSE,
        profile = NULL
      )

      # Should only have YAML-configured tag
      expect_true(length(tags) == 1)
      expect_true("v1.0.0" %in% tags)
    }
  )
})

test_that(".dest_github_tags_needed formats tags with spaces", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Add destination with spaces in title
      # projr_yml_dest_add_github automatically replaces spaces with hyphens
      projr_yml_dest_add_github(
        title = "tag with spaces",
        content = "output"
      )

      # Get tags
      tags <- .dest_github_tags_needed(
        archive_github = FALSE,
        always_archive = FALSE,
        profile = NULL
      )

      # Spaces should be replaced with hyphens
      expect_true("tag-with-spaces" %in% tags)
      expect_false(any(grepl(" ", tags)))
    }
  )
})

# =============================================================================
# .dest_prepare_github_releases() - Release preparation logic
# =============================================================================

test_that(".dest_prepare_github_releases returns FALSE for non-output build", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # bump_component = NULL means not an output build
      result <- .dest_prepare_github_releases(
        bump_component = NULL,
        archive_github = TRUE,
        archive_local = FALSE,
        always_archive = FALSE,
        strict = FALSE
      )

      expect_false(result)
    }
  )
})

test_that(".dest_prepare_github_releases returns FALSE when no GitHub destinations", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # Only local destination, no GitHub
      result <- .dest_prepare_github_releases(
        bump_component = "patch",
        archive_github = FALSE,
        archive_local = TRUE,
        always_archive = FALSE,
        strict = FALSE
      )

      expect_false(result)
    }
  )
})

test_that(".dest_prepare_github_releases returns FALSE when no tags needed", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove all destinations
      .yml_dest_rm_type_all("default")

      # archive_github = FALSE and no YAML destinations
      result <- .dest_prepare_github_releases(
        bump_component = "patch",
        archive_github = FALSE,
        archive_local = FALSE,
        always_archive = FALSE,
        strict = FALSE
      )

      expect_false(result)
    }
  )
})

test_that("release preparation creates only the missing configured tags", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE)
  created <- character()
  testthat::local_mocked_bindings(
    .remote_check_exists = function(type, id) id == "existing",
    .remote_create = function(type, id) {
      expect_identical(type, "github")
      created <<- c(created, id)
    }
  )
  usethis::with_project(dir_test, {
    for (tag in c("existing", "first-new", "second-new")) {
      projr_yml_dest_add_github(title = tag, content = "output")
    }
    .dest_prepare_github_releases("patch", FALSE, FALSE, FALSE, FALSE)
    expect_setequal(created, c("first-new", "second-new"))
  })
})

test_that("an implicit GitHub archive checks credentials before publishing", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    local_mocked_bindings(.auth_get_github_pat_find = function(...) "")
    expect_error(
      .dest_prepare_github_releases("patch", TRUE, FALSE, FALSE, TRUE),
      "Publishing a GitHub release requires a token"
    )
  })
})
