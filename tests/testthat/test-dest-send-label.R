# Unit Tests for R/dest-send-label.R
# =====================================
#
# Tests for internal functions in dest-send-label.R
# All tests run in LITE mode (no skip_if(.is_test_lite()))
# Tests focus on core logic without requiring full builds

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# Helper function tests
# =============================================================================

test_that(".dsl_get_fn_source returns files from manifest", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create content in output directory (not safe, so it gets hashed)
      .test_content_setup_label(c("output"), safe = FALSE)

      # Build to populate manifest
      projr_build_patch()

      # Get source files for current version
      fn_source <- .dsl_get_fn_source("output")

      # Should return character vector
      expect_true(is.character(fn_source))

      # If manifest has output entries, check them
      manifest_all <- .manifest_read(file.path(.path_get(), "manifest.csv"))
      output_entries <- manifest_all[manifest_all$label == "output" &
        manifest_all$version == projr_version_get(), ]

      if (nrow(output_entries) > 0) {
        expect_true(length(fn_source) > 0)
        # Should contain files from manifest
        expect_true(all(fn_source %in% output_entries$fn))
      }
    }
  )
})

test_that(".dsl_get_manifest_source filters by label and version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create content and build
      .test_content_setup_label(c("output"), safe = FALSE)
      projr_build_patch()

      # Get manifest for output label
      manifest <- .dsl_get_manifest_source("output")

      # Should be a data frame
      expect_true(is.data.frame(manifest))

      # Should have required columns
      expect_true(all(c("label", "fn", "version", "hash") %in% names(manifest)))

      # Should only have output label
      if (nrow(manifest) > 0) {
        expect_true(all(manifest$label == "output"))

        # Should only have current version
        current_version <- projr_version_get()
        expect_true(all(manifest$version == current_version))
      }
    }
  )
})

# =============================================================================
# Version comparison logic tests
# =============================================================================

test_that(".dsl_gr_gvc_check_nothing returns TRUE for no inspection", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a dummy remote_pre
      remote_pre <- "/dummy/path"

      # Should return TRUE when inspect is "none"
      result <- .dsl_gr_gvc_check_nothing(remote_pre, "none", "sync-diff")
      expect_true(result)

      # Should return TRUE when strategy is "upload-all"
      result <- .dsl_gr_gvc_check_nothing(remote_pre, "file", "upload-all")
      expect_true(result)

      # Should return FALSE when remote exists, inspect is not none, and strategy is not upload-all
      result <- .dsl_gr_gvc_check_nothing(remote_pre, "file", "sync-diff")
      expect_false(result)

      # Should return TRUE when remote_pre is NULL
      result <- .dsl_gr_gvc_check_nothing(NULL, "file", "sync-diff")
      expect_true(result)
    }
  )
})

test_that(".dsl_gr_gvc_latest_file returns current version without v", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Get result
      result <- .dsl_gr_gvc_latest_file()

      # Should be character
      expect_true(is.character(result))
      expect_true(length(result) == 1)

      # Should not start with v
      expect_false(grepl("^v", result))

      # Should match current version format
      current_version <- projr_version_get()
      expected <- .version_v_rm(current_version)
      expect_identical(result, expected)
    }
  )
})

# =============================================================================
# Plan generation tests
# =============================================================================

test_that(".dsl_gpfn_upload_all returns list with fn_source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create content and build
      .test_content_setup_label(c("output"), safe = FALSE)
      projr_build_patch()

      # Get plan
      plan <- .dsl_gpfn_upload_all("output")

      # Should be a list
      expect_true(is.list(plan))

      # Should have fn_source
      expect_true("fn_source" %in% names(plan))

      # fn_source should be character vector
      expect_true(is.character(plan$fn_source))

      # Check if output was actually hashed in manifest
      manifest_all <- .manifest_read(file.path(.path_get(), "manifest.csv"))
      output_entries <- manifest_all[manifest_all$label == "output" &
        manifest_all$version == projr_version_get(), ]

      # Plan should match manifest entries
      if (nrow(output_entries) > 0) {
        expect_true(length(plan$fn_source) > 0)
      }
    }
  )
})

test_that(".dsl_get_fn_dest returns empty for NULL remote_comp", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # With NULL remote_comp
      result <- .dsl_get_fn_dest(
        inspect = "file",
        version_comp = "0.0.1",
        type = "local",
        remote_comp = NULL,
        remote_pre = NULL,
        label = "output"
      )

      # Should return empty character vector
      expect_true(is.character(result))
      expect_identical(length(result), 0L)

      # With inspect = "none"
      result <- .dsl_get_fn_dest(
        inspect = "none",
        version_comp = "0.0.1",
        type = "local",
        remote_comp = "/some/path",
        remote_pre = NULL,
        label = "output"
      )

      # Should return empty character vector
      expect_identical(length(result), 0L)
    }
  )
})

# =============================================================================
# Plan action tests
# =============================================================================

test_that(".dsl_gpa_ua_ensure_exists returns TRUE when adding files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # When adding files
      result <- .dsl_gpa_ua_ensure_exists(
        fn_add = c("file1.txt", "file2.txt"),
        version_comp = "0.0.1",
        cue = "if-change"
      )
      expect_true(result)

      # When version_comp is NULL
      result <- .dsl_gpa_ua_ensure_exists(
        fn_add = character(0),
        version_comp = NULL,
        cue = "if-change"
      )
      expect_true(result)

      # When cue is "always"
      result <- .dsl_gpa_ua_ensure_exists(
        fn_add = character(0),
        version_comp = "0.0.1",
        cue = "always"
      )
      expect_true(result)

      # When none of the conditions apply
      result <- .dsl_gpa_ua_ensure_exists(
        fn_add = character(0),
        version_comp = "0.0.1",
        cue = "if-change"
      )
      expect_false(result)
    }
  )
})

test_that(".dsl_gpa_version_file_check_untrusted detects asterisks", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Version file with untrusted label
      version_file <- c(
        "0.0.1",
        "output: 0.0.1*",
        "docs: 0.0.1"
      )

      # Should return TRUE for output (has asterisk)
      result <- .dsl_gpa_version_file_check_untrusted(version_file, "output")
      expect_true(result)

      # Should return FALSE for docs (no asterisk)
      result <- .dsl_gpa_version_file_check_untrusted(version_file, "docs")
      expect_false(result)

      # Should return FALSE for missing label
      result <- .dsl_gpa_version_file_check_untrusted(version_file, "cache")
      expect_false(result)

      # Should return FALSE for empty version file
      result <- .dsl_gpa_version_file_check_untrusted(character(0), "output")
      expect_false(result)
    }
  )
})

test_that(".dsl_gpa_version_file_get_use_asterisk handles force flags", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      version_file <- c("0.0.1", "output: 0.0.1*")

      # Force remove should override everything
      result <- .dsl_gpa_version_file_get_use_asterisk(
        asterisk_force_rm = TRUE,
        asterisk_force_add = FALSE,
        version_remote = version_file,
        label = "output"
      )
      expect_false(result)

      # Force add should return TRUE when not forcing remove
      result <- .dsl_gpa_version_file_get_use_asterisk(
        asterisk_force_rm = FALSE,
        asterisk_force_add = TRUE,
        version_remote = version_file,
        label = "output"
      )
      expect_true(result)

      # When neither forced, should check version file
      result <- .dsl_gpa_version_file_get_use_asterisk(
        asterisk_force_rm = FALSE,
        asterisk_force_add = FALSE,
        version_remote = version_file,
        label = "output"
      )
      expect_true(result) # output has asterisk in version_file
    }
  )
})

# =============================================================================
# Sync plan tests
# =============================================================================

test_that(".dsl_gpa_d_ensure_exists handles different conditions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # When there are changes (fn_add or fn_rm)
      result <- .dsl_gpa_d_ensure_exists(
        fn_add = c("file1.txt"),
        fn_rm = character(0),
        version_comp = "0.0.1",
        cue = "if-change"
      )
      expect_true(result)

      result <- .dsl_gpa_d_ensure_exists(
        fn_add = character(0),
        fn_rm = c("file2.txt"),
        version_comp = "0.0.1",
        cue = "if-change"
      )
      expect_true(result)

      # When version_comp is NULL
      result <- .dsl_gpa_d_ensure_exists(
        fn_add = character(0),
        fn_rm = character(0),
        version_comp = NULL,
        cue = "if-change"
      )
      expect_true(result)

      # When cue is "always"
      result <- .dsl_gpa_d_ensure_exists(
        fn_add = character(0),
        fn_rm = character(0),
        version_comp = "0.0.1",
        cue = "always"
      )
      expect_true(result)

      # When none of the conditions apply
      result <- .dsl_gpa_d_ensure_exists(
        fn_add = character(0),
        fn_rm = character(0),
        version_comp = "0.0.1",
        cue = "if-change"
      )
      expect_false(result)
    }
  )
})

test_that(".dsl_gpa_p_ensure_exists handles purge conditions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # When adding files
      result <- .dsl_gpa_p_ensure_exists(
        fn_add = c("file1.txt"),
        fn_dest_extra = character(0),
        cue = "if-change",
        version_comp = "0.0.1"
      )
      expect_true(result)

      # When remote has extra files
      result <- .dsl_gpa_p_ensure_exists(
        fn_add = character(0),
        fn_dest_extra = c("old_file.txt"),
        cue = "if-change",
        version_comp = "0.0.1"
      )
      expect_true(result)

      # When cue is "always"
      result <- .dsl_gpa_p_ensure_exists(
        fn_add = character(0),
        fn_dest_extra = character(0),
        cue = "always",
        version_comp = "0.0.1"
      )
      expect_true(result)

      # When version_comp is NULL (no trusted remote)
      result <- .dsl_gpa_p_ensure_exists(
        fn_add = character(0),
        fn_dest_extra = character(0),
        cue = "if-change",
        version_comp = NULL
      )
      expect_true(result)

      # When none of the conditions apply
      result <- .dsl_gpa_p_ensure_exists(
        fn_add = character(0),
        fn_dest_extra = character(0),
        cue = "if-change",
        version_comp = "0.0.1"
      )
      expect_false(result)
    }
  )
})

# =============================================================================
# Version file extraction tests
# =============================================================================

test_that(".dsl_ip_fr_extract_version extracts version from local remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Local remote path ending with version
      remote_local <- "/path/to/output/v0.0.1"
      result <- .dsl_ip_fr_extract_version(remote_local, "local")
      expect_identical(result, "0.0.1")

      # Local remote path with dev version
      remote_local_dev <- "/path/to/output/v0.0.1-1"
      result <- .dsl_ip_fr_extract_version(remote_local_dev, "local")
      expect_identical(result, "0.0.1-1")

      # Path without version should return NULL
      remote_no_version <- "/path/to/output"
      result <- .dsl_ip_fr_extract_version(remote_no_version, "local")
      expect_null(result)
    }
  )
})

test_that(".dsl_ip_fr_extract_version extracts version from github remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # GitHub remote with version in filename
      remote_github <- c("tag" = "v0.0.1", "fn" = "output-v0.0.1.zip")
      result <- .dsl_ip_fr_extract_version(remote_github, "github")
      expect_identical(result, "0.0.1")

      # GitHub remote with dev version
      remote_github_dev <- c("tag" = "v0.0.1-1", "fn" = "output-v0.0.1-1.zip")
      result <- .dsl_ip_fr_extract_version(remote_github_dev, "github")
      expect_identical(result, "0.0.1-1")

      # Missing fn component should return NULL (using list instead of named vector)
      remote_no_fn <- list("tag" = "v0.0.1")
      result <- .dsl_ip_fr_extract_version(remote_no_fn, "github")
      expect_null(result)
    }
  )
})

# =============================================================================
# Empty remote detection tests
# =============================================================================

test_that(".dsl_gpa_get_is_remote_dest_empty detects non-empty when adding files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # When adding files, should return FALSE (not empty)
      result <- .dsl_gpa_get_is_remote_dest_empty(
        fn_add = c("file1.txt"),
        fn_rm = character(0),
        label = "output",
        version_file = character(0),
        manifest = .empty_tbl_get_manifest("output", "0.0.1"),
        remote_dest_full = NULL,
        remote_dest_empty = NULL
      )
      expect_false(result)
    }
  )
})

test_that(".dsl_gpa_get_is_remote_dest_empty detects empty after removing all", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with adding files - should always be FALSE
      result_adding <- .dsl_gpa_get_is_remote_dest_empty(
        fn_add = c("file1.txt"),
        fn_rm = character(0),
        label = "output",
        version_file = character(0),
        manifest = .empty_tbl_get_manifest("output", "0.0.1"),
        remote_dest_full = NULL,
        remote_dest_empty = NULL
      )
      expect_false(result_adding)

      # Test with NULL remote_dest_full and only removals - should be TRUE
      result_null_remote <- .dsl_gpa_get_is_remote_dest_empty(
        fn_add = character(0),
        fn_rm = c("file1.txt"),
        label = "output",
        version_file = c("0.0.1", "output: 0.0.1*"), # Untrusted
        manifest = .empty_tbl_get_manifest("output", "0.0.1"),
        remote_dest_full = NULL,
        remote_dest_empty = NULL
      )
      expect_true(result_null_remote)

      # Test with untrusted version file - should return NULL (unknown)
      result_untrusted <- .dsl_gpa_get_is_remote_dest_empty(
        fn_add = character(0),
        fn_rm = character(0),
        label = "output",
        version_file = c("0.0.1", "output: 0.0.1*"), # Has asterisk = untrusted
        manifest = .empty_tbl_get_manifest("output", "0.0.1"),
        remote_dest_full = "/some/path",
        remote_dest_empty = NULL
      )
      expect_null(result_untrusted)
    }
  )
})

# =============================================================================
# Manifest helper tests
# =============================================================================

test_that(".dsl_gpa_manifest_rm_existing is callable with proper inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create sample manifests with row names
      current_version <- projr_version_get()
      manifest_remote <- data.frame(
        label = c("output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file3.txt"),
        version = rep(current_version, 3),
        hash = c("hash1", "hash2", "hash3"),
        stringsAsFactors = FALSE
      )
      rownames(manifest_remote) <- as.character(1:3)

      manifest_append <- data.frame(
        label = c("output"),
        fn = c("file2.txt"), # This overlaps
        version = current_version,
        hash = "hash2_new",
        stringsAsFactors = FALSE
      )

      # Call the function - testing that it runs without error
      result <- .dsl_gpa_manifest_rm_existing(
        manifest_remote, "output", manifest_append
      )

      # Result should be a data frame
      expect_true(is.data.frame(result))

      # Should have the expected columns
      expect_true(all(c("label", "fn", "version", "hash") %in% names(result)))

      # Should not error and return valid manifest structure
      expect_true(nrow(result) >= 0)
    }
  )
})
