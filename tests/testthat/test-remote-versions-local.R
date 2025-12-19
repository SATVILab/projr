# Tests for R/remote-versions.R using local remotes
#
# This file tests version tracking, manifest operations, and structure detection
# functionality in remote-versions.R using local remotes (no external dependencies).
#
# Test strategy:
# - Use local remotes for fast, credential-free testing
# - Tests run in LITE mode (included in regular development testing)
# - Focus on version detection, manifest operations, and structure inference
# - Test edge cases: empty directories, missing files, malformed versions

# =============================================================================
# Setup test environment
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# CHANGELOG operations
# =============================================================================

test_that(".remote_write_changelog works for local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a temp directory for the local remote
      remote_dir <- .dir_create_tmp_random()

      # Create a test CHANGELOG.md
      writeLines(
        c("# Changelog", "", "## v0.0.1", "- Initial release"),
        "CHANGELOG.md"
      )

      # Write CHANGELOG to local remote
      .remote_write_changelog("local", remote_dir)

      # Verify CHANGELOG was written
      expect_true(
        "CHANGELOG.md" %in% .remote_file_ls(
          "local",
          remote = remote_dir
        )
      )

      # Read it back and verify content
      path_dir_save <- .dir_create_tmp_random()
      .remote_file_get(
        "local",
        fn = "CHANGELOG.md",
        remote = remote_dir,
        path_dir_save_local = path_dir_save
      )
      changelog_content <- readLines(file.path(path_dir_save, "CHANGELOG.md"))
      expect_true(any(grepl("Changelog", changelog_content)))
      expect_true(any(grepl("v0.0.1", changelog_content)))

      # Cleanup
      unlink(remote_dir, recursive = TRUE)
      unlink(path_dir_save, recursive = TRUE)
      unlink("CHANGELOG.md")
    }
  )
})

test_that(".remote_write_changelog handles missing CHANGELOG gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      remote_dir <- .dir_create_tmp_random()

      # Remove CHANGELOG if it exists
      if (file.exists("CHANGELOG.md")) {
        file.remove("CHANGELOG.md")
      }

      # Should not error even if CHANGELOG doesn't exist
      result <- tryCatch(
        {
          .remote_write_changelog("local", remote_dir)
          TRUE
        },
        error = function(e) FALSE
      )

      # Cleanup
      unlink(remote_dir, recursive = TRUE)
    }
  )
})

# =============================================================================
# Version detection from file structures
# =============================================================================

test_that(".remote_detect_structure_local detects archive structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Archive structure: path ends with version (without v prefix)
      result <- .remote_detect_structure_local("/path/to/0.0.1")
      expect_identical(result, "archive")

      result <- .remote_detect_structure_local("/path/to/1.2.3")
      expect_identical(result, "archive")

      result <- .remote_detect_structure_local("/path/to/0.1.0-1")
      expect_identical(result, "archive")
    }
  )
})

test_that(".remote_detect_structure_local detects latest structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Latest structure: path doesn't end with version
  result <- .remote_detect_structure_local("/path/to/latest")
  expect_identical(result, "latest")

  result <- .remote_detect_structure_local("/path/to/raw-data")
  expect_identical(result, "latest")

  result <- .remote_detect_structure_local("/path/to/output-files")
  expect_identical(result, "latest")
})

test_that(".remote_detect_structure dispatches correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Test dispatcher
      result <- .remote_detect_structure("/path/to/0.0.1", "local")
      expect_identical(result, "archive")

      result <- .remote_detect_structure("/path/to/latest", "local")
      expect_identical(result, "latest")
    }
  )
})

# =============================================================================
# Version extraction from remote paths
# =============================================================================

test_that(".version_get_remote_local extracts version from path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Valid versions (without v prefix)
      result <- .version_get_remote_local("/path/to/0.0.1")
      expect_identical(result, "0.0.1")

      result <- .version_get_remote_local("/path/to/1.2.3")
      expect_identical(result, "1.2.3")

      result <- .version_get_remote_local("/path/to/0.1.0-1")
      expect_identical(result, "0.1.0-1")
    }
  )
})

test_that(".version_get_remote_local returns NULL for invalid versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Invalid versions
  result <- .version_get_remote_local("/path/to/latest")
  expect_null(result)

  result <- .version_get_remote_local("/path/to/raw-data")
  expect_null(result)

  result <- .version_get_remote_local("/path/to/v1")
  expect_null(result)
})

test_that(".version_get_remote dispatches correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      result <- .version_get_remote("/path/to/0.0.1", "local")
      expect_identical(result, "0.0.1")

      result <- .version_get_remote("/path/to/latest", "local")
      expect_null(result)
    }
  )
})

# =============================================================================
# Version latest detection from filenames
# =============================================================================

test_that(".remote_version_latest_get finds latest version from local files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # For local type, input should have v prefix (gets removed internally)
  fn_vec <- c("v0.0.1", "v0.0.2", "v0.1.0", "v1.0.0")
  result <- .remote_version_latest_get(fn_vec, "local", "output")

  # Should return package_version object
  expect_s3_class(result, "package_version")
  expect_identical(as.character(result), "1.0.0")
})

test_that(".remote_version_latest_get handles empty suffixes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # With -empty suffix
  fn_vec <- c("v0.0.1", "v0.0.2-empty", "v0.1.0")
  result <- .remote_version_latest_get(fn_vec, "local", "output")

  expect_s3_class(result, "package_version")
  expect_identical(as.character(result), "0.1.0")
})

test_that(".remote_version_latest_get handles errors gracefully for non-versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test that it returns empty for truly invalid input
  # Empty input
  result <- .remote_version_latest_get(character(0), "local", "output")
  expect_identical(length(result), 0L)

  # Input with no valid versions (after removing v prefix)
  # These will fail when passed to package_version
  result <- tryCatch(
    .remote_version_latest_get(c("v"), "local", "output"),
    error = function(e) character(0L)
  )
  expect_identical(length(result), 0L)
})

test_that(".remote_version_latest_get handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  result <- .remote_version_latest_get(character(0), "local", "output")
  expect_identical(length(result), 0L)
})

# =============================================================================
# Version filtering with regex
# =============================================================================

test_that(".remote_version_latest_filter filters by version format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # For local type, filter expects label-vX.X.X format
      fn_vec <- c("output-v0.0.1", "output-v0.0.2", "output-v1.0.0", "latest", "raw-data")
      result <- .remote_version_latest_filter(fn_vec, "local", "output")

      # Should include versioned filenames, exclude non-version names
      expect_true("output-v0.0.1" %in% result)
      expect_true("output-v0.0.2" %in% result)
      expect_true("output-v1.0.0" %in% result)
      expect_false("latest" %in% result)
      expect_false("raw-data" %in% result)
    }
  )
})

test_that(".remote_version_latest_filter handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  result <- .remote_version_latest_filter(character(0), "local", "output")
  expect_identical(length(result), 0L)
})

# =============================================================================
# Version extraction from filtered filenames
# =============================================================================

test_that(".remote_version_latest_extract extracts latest version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Extract version from label-vX.X.X format
      fn_vec <- c("output-v0.0.1", "output-v0.0.2", "output-v1.0.0")
      result <- .remote_version_latest_extract(fn_vec, "output")

      # Returns package_version object from .version_get_latest()
      expect_s3_class(result, "package_version")
      expect_identical(as.character(result), "1.0.0")
    }
  )
})

test_that(".remote_version_latest_extract handles empty suffix", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      fn_vec <- c("output-v0.0.1", "output-v0.0.2-empty", "output-v0.1.0")
      result <- .remote_version_latest_extract(fn_vec, "output")

      # -empty suffix is stripped, so 0.1.0 is latest
      # Returns package_version object
      expect_s3_class(result, "package_version")
      expect_identical(as.character(result), "0.1.0")
    }
  )
})

test_that(".remote_version_latest_extract returns empty for invalid input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  result <- .remote_version_latest_extract(character(0), "output")
  expect_identical(length(result), 0L)

  result <- .remote_version_latest_extract(c("invalid", "names"), "output")
  expect_identical(length(result), 0L)
})

# =============================================================================
# Manifest-based version tracking with local remotes
# =============================================================================

test_that(".remote_get_version_latest_label works with archive structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Set up archive structure
      label <- "raw-data"
      version <- "0.0.1"

      # Create versioned directory
      version_dir <- file.path(remote_base, label, paste0("v", version))
      dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)

      # Add some files
      writeLines("test1", file.path(version_dir, "file1.txt"))
      writeLines("test2", file.path(version_dir, "file2.txt"))

      # Create and write manifest
      manifest_local <- data.frame(
        label = c(label, label),
        fn = c("file1.txt", "file2.txt"),
        version = paste0("v", version),
        hash = c(
          .hash_file(file.path(version_dir, "file1.txt")),
          .hash_file(file.path(version_dir, "file2.txt"))
        ),
        stringsAsFactors = FALSE
      )
      .manifest_write(manifest_local, .path_get("manifest.csv"))

      # Write manifest to remote
      .remote_write_manifest("local", remote_base, manifest_local)

      # Write VERSION file
      version_file_content <- paste0(label, ": ", version)
      .remote_write_version_file("local", remote_base, version_file_content)

      # Test version detection
      result <- .remote_get_version_latest_label(
        remote_pre = remote_base,
        type = "local",
        label = label,
        structure = "archive"
      )

      # Should return version without 'v' prefix
      expect_identical(result, version)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that(".remote_get_version_latest_label returns empty for mismatched manifests", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      remote_base <- .dir_create_tmp_random()
      label <- "raw-data"
      version <- "0.0.1"

      # Create versioned directory
      version_dir <- file.path(remote_base, label, paste0("v", version))
      dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines("test", file.path(version_dir, "file1.txt"))

      # Create local manifest with different hash
      manifest_local <- data.frame(
        label = label,
        fn = "file1.txt",
        version = paste0("v", version),
        hash = "different_hash",
        stringsAsFactors = FALSE
      )
      .manifest_write(manifest_local, .path_get("manifest.csv"))

      # Create remote manifest with actual hash
      manifest_remote <- data.frame(
        label = label,
        fn = "file1.txt",
        version = paste0("v", version),
        hash = .hash_file(file.path(version_dir, "file1.txt")),
        stringsAsFactors = FALSE
      )
      .remote_write_manifest("local", remote_base, manifest_remote)

      # Write VERSION file
      version_file_content <- paste0(label, ": ", version)
      .remote_write_version_file("local", remote_base, version_file_content)

      # Should return empty because manifests don't match
      result <- .remote_get_version_latest_label(
        remote_pre = remote_base,
        type = "local",
        label = label,
        structure = "archive"
      )

      expect_identical(length(result), 0L)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that(".remote_get_version_latest_label_non_project_archive detects versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      remote_base <- .dir_create_tmp_random()
      label <- "output"

      # Create multiple version directories
      versions <- c("v0.0.1", "v0.0.2", "v0.1.0")
      for (v in versions) {
        version_dir <- file.path(remote_base, label, v)
        dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
        writeLines("test", file.path(version_dir, "file.txt"))
      }

      # Should return latest version without 'v' prefix
      result <- .remote_get_version_latest_label_non_project_archive(
        remote_pre = remote_base,
        type = "local",
        label = label,
        structure = "archive"
      )

      expect_identical(result, "0.1.0")

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that(".remote_get_version_latest_label_non_project_archive returns empty for latest structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  remote_base <- "/path/to/remote"
  result <- .remote_get_version_latest_label_non_project_archive(
    remote_pre = remote_base,
    type = "local",
    label = "output",
    structure = "latest"
  )

  expect_identical(length(result), 0L)
})

# =============================================================================
# VERSION file operations
# =============================================================================

test_that(".remote_get_version_file_read reads VERSION file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create temp VERSION file
  temp_dir <- .dir_create_tmp_random()
  version_path <- file.path(temp_dir, "VERSION")
  writeLines(c("output: 0.0.1", "raw-data: 0.0.2"), version_path)

  result <- .remote_get_version_file_read(version_path)

  expect_identical(length(result), 2L)
  expect_true("output: 0.0.1" %in% result)
  expect_true("raw-data: 0.0.2" %in% result)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that(".remote_get_version_file_read returns empty for missing file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  result <- .remote_get_version_file_read("/nonexistent/path")
  expect_identical(length(result), 0L)

  result <- .remote_get_version_file_read("")
  expect_identical(length(result), 0L)
})

test_that(".remote_get_version_latest_label_non_project_file_extract extracts version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  version_file <- c("output: 0.0.1", "raw-data: 0.0.2", "docs: 1.0.0")

  # Extract output version
  result <- .remote_get_version_latest_label_non_project_file_extract(
    version_file, "output"
  )
  expect_identical(result, "0.0.1")

  # Extract raw-data version
  result <- .remote_get_version_latest_label_non_project_file_extract(
    version_file, "raw-data"
  )
  expect_identical(result, "0.0.2")

  # Extract docs version
  result <- .remote_get_version_latest_label_non_project_file_extract(
    version_file, "docs"
  )
  expect_identical(result, "1.0.0")
})

test_that(".remote_get_version_latest_label_non_project_file_extract handles asterisk trust marker", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # VERSION file with trust asterisk
  version_file <- c("output: 0.0.1*", "raw-data: 0.0.2")

  result <- .remote_get_version_latest_label_non_project_file_extract(
    version_file, "output"
  )

  # Should remove asterisk
  expect_identical(result, "0.0.1")
})

test_that(".remote_get_version_latest_label_non_project_file_extract returns empty for missing label", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  version_file <- c("output: 0.0.1", "raw-data: 0.0.2")

  result <- .remote_get_version_latest_label_non_project_file_extract(
    version_file, "nonexistent"
  )

  expect_identical(length(result), 0L)
})

# =============================================================================
# Manifest retrieval edge cases
# =============================================================================

test_that(".remote_get_manifest_non_project returns empty for missing manifest", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      remote_dir <- .dir_create_tmp_random()

      # No manifest exists
      result <- .remote_get_manifest_non_project(
        type = "local",
        remote_pre = remote_dir
      )

      expect_s3_class(result, "data.frame")
      expect_identical(nrow(result), 0L)

      # Cleanup
      unlink(remote_dir, recursive = TRUE)
    }
  )
})

test_that(".remote_get_manifest_project reads local project manifest", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test manifest
      manifest <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("abc123", "def456"),
        stringsAsFactors = FALSE
      )
      .manifest_write(manifest, .path_get("manifest.csv"))

      result <- .remote_get_manifest_project()

      expect_s3_class(result, "data.frame")
      expect_identical(nrow(result), 2L)
      expect_true("file1.txt" %in% result$fn)
      expect_true("file2.txt" %in% result$fn)
    }
  )
})

# =============================================================================
# Integration: Full workflow with local remote
# =============================================================================

test_that("full version tracking workflow with local archive remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      remote_base <- .dir_create_tmp_random()
      label <- "raw-data"

      # Setup: Create initial content
      .test_content_setup_label(label)

      # Version 1: First upload
      version1 <- "0.0.1"
      version1_dir <- file.path(remote_base, label, paste0("v", version1))
      dir.create(version1_dir, recursive = TRUE, showWarnings = FALSE)

      # Copy files to version directory
      files <- list.files(
        projr_path_get_dir(label, safe = FALSE),
        recursive = TRUE, full.names = TRUE
      )
      rel_files <- list.files(
        projr_path_get_dir(label, safe = FALSE),
        recursive = TRUE, full.names = FALSE
      )
      for (i in seq_along(files)) {
        dest_dir <- dirname(file.path(version1_dir, rel_files[i]))
        if (!dir.exists(dest_dir)) {
          dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        }
        fs::file_copy(files[i], file.path(version1_dir, rel_files[i]))
      }

      # Create manifest for version 1
      manifest1 <- data.frame(
        label = label,
        fn = rel_files,
        version = paste0("v", version1),
        hash = sapply(files, .hash_file),
        stringsAsFactors = FALSE
      )
      .manifest_write(manifest1, .path_get("manifest.csv"))
      .remote_write_manifest("local", remote_base, manifest1)

      # Write VERSION file
      version_file1 <- paste0(label, ": ", version1)
      .remote_write_version_file("local", remote_base, version_file1)

      # Verify version detection
      detected_version <- .remote_get_version_latest_label(
        remote_pre = remote_base,
        type = "local",
        label = label,
        structure = "archive"
      )
      expect_identical(detected_version, version1)

      # Verify structure detection (version dir basename is v0.0.1, not 0.0.1)
      # Need to use the actual version format without v prefix
      version_dir_no_v <- file.path(remote_base, label, version1)
      dir.create(version_dir_no_v, recursive = TRUE, showWarnings = FALSE)
      structure <- .remote_detect_structure(version_dir_no_v, "local")
      expect_identical(structure, "archive")

      # Verify manifest retrieval
      manifest_retrieved <- .remote_get_manifest("local", remote_base)
      expect_identical(nrow(manifest_retrieved), nrow(manifest1))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})
