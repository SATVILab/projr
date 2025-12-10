# Test dispatcher functions in R/remote.R
#
# This file adds LITE-compatible tests for dispatcher functions in R/remote.R
# that were previously not covered in LITE mode tests.
#
# All tests use local backend to avoid external dependencies and run in LITE mode.

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# .remote_final_check_exists
# =============================================================================

test_that(".remote_final_check_exists works for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Test archive structure - final remote doesn't exist yet
      exists_before <- .remote_final_check_exists(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.1",
        empty = FALSE
      )
      expect_false(exists_before)

      # Create the final remote directory
      final_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.1",
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(final_path)
      writeLines("test", file.path(final_path, "test.txt"))

      # Now it should exist
      exists_after <- .remote_final_check_exists(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.1",
        empty = FALSE
      )
      expect_true(exists_after)

      # Test with empty variant
      exists_empty <- .remote_final_check_exists(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.1",
        empty = TRUE
      )
      expect_false(exists_empty)

      # Test latest structure
      latest_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(latest_path)

      exists_latest <- .remote_final_check_exists(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        empty = FALSE
      )
      expect_true(exists_latest)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_final_empty_get
# =============================================================================

test_that(".remote_final_empty_get works for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Test archive structure
      remote_final_empty <- .remote_final_empty_get(
        type = "local",
        id = remote_base,
        label = "output",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.2"
      )

      # Should return path ending with -empty
      expect_true(grepl("-empty$", remote_final_empty))
      expect_true(grepl("output", remote_final_empty))
      expect_true(grepl("v0\\.0\\.2", remote_final_empty))

      # Directory should be created
      expect_true(dir.exists(remote_final_empty))

      # Test latest structure
      remote_final_empty_latest <- .remote_final_empty_get(
        type = "local",
        id = remote_base,
        label = "docs",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL
      )

      # Should return path ending with -empty
      expect_true(grepl("-empty$", remote_final_empty_latest))
      expect_true(grepl("docs", remote_final_empty_latest))
      expect_true(dir.exists(remote_final_empty_latest))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_final_get_if_exists
# =============================================================================

test_that(".remote_final_get_if_exists returns NULL for non-existent remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Test with non-existent remote
      remote_final <- .remote_final_get_if_exists(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.1",
        pre = FALSE,
        empty = NULL
      )

      expect_null(remote_final)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that(".remote_final_get_if_exists returns remote for existing remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create the final remote
      final_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "output",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.3",
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(final_path)
      writeLines("content", file.path(final_path, "file.txt"))

      # Should return the remote
      remote_final <- .remote_final_get_if_exists(
        type = "local",
        id = remote_base,
        label = "output",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.3",
        pre = FALSE,
        empty = NULL
      )

      expect_false(is.null(remote_final))
      expect_identical(remote_final, final_path)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that(".remote_final_get_if_exists prefers non-empty over empty variant", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create only the empty variant
      empty_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = TRUE
      )
      .dir_create(empty_path)

      # With empty=NULL, should fallback to empty variant
      remote_final <- .remote_final_get_if_exists(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = NULL
      )

      expect_false(is.null(remote_final))
      expect_identical(remote_final, empty_path)

      # Now create non-empty variant
      nonempty_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(nonempty_path)

      # Should now prefer non-empty
      remote_final2 <- .remote_final_get_if_exists(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = NULL
      )

      expect_identical(remote_final2, nonempty_path)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_get_path_rel and helpers
# =============================================================================

test_that(".remote_get_path_rel_hierarchy builds correct paths", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Test archive with label
      path1 <- .remote_get_path_rel_hierarchy(
        path = NULL,
        path_append_label = TRUE,
        label = "output",
        structure = "archive",
        version = "0.0.5",
        pre = FALSE,
        empty = FALSE
      )
      expect_identical(path1, file.path("output", "v0.0.5"))

      # Test archive with path prefix
      path2 <- .remote_get_path_rel_hierarchy(
        path = "mydata",
        path_append_label = TRUE,
        label = "raw-data",
        structure = "archive",
        version = "1.0.0",
        pre = FALSE,
        empty = FALSE
      )
      expect_identical(path2, file.path("mydata", "raw-data", "v1.0.0"))

      # Test latest structure
      path3 <- .remote_get_path_rel_hierarchy(
        path = NULL,
        path_append_label = TRUE,
        label = "docs",
        structure = "latest",
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )
      expect_identical(path3, "docs")

      # Test with empty flag
      path4 <- .remote_get_path_rel_hierarchy(
        path = NULL,
        path_append_label = TRUE,
        label = "cache",
        structure = "archive",
        version = "0.1.0",
        pre = FALSE,
        empty = TRUE
      )
      expect_identical(path4, file.path("cache", "v0.1.0-empty"))

      # Test pre=TRUE (parent directory)
      path5 <- .remote_get_path_rel_hierarchy(
        path = NULL,
        path_append_label = TRUE,
        label = "output",
        structure = "archive",
        version = "0.0.1",
        pre = TRUE,
        empty = FALSE
      )
      # Should stop at label level for archive
      expect_identical(path5, character())

      # Test without path_append_label
      path6 <- .remote_get_path_rel_hierarchy(
        path = "custom",
        path_append_label = FALSE,
        label = "output",
        structure = "archive",
        version = "2.0.0",
        pre = FALSE,
        empty = FALSE
      )
      expect_identical(path6, file.path("custom", "v2.0.0"))
    }
  )
})

test_that(".remote_get_path_rel_flat builds correct asset names", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Test archive with label
      name1 <- .remote_get_path_rel_flat(
        path = NULL,
        path_append_label = TRUE,
        label = "output",
        structure = "archive",
        version = "0.0.5",
        empty = FALSE
      )
      expect_identical(name1, "output-v0.0.5")

      # Test with path prefix
      name2 <- .remote_get_path_rel_flat(
        path = "myasset",
        path_append_label = TRUE,
        label = "docs",
        structure = "archive",
        version = "1.2.3",
        empty = FALSE
      )
      expect_identical(name2, "myasset-docs-v1.2.3")

      # Test latest structure
      name3 <- .remote_get_path_rel_flat(
        path = NULL,
        path_append_label = TRUE,
        label = "raw-data",
        structure = "latest",
        version = NULL,
        empty = FALSE
      )
      expect_identical(name3, "raw-data")

      # Test with empty flag
      name4 <- .remote_get_path_rel_flat(
        path = NULL,
        path_append_label = TRUE,
        label = "cache",
        structure = "archive",
        version = "0.1.0",
        empty = TRUE
      )
      expect_identical(name4, "cache-v0.1.0-empty")

      # Test without path_append_label
      name5 <- .remote_get_path_rel_flat(
        path = "custom",
        path_append_label = FALSE,
        label = "output",
        structure = "latest",
        version = NULL,
        empty = FALSE
      )
      expect_identical(name5, "custom")
    }
  )
})

test_that(".remote_get_path_rel dispatches correctly by type", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Test local type (hierarchical)
      path_local <- .remote_get_path_rel(
        path = NULL,
        path_append_label = TRUE,
        label = "output",
        structure = "archive",
        type = "local",
        version = "0.0.1",
        pre = FALSE,
        empty = FALSE
      )
      expect_identical(path_local, file.path("output", "v0.0.1"))

      # Test osf type (hierarchical, same as local)
      path_osf <- .remote_get_path_rel(
        path = NULL,
        path_append_label = TRUE,
        label = "docs",
        structure = "latest",
        type = "osf",
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )
      expect_identical(path_osf, "docs")

      # Test github type (flat) - includes .zip extension
      path_github <- .remote_get_path_rel(
        path = NULL,
        path_append_label = TRUE,
        label = "cache",
        structure = "archive",
        type = "github",
        version = "1.0.0",
        empty = FALSE
      )
      expect_identical(path_github, "cache-v1.0.0.zip")
    }
  )
})

# =============================================================================
# .remote_final_rm
# =============================================================================

test_that(".remote_final_rm deletes local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with content
      final_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "output",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.7",
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(final_path)
      writeLines("test content", file.path(final_path, "file.txt"))

      # Verify it exists
      expect_true(dir.exists(final_path))

      # Remove it
      .remote_final_rm(
        type = "local",
        remote = final_path
      )

      # Verify it's gone
      expect_false(dir.exists(final_path))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_final_get_info
# =============================================================================

test_that(".remote_final_get_info returns NULL for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory
      remote_base <- .dir_create_tmp_random()
      final_path <- file.path(remote_base, "output")
      .dir_create(final_path)

      # Local remotes don't have metadata info
      info <- .remote_final_get_info(
        type = "local",
        remote_final = final_path
      )

      expect_null(info)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_hash
# =============================================================================

test_that(".remote_hash calculates hashes for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with test files
      final_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "output",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.8",
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(final_path)
      writeLines("content1", file.path(final_path, "file1.txt"))
      dir.create(file.path(final_path, "subdir"), showWarnings = FALSE)
      writeLines("content2", file.path(final_path, "subdir", "file2.txt"))

      # Calculate hashes
      hash_tbl <- .remote_hash(
        type = "local",
        remote_final = final_path,
        version = "v0.0.8",
        label = "output"
      )

      # Verify structure
      expect_true(is.data.frame(hash_tbl))
      expect_true("label" %in% names(hash_tbl))
      expect_true("fn" %in% names(hash_tbl))
      expect_true("version" %in% names(hash_tbl))
      expect_true("hash" %in% names(hash_tbl))

      # Verify content
      expect_identical(nrow(hash_tbl), 2L)
      expect_true(all(hash_tbl$label == "output"))
      expect_true(all(hash_tbl$version == "v0.0.8"))
      expect_true("file1.txt" %in% hash_tbl$fn)
      expect_true("subdir/file2.txt" %in% hash_tbl$fn)

      # Hashes should be non-empty
      expect_true(all(nchar(hash_tbl$hash) > 0))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that(".remote_hash returns empty manifest for empty remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create an empty final remote
      final_path <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )
      .dir_create(final_path)

      # Calculate hashes for empty directory
      hash_tbl <- .remote_hash(
        type = "local",
        remote_final = final_path,
        version = "v0.0.1",
        label = "cache"
      )

      # Should return empty manifest
      expect_true(is.data.frame(hash_tbl))
      expect_identical(nrow(hash_tbl), 0L)
      expect_true("label" %in% names(hash_tbl))
      expect_true("fn" %in% names(hash_tbl))
      expect_true("version" %in% names(hash_tbl))
      expect_true("hash" %in% names(hash_tbl))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})
