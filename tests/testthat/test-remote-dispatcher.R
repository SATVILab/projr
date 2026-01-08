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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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

# =============================================================================
# .remote_check_exists
# =============================================================================

test_that(".remote_check_exists works for local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory path (not the directory itself)
      remote_base <- file.path(tempdir(), paste0("test_remote_", floor(runif(1, 1e6, 1e7))))

      # Ensure it doesn't exist
      if (dir.exists(remote_base)) {
        unlink(remote_base, recursive = TRUE)
      }

      # Test non-existent remote
      exists_before <- .remote_check_exists(
        type = "local",
        id = remote_base
      )
      expect_false(exists_before)

      # Create the remote
      .dir_create(remote_base)

      # Now it should exist
      exists_after <- .remote_check_exists(
        type = "local",
        id = remote_base
      )
      expect_true(exists_after)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_final_check_exists_direct
# =============================================================================

test_that(".remote_final_check_exists_direct works for local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Get a remote handle
      remote_path <- file.path(remote_base, "output", "v0.0.1")

      # Test non-existent remote
      exists_before <- .remote_final_check_exists_direct(
        type = "local",
        remote = remote_path
      )
      expect_false(exists_before)

      # Create the remote
      .dir_create(remote_path)

      # Now it should exist
      exists_after <- .remote_final_check_exists_direct(
        type = "local",
        remote = remote_path
      )
      expect_true(exists_after)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_create
# =============================================================================

test_that(".remote_create creates local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory path
      remote_base <- file.path(tempdir(), paste0("test_remote_", floor(runif(1, 1e6, 1e7))))

      # Ensure it doesn't exist
      if (dir.exists(remote_base)) {
        unlink(remote_base, recursive = TRUE)
      }

      # Create the remote
      result <- .remote_create(
        type = "local",
        id = remote_base,
        name = "Test Remote"
      )

      # Verify it was created
      expect_true(dir.exists(remote_base))
      expect_identical(result, remote_base)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_ls_final
# =============================================================================

test_that(".remote_ls_final lists local final remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create some final remotes
      dir1 <- file.path(remote_base, "output")
      dir2 <- file.path(remote_base, "docs")
      dir3 <- file.path(remote_base, "cache")
      .dir_create(dir1)
      .dir_create(dir2)
      .dir_create(dir3)

      # List final remotes
      finals <- .remote_ls_final(
        type = "local",
        remote_pre = remote_base
      )

      # Verify all three directories are listed
      expect_true(is.character(finals))
      expect_true(length(finals) >= 3)
      expect_true("output" %in% basename(finals))
      expect_true("docs" %in% basename(finals))
      expect_true("cache" %in% basename(finals))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_get
# =============================================================================

test_that(".remote_get returns local remote handle", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Get remote handle
      remote <- .remote_get(
        type = "local",
        id = remote_base
      )

      # For local, should return the path
      expect_identical(remote, remote_base)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_final_get
# =============================================================================

test_that(".remote_final_get composes correct local paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Test archive structure
      final1 <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "output",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "0.0.9",
        pre = FALSE,
        empty = FALSE
      )

      expect_true(grepl("output", final1))
      expect_true(grepl("v0\\.0\\.9", final1))
      expect_false(grepl("-empty", final1))

      # Test latest structure
      final2 <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "docs",
        structure = "latest",
        path = NULL,
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )

      expect_true(grepl("docs", final2))
      expect_false(grepl("v[0-9]", final2))

      # Test pre=TRUE (parent)
      final3 <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "cache",
        structure = "archive",
        path = NULL,
        path_append_label = TRUE,
        version = "1.0.0",
        pre = TRUE,
        empty = FALSE
      )

      # Should return parent directory (without version)
      expect_false(grepl("v1\\.0\\.0", final3))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_final_rm_if_empty
# =============================================================================

test_that(".remote_final_rm_if_empty removes empty local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create an empty final remote
      final_path <- file.path(remote_base, "empty_dir")
      .dir_create(final_path)

      # Verify it exists
      expect_true(dir.exists(final_path))

      # Remove it if empty
      .remote_final_rm_if_empty(
        type = "local",
        remote = final_path
      )

      # Should be removed
      expect_false(dir.exists(final_path))

      # Create a non-empty final remote
      final_path2 <- file.path(remote_base, "nonempty_dir")
      .dir_create(final_path2)
      writeLines("content", file.path(final_path2, "file.txt"))

      # Try to remove (should not remove non-empty)
      .remote_final_rm_if_empty(
        type = "local",
        remote = final_path2
      )

      # Should still exist
      expect_true(dir.exists(final_path2))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_rm
# =============================================================================

test_that(".remote_rm removes local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Add some content
      writeLines("test", file.path(remote_base, "file.txt"))
      dir.create(file.path(remote_base, "subdir"))
      writeLines("test2", file.path(remote_base, "subdir", "file2.txt"))

      # Verify it exists
      expect_true(dir.exists(remote_base))

      # Remove it
      .remote_rm(
        type = "local",
        remote = remote_base
      )

      # Should be removed
      expect_false(dir.exists(remote_base))
    }
  )
})

# =============================================================================
# .remote_final_empty
# =============================================================================

test_that(".remote_final_empty removes contents from local remotes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with content
      final_path <- file.path(remote_base, "to_empty")
      .dir_create(final_path)
      writeLines("test", file.path(final_path, "file1.txt"))
      dir.create(file.path(final_path, "subdir"))
      writeLines("test2", file.path(final_path, "subdir", "file2.txt"))

      # Verify content exists
      expect_true(file.exists(file.path(final_path, "file1.txt")))
      expect_true(file.exists(file.path(final_path, "subdir", "file2.txt")))

      # Empty it
      .remote_final_empty(
        type = "local",
        remote = final_path
      )

      # Directory should still exist but be empty
      expect_true(dir.exists(final_path))
      files <- list.files(final_path, recursive = TRUE)
      expect_identical(length(files), 0L)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_file_get_all
# =============================================================================

test_that(".remote_file_get_all downloads all files from local remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with files
      final_path <- file.path(remote_base, "source")
      .dir_create(final_path)
      writeLines("content1", file.path(final_path, "file1.txt"))
      dir.create(file.path(final_path, "subdir"))
      writeLines("content2", file.path(final_path, "subdir", "file2.txt"))

      # Create destination directory
      dest_path <- file.path(remote_base, "dest")

      # Download all files
      result <- .remote_file_get_all(
        type = "local",
        remote = final_path,
        path_dir_save_local = dest_path
      )

      # Verify files were copied
      expect_true(dir.exists(dest_path))
      expect_true(file.exists(file.path(dest_path, "file1.txt")))
      expect_true(file.exists(file.path(dest_path, "subdir", "file2.txt")))

      # Verify content
      expect_identical(readLines(file.path(dest_path, "file1.txt")), "content1")
      expect_identical(readLines(file.path(dest_path, "subdir", "file2.txt")), "content2")

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_file_get
# =============================================================================

test_that(".remote_file_get downloads single file from local remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with files
      final_path <- file.path(remote_base, "source")
      .dir_create(final_path)
      writeLines("content1", file.path(final_path, "file1.txt"))
      dir.create(file.path(final_path, "subdir"))
      writeLines("content2", file.path(final_path, "subdir", "file2.txt"))

      # Create destination directory
      dest_path <- file.path(remote_base, "dest")

      # Download single file
      result <- .remote_file_get(
        type = "local",
        remote = final_path,
        fn = "file1.txt",
        path_dir_save_local = dest_path
      )

      # Verify file was copied
      expect_true(file.exists(file.path(dest_path, "file1.txt")))
      expect_identical(readLines(file.path(dest_path, "file1.txt")), "content1")

      # Other file should not be copied
      expect_false(file.exists(file.path(dest_path, "subdir", "file2.txt")))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_file_ls
# =============================================================================

test_that(".remote_file_ls lists files in local remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with files
      final_path <- file.path(remote_base, "source")
      .dir_create(final_path)
      writeLines("content1", file.path(final_path, "file1.txt"))
      writeLines("content2", file.path(final_path, "file2.txt"))
      dir.create(file.path(final_path, "subdir"))
      writeLines("content3", file.path(final_path, "subdir", "file3.txt"))

      # List files
      files <- .remote_file_ls(
        type = "local",
        remote = final_path
      )

      # Verify all files are listed
      expect_true(is.character(files))
      expect_true(length(files) >= 3)
      expect_true("file1.txt" %in% files)
      expect_true("file2.txt" %in% files)
      expect_true(any(grepl("file3.txt", files)))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_file_rm
# =============================================================================

test_that(".remote_file_rm removes specific files from local remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create a final remote with files
      final_path <- file.path(remote_base, "source")
      .dir_create(final_path)
      writeLines("content1", file.path(final_path, "file1.txt"))
      writeLines("content2", file.path(final_path, "file2.txt"))
      writeLines("content3", file.path(final_path, "file3.txt"))

      # Verify files exist
      expect_true(file.exists(file.path(final_path, "file1.txt")))
      expect_true(file.exists(file.path(final_path, "file2.txt")))
      expect_true(file.exists(file.path(final_path, "file3.txt")))

      # Remove specific files
      .remote_file_rm(
        type = "local",
        fn = c("file1.txt", "file2.txt"),
        remote = final_path
      )

      # Verify files were removed
      expect_false(file.exists(file.path(final_path, "file1.txt")))
      expect_false(file.exists(file.path(final_path, "file2.txt")))

      # file3.txt should still exist
      expect_true(file.exists(file.path(final_path, "file3.txt")))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# .remote_file_add
# =============================================================================

test_that(".remote_file_add uploads files to local remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Create source directory with files
      source_path <- file.path(remote_base, "source")
      .dir_create(source_path)
      writeLines("content1", file.path(source_path, "file1.txt"))
      dir.create(file.path(source_path, "subdir"))
      writeLines("content2", file.path(source_path, "subdir", "file2.txt"))

      # Create empty destination
      dest_path <- file.path(remote_base, "dest")
      .dir_create(dest_path)

      # Add files to remote
      .remote_file_add(
        type = "local",
        remote = dest_path,
        path_dir_local = source_path,
        fn = c("file1.txt", "subdir/file2.txt")
      )

      # Verify files were added
      expect_true(file.exists(file.path(dest_path, "file1.txt")))
      expect_true(file.exists(file.path(dest_path, "subdir", "file2.txt")))

      # Verify content
      expect_identical(readLines(file.path(dest_path, "file1.txt")), "content1")
      expect_identical(readLines(file.path(dest_path, "subdir", "file2.txt")), "content2")

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})
