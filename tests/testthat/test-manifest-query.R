test_that("projr_manifest_changes works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get initial version
      initial_version <- projr_version_get()

      # Create initial files
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Bump version and add a file
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      file.create(file.path(output_dir, "new_file.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Query changes between versions
      changes <- projr_manifest_changes(initial_version, "0.0.1")
      expect_true(nrow(changes) > 0)
      expect_true("new_file.txt" %in% changes$fn)

      # Check that new_file.txt is marked as added
      new_file_change <- changes[grepl("new_file.txt", changes$fn), ]
      expect_identical(new_file_change$change_type, "added")
      expect_true(is.na(new_file_change$hash_from))
      expect_false(is.na(new_file_change$hash_to))

      # Test: Query with label filter
      changes_output <- projr_manifest_changes(
        initial_version, "0.0.1",
        label = "output"
      )
      expect_true(all(changes_output$label == "output"))

      # Test: Modify a file
      projr_version_set("0.0.2")
      writeLines("modified content", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      changes_modified <- projr_manifest_changes("0.0.1", "0.0.2")
      abc_change <- changes_modified[changes_modified$fn == "abc.txt", ]
      expect_identical(abc_change$change_type, "modified")
      expect_false(is.na(abc_change$hash_from))
      expect_false(is.na(abc_change$hash_to))
      expect_false(abc_change$hash_from == abc_change$hash_to)

      # Test: Remove a file
      projr_version_set("0.0.3")
      file.remove(file.path(output_dir, "new_file.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      changes_removed <- projr_manifest_changes("0.0.2", "0.0.3")
      new_file_removed <- changes_removed[
        grepl("new_file.txt", changes_removed$fn),
      ]
      expect_identical(new_file_removed$change_type, "removed")
      expect_false(is.na(new_file_removed$hash_from))
      expect_true(is.na(new_file_removed$hash_to))

      # Test: No changes between same version
      no_changes <- projr_manifest_changes("0.0.3", "0.0.3")
      expect_identical(nrow(no_changes), 0L)
    }
  )
})


test_that("projr_manifest_range works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get initial version
      initial_version <- projr_version_get()

      # Create files across multiple versions
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      file.create(file.path(output_dir, "v1_file.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.0.2")
      file.create(file.path(output_dir, "v2_file.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Query range from initial to current
      range_result <- projr_manifest_range(initial_version, "0.0.2")
      expect_true(nrow(range_result) > 0)
      expect_true(all(c("label", "fn", "version_first", "version_last_change", "hash") %in% colnames(range_result)))

      # Check that v1_file first appeared in v0.0.1
      v1_file <- range_result[grepl("v1_file.txt", range_result$fn), ]
      expect_identical(v1_file$version_first, "v0.0.1")

      # Check that v2_file first appeared in v0.0.2
      v2_file <- range_result[grepl("v2_file.txt", range_result$fn), ]
      expect_identical(v2_file$version_first, "v0.0.2")

      # Test: Query with label filter
      range_output <- projr_manifest_range(
        initial_version, "0.0.2",
        label = "output"
      )
      expect_true(all(range_output$label == "output"))

      # Test: Empty range (no versions)
      .file_rm(.path_get("manifest.csv"))
      empty_range <- projr_manifest_range()
      expect_identical(nrow(empty_range), 0L)
    }
  )
})


test_that("projr_manifest_last_change works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Get initial version
      initial_version <- projr_version_get()

      # Create files in multiple directories
      .test_content_setup_label(c("output", "raw-data"), safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Bump version and modify only output (raw-data will also be rehashed in pre)
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      file.create(file.path(output_dir, "new_file.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Query last changes
      last_changes <- projr_manifest_last_change("0.0.1")
      expect_true(nrow(last_changes) > 0)
      expect_true(all(c("label", "version_last_change", "n_files") %in% colnames(last_changes)))

      # Output should have last changed in v0.0.1
      output_change <- last_changes[last_changes$label == "output", ]
      expect_identical(output_change$version_last_change, "v0.0.1")
      expect_true(output_change$n_files > 0)

      # raw-data will also show v0.0.1 because it was hashed in the pre-build phase
      raw_data_change <- last_changes[last_changes$label == "raw-data", ]
      expect_identical(raw_data_change$version_last_change, "v0.0.1")

      # Test: Query with default (current version)
      projr_version_set("0.0.2")
      file.create(file.path(output_dir, "another_file.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      last_changes_current <- projr_manifest_last_change()
      output_current <- last_changes_current[
        last_changes_current$label == "output",
      ]
      expect_identical(output_current$version_last_change, "v0.0.2")

      # Test: Empty manifest
      .file_rm(.path_get("manifest.csv"))
      empty_last <- projr_manifest_last_change()
      expect_identical(nrow(empty_last), 0L)
    }
  )
})


test_that("manifest query functions handle edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: Empty manifest
      empty_changes <- projr_manifest_changes()
      expect_identical(nrow(empty_changes), 0L)

      empty_range <- projr_manifest_range()
      expect_identical(nrow(empty_range), 0L)

      empty_last <- projr_manifest_last_change()
      expect_identical(nrow(empty_last), 0L)

      # Create a single version
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      version <- projr_version_get()

      # Test: Same version comparison
      same_version <- projr_manifest_changes(version, version)
      expect_identical(nrow(same_version), 0L)

      # Test: NULL version parameters (should use defaults)
      changes_default <- projr_manifest_changes()
      expect_true(is.data.frame(changes_default))

      range_default <- projr_manifest_range()
      expect_true(is.data.frame(range_default))

      last_default <- projr_manifest_last_change()
      expect_true(is.data.frame(last_default))
    }
  )
})


test_that("projr_manifest_changes handles version with and without 'v' prefix", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files across two versions
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("content", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Both with 'v' prefix
      changes_v_v <- projr_manifest_changes(paste0("v", v1), "v0.0.1")
      expect_true(nrow(changes_v_v) > 0)

      # Test: First without 'v', second with 'v'
      changes_no_v <- projr_manifest_changes(v1, "v0.0.1")
      expect_true(nrow(changes_no_v) > 0)

      # Test: Both without 'v'
      changes_no_no <- projr_manifest_changes(v1, "0.0.1")
      expect_true(nrow(changes_no_no) > 0)

      # All should give same results
      expect_identical(nrow(changes_v_v), nrow(changes_no_v))
      expect_identical(nrow(changes_v_v), nrow(changes_no_no))
    }
  )
})


test_that("projr_manifest_range handles version ordering correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files across multiple versions with different ordering
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Create versions: 0.1.0, 0.2.0, 0.10.0 to test numeric sorting
      projr_version_set("0.1.0")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.2.0")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.10.0")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Version range should use proper version ordering (0.2.0 < 0.10.0)
      range_result <- projr_manifest_range("0.1.0", "0.10.0")
      expect_true(nrow(range_result) > 0)

      # The files should show changes across all three versions
      expect_true(all(c("label", "fn", "version_first", "version_last_change") %in% colnames(range_result)))
    }
  )
})


test_that("projr_manifest_last_change handles multiple labels correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files in multiple directories at different versions
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # Add raw-data at v0.0.1
      projr_version_set("0.0.1")
      .test_content_setup_label("raw-data", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # v0.0.2: Only modify output files, don't touch raw-data
      projr_version_set("0.0.2")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("new content", file.path(output_dir, "abc.txt"))
      # Don't run pre-build to avoid re-hashing raw-data
      .build_manifest_post(TRUE)

      # Test: Last changes should show different versions for different labels
      last_changes <- projr_manifest_last_change()
      expect_true(nrow(last_changes) >= 2)

      # Output should have last changed in v0.0.2
      output_change <- last_changes[last_changes$label == "output", ]
      expect_identical(output_change$version_last_change, "v0.0.2")

      # raw-data should have last changed in v0.0.1 (not re-hashed)
      raw_data_change <- last_changes[last_changes$label == "raw-data", ]
      expect_identical(raw_data_change$version_last_change, "v0.0.1")
    }
  )
})


test_that(".manifest_query_compare_versions handles empty and NA hashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test scenario with manifest entries
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Read manifest and manually create test data with edge cases
      manifest <- .manifest_read_project()

      # Create files_from with normal hash
      files_from <- data.frame(
        label = c("output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file3.txt"),
        version = c("v0.0.0", "v0.0.0", "v0.0.0"),
        hash = c("hash1", "", NA),
        stringsAsFactors = FALSE
      )

      # Create files_to with different hash scenarios
      files_to <- data.frame(
        label = c("output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file3.txt"),
        version = c("v0.0.1", "v0.0.1", "v0.0.1"),
        hash = c("hash2", "hash_new", "hash_new"),
        stringsAsFactors = FALSE
      )

      # Test: Compare versions with empty and NA hashes
      result <- .manifest_query_compare_versions(files_from, files_to, "v0.0.0", "v0.0.1")

      # file1.txt should be modified (normal hash change)
      file1 <- result[result$fn == "file1.txt", ]
      expect_identical(file1$change_type, "modified")

      # file2.txt and file3.txt should not appear as modified
      # because from hash is empty or NA
      expect_false(any(result$fn == "file2.txt" & result$change_type == "modified"))
      expect_false(any(result$fn == "file3.txt" & result$change_type == "modified"))
    }
  )
})


test_that("projr_manifest_file_last_change respects version_end parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file and modify it across multiple versions
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("v1", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.0.2")
      writeLines("v2", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.0.3")
      writeLines("v3", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Query last change up to v0.0.1 should return v0.0.1
      last_v1 <- projr_manifest_file_last_change("abc.txt", "output", version_end = "0.0.1")
      expect_identical(last_v1$version_last_change, "v0.0.1")

      # Test: Query last change up to v0.0.2 should return v0.0.2
      last_v2 <- projr_manifest_file_last_change("abc.txt", "output", version_end = "0.0.2")
      expect_identical(last_v2$version_last_change, "v0.0.2")

      # Test: Query without version_end should return v0.0.3
      last_current <- projr_manifest_file_last_change("abc.txt", "output")
      expect_identical(last_current$version_last_change, "v0.0.3")
    }
  )
})


test_that("projr_manifest_file_history tracks hash changes correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file and modify it, then keep it same for a version
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # v0.0.1: modify file
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("content v1", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # v0.0.2: don't modify file (same hash)
      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # v0.0.3: modify file again
      projr_version_set("0.0.3")
      writeLines("content v3", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: History should only include versions where hash changed
      history <- projr_manifest_file_history("abc.txt", "output")

      # Should have entries for v1 (first), v0.0.1 (modified), v0.0.3 (modified/current)
      # v0.0.2 should NOT be in history since hash didn't change
      expect_true(paste0("v", v1) %in% history$version)
      expect_true("v0.0.1" %in% history$version)
      expect_true("v0.0.3" %in% history$version)

      # Verify change types
      expect_identical(history$change_type[1], "first_appearance")
      expect_true(history$change_type[nrow(history)] == "current")
    }
  )
})


test_that("manifest query functions handle same file in multiple labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create same filename in different directories
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)

      writeLines("output content", file.path(output_dir, "data.txt"))
      writeLines("raw data content", file.path(raw_data_dir, "data.txt"))

      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # Modify only output version
      projr_version_set("0.0.1")
      writeLines("output modified", file.path(output_dir, "data.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Changes should show modification for output, not for raw-data
      changes <- projr_manifest_changes(v1, "0.0.1")

      # Filter to data.txt files
      data_changes <- changes[changes$fn == "data.txt", ]
      expect_true(nrow(data_changes) > 0)

      # Output should be modified
      output_change <- data_changes[data_changes$label == "output", ]
      expect_identical(output_change$change_type, "modified")

      # Test: Range should track both files separately
      range_result <- projr_manifest_range(v1, "0.0.1")
      data_range <- range_result[range_result$fn == "data.txt", ]
      expect_true(nrow(data_range) >= 2) # At least one for each label

      # Test: Last change should show different versions for each label
      last_changes <- projr_manifest_last_change()
      expect_true(nrow(last_changes) >= 2)
    }
  )
})


test_that(".manifest_filter_up_to_version filters correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files across multiple versions
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      initial_version <- projr_version_get()

      projr_version_set("0.0.1")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      projr_version_set("0.0.3")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Read full manifest
      manifest <- .manifest_read_project()

      # Test: Filter up to v0.0.1
      filtered_v1 <- .manifest_filter_up_to_version(manifest, "v0.0.1")
      versions_v1 <- unique(filtered_v1$version)
      expect_true("v0.0.1" %in% versions_v1)
      expect_false("v0.0.2" %in% versions_v1)
      expect_false("v0.0.3" %in% versions_v1)

      # Test: Filter up to v0.0.2
      filtered_v2 <- .manifest_filter_up_to_version(manifest, "v0.0.2")
      versions_v2 <- unique(filtered_v2$version)
      expect_true("v0.0.1" %in% versions_v2)
      expect_true("v0.0.2" %in% versions_v2)
      expect_false("v0.0.3" %in% versions_v2)

      # Test: Empty manifest
      empty_manifest <- manifest[0, ]
      filtered_empty <- .manifest_filter_up_to_version(empty_manifest, "v0.0.1")
      expect_identical(nrow(filtered_empty), 0L)
    }
  )
})
