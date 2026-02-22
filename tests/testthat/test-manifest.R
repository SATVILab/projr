test_that(".manifest_hash_label works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # test hashing empty directory - returns 1 row with empty fn and hash
      path_dir_empty <- projr_path_get_dir("raw-data")
      .dir_rm(path_dir_empty)
      dir.create(path_dir_empty)
      manifest <- .manifest_hash_label("raw-data", FALSE)
      expect_identical(nrow(manifest), 1L)
      expect_identical(manifest$fn, "")
      expect_identical(manifest$hash, "")
      # test hashing empty directory with a sub-directory - still 1 row
      dir.create(file.path(path_dir_empty, "def"))
      manifest <- .manifest_hash_label("raw-data", TRUE)
      expect_identical(nrow(manifest), 1L)
      expect_identical(manifest$fn, "")
      expect_identical(manifest$hash, "")

      # test hashing non-empty directories
      path_dir <- .test_content_setup_label("output", safe = FALSE)
      manifest <- .manifest_hash_label("output", TRUE)
      expect_identical(nrow(manifest), 4L)
      expect_identical(length(unique(manifest$hash)), 1L)

      # test hashing non-empty directories - non-output run
      path_dir <- .test_content_setup_label("output", safe = TRUE)
      manifest <- .manifest_hash_label("output", FALSE)
      expect_identical(nrow(manifest), 4L)
      expect_identical(length(unique(manifest$hash)), 1L)
    }
  )
})

test_that(".build_manifest_* works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # pre
      # --------------------------

      # no content, except for an ignored label (output)
      label_vec <- c("cache", "raw-data", "output")[-3]
      for (x in label_vec) {
        .dir_rm(projr_path_get_dir(x, safe = TRUE, create = FALSE))
        projr_path_get_dir(x, safe = TRUE, create = TRUE)
      }
      .test_content_setup_label("output", safe = TRUE)
      expect_false(.build_manifest_pre(FALSE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      # Only raw-data is hashed (cache not by default), empty dir = 1 row
      expect_identical(nrow(manifest), 1L)

      # content, but ignore cache by default
      invisible(.test_content_setup_label(label_vec, safe = TRUE))
      expect_false(.build_manifest_pre(FALSE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 4L)

      # content, but now explicitly version cache
      .yml_dir_nm_set_hash(TRUE, "cache", "default")
      invisible(.test_content_setup_label(label_vec, safe = TRUE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 8L)

      # post
      # --------------------------
      expect_false(.build_manifest_post(FALSE))
      path_manifest <- .build_manifest_post(TRUE)
      # Pre (8) + empty docs (1) + output with 1 empty marker (1) = 10
      expect_identical(nrow(.manifest_read(path_manifest)), 10L)

      # now add output content
      .test_content_setup_label("output", safe = FALSE)
      path_manifest <- .build_manifest_post(TRUE)
      # Pre (8 duplicates of previous) + output (4 files) + empty docs (1 duplicate) + prev (10)
      # After dedup: prev (10) + output (4 new files) = 14
      expect_identical(nrow(.manifest_read(path_manifest)), 14L)

      # now add doc content
      .test_content_setup_label("docs", safe = FALSE)
      path_manifest <- .build_manifest_post(TRUE)
      # Pre (8 duplicates) + output (4 duplicates) + docs (4 files) + prev (14)
      # After dedup: prev (14) + docs (4 new files) = 18
      expect_identical(nrow(.manifest_read(path_manifest)), 18L)

      # return table with empty directories
      invisible(.file_rm(.build_manifest_pre_path_get()))
      .dir_rm(projr_path_get_dir("docs", safe = FALSE))
      .dir_rm(projr_path_get_dir("output", safe = FALSE))
      .file_rm(.path_get("manifest.csv"))
      path_manifest <- .build_manifest_post(TRUE)
      # Empty docs (1) + empty output (1) = 2
      expect_identical(nrow(.manifest_read(path_manifest)), 2L)
    }
  )
})

test_that("manifest tracks changes across multiple builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Check initial version
      initial_version <- projr_version_get()

      # Setup: Create initial content in raw-data and output
      .test_content_setup_label("raw-data", safe = FALSE)
      .test_content_setup_label("output", safe = FALSE)

      # First build - with initial version
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      manifest_v1 <- .manifest_read_project()
      expect_true(nrow(manifest_v1) > 0)
      expected_v1 <- paste0("v", initial_version)
      expect_true(all(manifest_v1$version == expected_v1))
      n_files_v1 <- nrow(manifest_v1)

      # Bump version to 0.0.1
      projr_version_set("0.0.1")

      # Second build - same files, new version
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      manifest_v2 <- .manifest_read_project()
      # Should have entries from both versions
      expect_true(nrow(manifest_v2) >= n_files_v1)
      # Should have initial version and v0.0.1 entries
      versions <- unique(manifest_v2$version)
      expect_true(expected_v1 %in% versions)
      expect_true("v0.0.1" %in% versions)

      # Add new file to output
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      file.create(file.path(output_dir, "new_file.txt"))

      # Bump version to 0.0.2
      projr_version_set("0.0.2")

      # Third build - with new file
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      manifest_v3 <- .manifest_read_project()
      # Should have more files now
      expect_true(nrow(manifest_v3) > nrow(manifest_v2))
      # Should have all three versions
      versions <- unique(manifest_v3$version)
      expect_true(all(c(expected_v1, "v0.0.1", "v0.0.2") %in% versions))

      # Check that new_file.txt only appears in v0.0.2
      new_file_entries <- manifest_v3[
        grepl("new_file.txt", manifest_v3$fn),
      ]
      expect_identical(unique(new_file_entries$version), "v0.0.2")
    }
  )
})

test_that(".manifest_filter_label filters by label correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a manifest with multiple labels
      manifest <- data.frame(
        label = c("output", "output", "raw-data", "cache", "docs"),
        fn = c("file1.txt", "file2.txt", "data.csv", "cache.rds", "doc.html"),
        version = c("v0.0.1", "v0.0.1", "v0.0.1", "v0.0.1", "v0.0.1"),
        hash = c("hash1", "hash2", "hash3", "hash4", "hash5"),
        stringsAsFactors = FALSE
      )

      # Test: Filter for output label
      output_only <- .manifest_filter_label(manifest, "output")
      expect_identical(nrow(output_only), 2L)
      expect_true(all(output_only$label == "output"))
      expect_true(all(output_only$fn %in% c("file1.txt", "file2.txt")))

      # Test: Filter for raw-data label
      raw_data_only <- .manifest_filter_label(manifest, "raw-data")
      expect_identical(nrow(raw_data_only), 1L)
      expect_identical(raw_data_only$label, "raw-data")
      expect_identical(raw_data_only$fn, "data.csv")

      # Test: Filter for non-existent label returns empty manifest
      empty <- .manifest_filter_label(manifest, "nonexistent")
      expect_identical(nrow(empty), 0L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(empty)))
    }
  )
})

test_that(".manifest_filter_version filters by version correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a manifest with multiple versions
      manifest <- data.frame(
        label = c("output", "output", "output", "output"),
        fn = c("file1.txt", "file1.txt", "file2.txt", "file2.txt"),
        version = c("v0.0.1", "v0.0.2", "v0.0.1", "v0.0.2"),
        hash = c("hash1", "hash1_v2", "hash2", "hash2_v2"),
        stringsAsFactors = FALSE
      )

      # Test: Filter for v0.0.1 (without 'v' prefix in argument)
      v1_only <- .manifest_filter_version(manifest, "0.0.1")
      expect_identical(nrow(v1_only), 2L)
      expect_true(all(v1_only$version == "v0.0.1"))

      # Test: Filter for v0.0.2
      v2_only <- .manifest_filter_version(manifest, "0.0.2")
      expect_identical(nrow(v2_only), 2L)
      expect_true(all(v2_only$version == "v0.0.2"))

      # Test: Filter for non-existent version returns empty manifest
      empty <- .manifest_filter_version(manifest, "1.0.0")
      expect_identical(nrow(empty), 0L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(empty)))
    }
  )
})

test_that(".manifest_filter_out_version_label filters out version and label", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a manifest with multiple versions and labels
      manifest <- data.frame(
        label = c("output", "output", "raw-data", "output", "raw-data"),
        fn = c("file1.txt", "file2.txt", "data1.csv", "file1.txt", "data1.csv"),
        version = c("v0.0.1", "v0.0.1", "v0.0.1", "v0.0.2", "v0.0.2"),
        hash = c("hash1", "hash2", "hash3", "hash1_v2", "hash3_v2"),
        stringsAsFactors = FALSE
      )

      # Test: Filter out output + v0.0.1 using AND logic
      # Function keeps rows where BOTH label != "output" AND version != "v0.0.1"
      # So it removes: output+v0.0.1 AND also raw-data+v0.0.1 AND also output+v0.0.2
      # It only keeps: raw-data+v0.0.2 (label != output AND version != v0.0.1)
      filtered <- .manifest_filter_out_version_label(manifest, "0.0.1", "output")
      expect_identical(nrow(filtered), 1L)
      # Should only keep raw-data v0.0.2 (both conditions met)
      expect_identical(filtered$label, "raw-data")
      expect_identical(filtered$version, "v0.0.2")

      # Test: Filter out raw-data + v0.0.2
      filtered2 <- .manifest_filter_out_version_label(manifest, "0.0.2", "raw-data")
      expect_identical(nrow(filtered2), 2L)
      # Should keep only output v0.0.1 (both label != raw-data AND version != v0.0.2)
      expect_true(all(filtered2$label == "output" & filtered2$version == "v0.0.1"))

      # Test: Filter out non-existent combination
      # All rows should fail at least one condition, so all are removed
      filtered3 <- .manifest_filter_out_version_label(manifest, "1.0.0", "nonexistent")
      # Since nonexistent label, all rows have label != nonexistent (TRUE)
      # But need version != v1.0.0, which is also TRUE for all
      # So all rows should be kept
      expect_identical(nrow(filtered3), nrow(manifest))
    }
  )
})

test_that(".manifest_append_previous and .manifest_append_previous_impl work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test manifests
      manifest_new <- data.frame(
        label = c("output", "output"),
        fn = c("new1.txt", "new2.txt"),
        version = c("v0.0.2", "v0.0.2"),
        hash = c("hash_new1", "hash_new2"),
        stringsAsFactors = FALSE
      )

      manifest_old <- data.frame(
        label = c("output", "raw-data"),
        fn = c("old1.txt", "data.csv"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("hash_old1", "hash_old2"),
        stringsAsFactors = FALSE
      )

      # Test: append_previous_impl combines manifests
      combined <- .manifest_append_previous_impl(manifest_new, manifest_old)
      expect_identical(nrow(combined), 4L)
      # Old entries should be first
      expect_identical(combined$version[1:2], c("v0.0.1", "v0.0.1"))
      expect_identical(combined$version[3:4], c("v0.0.2", "v0.0.2"))

      # Test: append_previous with append = FALSE returns original
      not_appended <- .manifest_append_previous(manifest_new, FALSE, NULL)
      expect_identical(nrow(not_appended), nrow(manifest_new))
      expect_identical(not_appended, manifest_new)

      # Test: append_previous with file
      path_prev <- file.path(tempdir(), "previous_manifest.csv")
      .manifest_write(manifest_old, path_prev)
      appended <- .manifest_append_previous(manifest_new, TRUE, path_prev)
      expect_identical(nrow(appended), 4L)
      expect_true(all(c("v0.0.1", "v0.0.2") %in% appended$version))

      # Test: append_previous with non-existent file returns original
      no_file <- .manifest_append_previous(manifest_new, TRUE, "/nonexistent/path.csv")
      expect_identical(nrow(no_file), nrow(manifest_new))

      # Clean up
      unlink(path_prev)
    }
  )
})

test_that(".manifest_remove_duplicate removes duplicates correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest with duplicates
      manifest <- data.frame(
        label = c("output", "output", "raw-data", "output"),
        fn = c("file1.txt", "file1.txt", "data.csv", "file1.txt"),
        version = c("v0.0.1", "v0.0.1", "v0.0.1", "v0.0.1"),
        hash = c("hash1", "hash1", "hash2", "hash1"),
        stringsAsFactors = FALSE
      )

      # Test: Remove duplicates
      deduped <- .manifest_remove_duplicate(manifest)
      expect_identical(nrow(deduped), 2L)
      expect_true(any(deduped$fn == "file1.txt"))
      expect_true(any(deduped$fn == "data.csv"))

      # Test: No string column in result
      expect_false("string" %in% names(deduped))

      # Test: Manifest without duplicates stays same size
      manifest_no_dup <- data.frame(
        label = c("output", "raw-data"),
        fn = c("file1.txt", "data.csv"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )
      no_change <- .manifest_remove_duplicate(manifest_no_dup)
      expect_identical(nrow(no_change), 2L)
    }
  )
})

test_that(".manifest_write_impl writes manifest correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      manifest <- data.frame(
        label = c("output", "raw-data"),
        fn = c("file1.txt", "data.csv"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )

      # Test: Write to new file
      path_new <- file.path(tempdir(), "test_manifest_new.csv")
      result <- .manifest_write_impl(manifest, path_new, overwrite = FALSE)
      expect_true(file.exists(path_new))
      expect_identical(result, path_new)

      # Verify content
      read_back <- utils::read.csv(path_new, stringsAsFactors = FALSE)
      expect_identical(nrow(read_back), 2L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(read_back)))

      # Test: Overwrite = FALSE with existing file throws error
      expect_error(
        .manifest_write_impl(manifest, path_new, overwrite = FALSE),
        "already exists"
      )

      # Test: Overwrite = TRUE replaces file
      manifest_new <- rbind(manifest, data.frame(
        label = "cache",
        fn = "cache.rds",
        version = "v0.0.1",
        hash = "hash3",
        stringsAsFactors = FALSE
      ))
      .manifest_write_impl(manifest_new, path_new, overwrite = TRUE)
      read_new <- utils::read.csv(path_new, stringsAsFactors = FALSE)
      expect_identical(nrow(read_new), 3L)

      # Clean up
      unlink(path_new)
    }
  )
})

test_that(".manifest_get_path_file returns correct file path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: With NULL, returns manifest.csv in project root
      path_null <- .manifest_get_path_file(NULL)
      expect_identical(path_null, file.path(.path_get(), "manifest.csv"))

      # Test: With specific path
      test_path <- file.path(tempdir(), "manifest_dir")
      path_specific <- .manifest_get_path_file(test_path)
      expect_identical(path_specific, file.path(test_path, "manifest.csv"))

      # Clean up
      unlink(test_path, recursive = TRUE)
    }
  )
})

test_that(".manifest_hash_cache_filter filters cache projr subdirectory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create hash table with cache files
      hash_tbl <- data.frame(
        fn = c(
          "regular_cache.rds",
          "projr/v0.0.1/data.csv",
          "projr/v0.0.2/output.txt",
          "other_dir/file.txt"
        ),
        version = c("v0.0.1", "v0.0.1", "v0.0.1", "v0.0.1"),
        hash = c("hash1", "hash2", "hash3", "hash4"),
        stringsAsFactors = FALSE
      )

      # Test: For cache label, filters out projr/v* files
      filtered_cache <- .manifest_hash_cache_filter(hash_tbl, "cache")
      expect_identical(nrow(filtered_cache), 2L)
      expect_true(all(filtered_cache$fn %in% c("regular_cache.rds", "other_dir/file.txt")))
      expect_false(any(grepl("^projr/v", filtered_cache$fn)))

      # Test: For non-cache label, returns everything
      filtered_output <- .manifest_hash_cache_filter(hash_tbl, "output")
      expect_identical(nrow(filtered_output), nrow(hash_tbl))
    }
  )
})

test_that(".manifest_version_get_latest returns earliest version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest with multiple versions
      manifest <- data.frame(
        label = c("output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file3.txt"),
        version = c("v0.0.3", "v0.0.1", "v0.0.2"),
        hash = c("hash1", "hash2", "hash3"),
        stringsAsFactors = FALSE
      )

      # Test: Returns earliest (lowest) version as package_version
      # Note: function name says "latest" but implementation uses .version_get_earliest
      latest <- .manifest_version_get_latest(manifest)
      expect_true(inherits(latest, "package_version"))
      expect_identical(as.character(latest), "0.0.1")

      # Test: Single version
      single_manifest <- data.frame(
        label = "output",
        fn = "file.txt",
        version = "v1.0.0",
        hash = "hash1",
        stringsAsFactors = FALSE
      )
      single_result <- .manifest_version_get_latest(single_manifest)
      expect_true(inherits(single_result, "package_version"))
      expect_identical(as.character(single_result), "1.0.0")
    }
  )
})

test_that(".manifest_get_add_project gets version-specific additions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup: Create files and build manifest
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      current_version <- projr_version_get()

      # Test: Get additions for current version and label
      manifest_input <- .zero_tbl_get_manifest()
      additions <- .manifest_get_add_project(manifest_input, "output")

      # Should have files from current version
      expect_true(nrow(additions) > 0)
      expect_true(all(additions$label == "output"))
      expect_identical(unique(additions$version), paste0("v", current_version))

      # Test: Get additions for non-existent label returns empty
      empty_additions <- .manifest_get_add_project(manifest_input, "nonexistent")
      expect_identical(nrow(empty_additions), 1L) # Returns 1 row with empty fn/hash
      expect_identical(empty_additions$fn, "")
    }
  )
})

test_that(".version_file_update_project_version updates project version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      current_version <- projr_version_get()

      # Test: Empty version file gets project version added
      version_file <- character(0)
      updated <- .version_file_update_project_version(version_file)
      expect_identical(length(updated), 1L)
      expect_identical(updated, paste0("Project: ", current_version))

      # Test: Existing version file gets project version added
      version_file <- c("output: 0.0.1", "raw-data: 0.0.2")
      updated <- .version_file_update_project_version(version_file)
      expect_identical(length(updated), 3L)
      expect_identical(updated[1], paste0("Project: ", current_version))
      expect_true(all(c("output: 0.0.1", "raw-data: 0.0.2") %in% updated))

      # Test: Updates existing project version - marks labels stale when version changes
      version_file <- c("Project: 0.0.1", "output: 0.0.2")
      updated <- .version_file_update_project_version(version_file)
      expect_identical(length(updated), 2L)
      expect_identical(updated[1], paste0("Project: ", current_version))
      # Label gets # marker because project version changed from 0.0.1
      expect_identical(updated[2], "output: 0.0.2#")
    }
  )
})

test_that(".version_file_update_label_version updates label version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      current_version <- .version_get()

      # Test: Empty version file gets label added
      version_file <- character(0)
      updated <- .version_file_update_label_version(version_file, "output", FALSE)
      expect_identical(length(updated), 1L)
      expect_identical(updated, paste0("output: ", current_version))

      # Test: Add asterisk when requested
      updated_asterisk <- .version_file_update_label_version(version_file, "output", TRUE)
      expect_identical(updated_asterisk, paste0("output: ", current_version, "*"))

      # Test: Update existing label
      version_file <- c("output: 0.0.1", "raw-data: 0.0.2")
      updated <- .version_file_update_label_version(version_file, "output", FALSE)
      expect_identical(length(updated), 2L)
      expect_identical(updated[1], paste0("output: ", current_version))
      expect_identical(updated[2], "raw-data: 0.0.2")

      # Test: Add new label to existing file
      version_file <- c("output: 0.0.1")
      updated <- .version_file_update_label_version(version_file, "cache", FALSE)
      expect_identical(length(updated), 2L)
      expect_identical(updated[1], "output: 0.0.1")
      expect_identical(updated[2], paste0("cache: ", current_version))

      # Test: Remove duplicate label entries (keeps first)
      version_file <- c("output: 0.0.1", "raw-data: 0.0.2", "output: 0.0.3")
      updated <- .version_file_update_label_version(version_file, "output", FALSE)
      expect_identical(length(updated), 2L)
      expect_identical(updated[1], paste0("output: ", current_version))
      expect_identical(updated[2], "raw-data: 0.0.2")
    }
  )
})

test_that(".version_file_check_update_label determines if update needed", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Function returns: is_change OR !is_label_present
      # So TRUE if: files exist OR label not present

      # Test: Label not present and no files -> TRUE (because !is_label_present)
      version_file <- c("output: 0.0.1")
      fn <- character(0)
      result <- .version_file_check_update_label(fn, version_file, "raw-data")
      expect_true(result) # Label not present means update needed

      # Test: Label not present but files exist -> TRUE
      fn <- c("file1.txt", "file2.txt")
      result <- .version_file_check_update_label(fn, version_file, "raw-data")
      expect_true(result)

      # Test: Label present but files exist -> TRUE
      version_file <- c("output: 0.0.1", "raw-data: 0.0.2")
      result <- .version_file_check_update_label(fn, version_file, "raw-data")
      expect_true(result)

      # Test: Label present and no files -> FALSE
      fn <- character(0)
      result <- .version_file_check_update_label(fn, version_file, "raw-data")
      expect_false(result)
    }
  )
})

test_that(".version_file_check_update_label_present checks label presence", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: Label is present
      version_file <- c("output: 0.0.1", "raw-data: 0.0.2")
      result <- .version_file_check_update_label_present(version_file, "output")
      expect_true(result)

      # Test: Label is not present
      result <- .version_file_check_update_label_present(version_file, "cache")
      expect_false(result)

      # Test: Empty version file
      version_file <- character(0)
      result <- .version_file_check_update_label_present(version_file, "output")
      expect_false(result)

      # Test: Partial match should not count
      version_file <- c("output-extra: 0.0.1")
      result <- .version_file_check_update_label_present(version_file, "output")
      expect_false(result)
    }
  )
})

test_that(".version_file_check_label_trusted checks asterisk marker", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: Label with asterisk is not trusted
      version_file <- c("output: 0.0.1*", "raw-data: 0.0.2")
      result <- .version_file_check_label_trusted(version_file, "output")
      expect_false(result)

      # Test: Label without asterisk is trusted
      result <- .version_file_check_label_trusted(version_file, "raw-data")
      expect_true(result)

      # Test: Label not present returns FALSE
      result <- .version_file_check_label_trusted(version_file, "cache")
      expect_false(result)

      # Test: Multiple entries, only first matters
      version_file <- c("output: 0.0.1", "output: 0.0.2*")
      result <- .version_file_check_label_trusted(version_file, "output")
      expect_true(result)

      # Test: Asterisk at end is detected
      version_file <- c("cache: 1.0.0*")
      result <- .version_file_check_label_trusted(version_file, "cache")
      expect_false(result)
    }
  )
})

test_that(".manifest_get_version_earliest_match finds earliest matching version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup: Create files and build manifest across multiple versions
      .test_content_setup_label("output", safe = FALSE)

      # Version 0.0.1 - initial files
      projr_version_set("0.0.1")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Version 0.0.2 - same files (no changes)
      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Version 0.0.3 - still same files
      projr_version_set("0.0.3")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Should find v0.0.1 as earliest version with these files
      earliest <- .manifest_get_version_earliest_match("output", NULL)
      expect_true(inherits(earliest, "package_version"))
      expect_identical(as.character(earliest), "0.0.1")

      # Now modify a file in v0.0.4
      projr_version_set("0.0.4")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("modified content", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Should now find v0.0.4 as earliest match (files changed)
      earliest_after_change <- .manifest_get_version_earliest_match("output", NULL)
      expect_identical(as.character(earliest_after_change), "0.0.4")

      # Test: With version_comp parameter
      # Find earliest match but only considering versions >= 0.0.2
      projr_version_set("0.0.3")
      earliest_with_comp <- .manifest_get_version_earliest_match("output", "0.0.2")
      # Files haven't changed between 0.0.2 and 0.0.3, but function works backwards
      # from current version, so it returns 0.0.3 (current) when checking files match
      expect_true(as.character(earliest_with_comp) %in% c("0.0.2", "0.0.3"))
    }
  )
})

test_that(".manifest_get_version_earliest_match handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: Empty manifest returns current version as package_version
      current_version <- projr_version_get()
      earliest_empty <- .manifest_get_version_earliest_match("output", NULL)
      # package_version converts "0.0.0-1" to "0.0.0.1" format
      expect_true(inherits(earliest_empty, "package_version"))
      # Just verify it's a valid version, format may differ
      expect_true(length(as.character(earliest_empty)) > 0)

      # Setup: Create files for one version only
      .test_content_setup_label("output", safe = FALSE)
      projr_version_set("0.0.1")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Single version in manifest
      earliest_single <- .manifest_get_version_earliest_match("output", NULL)
      expect_identical(as.character(earliest_single), "0.0.1")

      # Test: version_comp higher than any available version
      earliest_high_comp <- .manifest_get_version_earliest_match("output", "1.0.0")
      # Should return current version (no versions meet criteria)
      expect_identical(as.character(earliest_high_comp), "0.0.1")
    }
  )
})

test_that(".manifest_get_version_earliest_match_get_version_comp generates default", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: With provided version_comp, returns it
      result <- .manifest_get_version_earliest_match_get_version_comp("0.0.5")
      expect_identical(result, "0.0.5")

      # Test: With NULL, generates from version format
      # Default format is major.minor.patch-dev
      result_default <- .manifest_get_version_earliest_match_get_version_comp(NULL)
      # Should generate something like "0.0.1" (all but last component = 0, last = 1)
      expect_true(is.character(result_default))
      expect_true(nchar(result_default) > 0)
      # Should match pattern like "0.0.1"
      expect_true(grepl("^0\\.0\\.1$", result_default))
    }
  )
})

test_that(".build_manifest_pre_get_manifest returns empty when no labels to hash", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup: Disable hashing for all input labels (raw-data and cache)
      # By default, cache is not hashed (FALSE), raw-data is hashed (TRUE)
      # So we need to disable raw-data hashing
      .yml_dir_nm_set_hash(FALSE, "raw-data", "default")

      # Test: Should return empty manifest when no labels are configured to hash
      manifest <- .build_manifest_pre_get_manifest()
      expect_identical(nrow(manifest), 0L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(manifest)))
    }
  )
})

test_that(".build_manifest_post_get_manifest returns empty when no output labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup: Disable hashing for all output labels (output and docs)
      .yml_dir_nm_set_hash(FALSE, "output", "default")
      .yml_dir_nm_set_hash(FALSE, "docs", "default")

      # Test: Should return empty manifest when no output labels are configured to hash
      manifest <- .build_manifest_post_get_manifest(TRUE)
      expect_identical(nrow(manifest), 0L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(manifest)))
    }
  )
})

test_that(".build_manifest_reduce handles empty and single manifests", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: Empty list returns zero-row manifest
      empty_list <- list()
      result_empty <- .build_manifest_reduce(empty_list)
      expect_identical(nrow(result_empty), 0L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(result_empty)))

      # Test: List with one empty manifest returns zero-row manifest
      empty_manifest <- .zero_tbl_get_manifest()
      result_one_empty <- .build_manifest_reduce(list(empty_manifest))
      expect_identical(nrow(result_one_empty), 0L)

      # Test: List with one non-empty manifest returns that manifest
      manifest <- data.frame(
        label = "output",
        fn = "file.txt",
        version = "v0.0.1",
        hash = "hash1",
        stringsAsFactors = FALSE
      )
      result_one <- .build_manifest_reduce(list(manifest))
      expect_identical(nrow(result_one), 1L)
      expect_identical(result_one$label, "output")

      # Test: List with multiple empty manifests returns zero-row manifest
      result_multi_empty <- .build_manifest_reduce(list(empty_manifest, empty_manifest))
      expect_identical(nrow(result_multi_empty), 0L)

      # Test: List with multiple items but only one non-empty (tests line 106)
      manifest2 <- data.frame(
        label = "output",
        fn = "file.txt",
        version = "v0.0.1",
        hash = "hash1",
        stringsAsFactors = FALSE
      )
      result_one_after_filter <- .build_manifest_reduce(list(empty_manifest, manifest2, empty_manifest))
      expect_identical(nrow(result_one_after_filter), 1L)
      expect_identical(result_one_after_filter$label, "output")

      # Test: List with mix of empty and multiple non-empty returns combined
      manifest3 <- data.frame(
        label = "raw-data",
        fn = "data.csv",
        version = "v0.0.1",
        hash = "hash2",
        stringsAsFactors = FALSE
      )
      result_mixed <- .build_manifest_reduce(list(empty_manifest, manifest, manifest3))
      expect_identical(nrow(result_mixed), 2L)
      expect_true(all(result_mixed$label %in% c("output", "raw-data")))
    }
  )
})

test_that(".build_manifest_post returns empty when all parts are empty", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup: Disable all hashing
      .yml_dir_nm_set_hash(FALSE, "raw-data", "default")
      .yml_dir_nm_set_hash(FALSE, "output", "default")
      .yml_dir_nm_set_hash(FALSE, "docs", "default")

      # Remove any existing manifest files
      if (file.exists(.build_manifest_pre_path_get())) {
        .file_rm(.build_manifest_pre_path_get())
      }
      if (file.exists(.path_get("manifest.csv"))) {
        .file_rm(.path_get("manifest.csv"))
      }

      # Test: When all manifest parts are empty, should write empty manifest
      path <- .build_manifest_post(TRUE)
      manifest <- .manifest_read(path)
      expect_identical(nrow(manifest), 0L)
      expect_true(all(c("label", "fn", "version", "hash") %in% names(manifest)))
    }
  )
})

test_that(".build_manifest_post_get_path returns cache path when output_run is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: output_run = TRUE returns project root manifest.csv
      path_output <- .build_manifest_post_get_path(TRUE)
      expect_identical(path_output, .path_get("manifest.csv"))

      # Test: output_run = FALSE returns versioned cache directory path
      path_cache <- .build_manifest_post_get_path(FALSE)
      expect_true(grepl("projr", path_cache))
      expect_true(grepl("manifest.csv", path_cache))
      expect_false(identical(path_cache, path_output))
    }
  )
})

test_that(".build_manifest_pre_get_label_ind_check detects raw labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  # Note: This function appears to be unused legacy code, but testing for completeness

  # Test: Returns TRUE for labels starting with "raw"
  expect_true(.build_manifest_pre_get_label_ind_check("raw-data"))
  expect_true(.build_manifest_pre_get_label_ind_check("raw"))
  expect_true(.build_manifest_pre_get_label_ind_check("raw-anything"))

  # Test: Returns NULL (implicitly) for non-raw labels
  expect_null(.build_manifest_pre_get_label_ind_check("output"))
  expect_null(.build_manifest_pre_get_label_ind_check("cache"))
  expect_null(.build_manifest_pre_get_label_ind_check("docs"))
})
