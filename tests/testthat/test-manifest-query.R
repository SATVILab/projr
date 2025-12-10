test_that("projr_manifest_changes works", {
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
