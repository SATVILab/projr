test_that("projr_manifest_file_last_change works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create initial files
      initial_version <- projr_version_get()
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Query last change for a file that exists
      last_change <- projr_manifest_file_last_change("abc.txt", label = "output")
      expect_identical(nrow(last_change), 1L)
      expect_identical(last_change$fn, "abc.txt")
      expect_identical(last_change$label, "output")
      expect_identical(last_change$version_last_change, paste0("v", initial_version))

      # Bump version and modify the file
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("modified content", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Last change should now be v0.0.1
      last_change <- projr_manifest_file_last_change("abc.txt", label = "output")
      expect_identical(last_change$version_last_change, "v0.0.1")

      # Bump version without modifying the file
      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Last change should still be v0.0.1
      last_change <- projr_manifest_file_last_change("abc.txt", label = "output")
      expect_identical(last_change$version_last_change, "v0.0.1")

      # Test: File not found returns empty
      not_found <- projr_manifest_file_last_change("nonexistent.txt", label = "output")
      expect_identical(nrow(not_found), 0L)

      # Test: Search without label
      last_change_no_label <- projr_manifest_file_last_change("abc.txt")
      expect_identical(nrow(last_change_no_label), 1L)
      expect_identical(last_change_no_label$fn, "abc.txt")
    }
  )
})


test_that("projr_manifest_file_changed works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create initial files
      initial_version <- projr_version_get()
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Bump version and modify a file
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("modified", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: File was modified
      changed <- projr_manifest_file_changed("abc.txt", "output", initial_version, "0.0.1")
      expect_identical(nrow(changed), 1L)
      expect_identical(changed$changed, TRUE)
      expect_identical(changed$change_type, "modified")
      expect_false(is.na(changed$hash_from))
      expect_false(is.na(changed$hash_to))
      expect_false(changed$hash_from == changed$hash_to)

      # Test: File unchanged between same version
      unchanged <- projr_manifest_file_changed("abc.txt", "output", "0.0.1", "0.0.1")
      expect_identical(unchanged$changed, FALSE)
      expect_identical(unchanged$change_type, "unchanged")

      # Bump version and add a new file
      projr_version_set("0.0.2")
      file.create(file.path(output_dir, "new.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: File was added
      added <- projr_manifest_file_changed("new.txt", "output", "0.0.1", "0.0.2")
      expect_identical(added$changed, TRUE)
      expect_identical(added$change_type, "added")
      expect_true(is.na(added$hash_from))
      expect_false(is.na(added$hash_to))

      # Bump version and remove the file
      projr_version_set("0.0.3")
      file.remove(file.path(output_dir, "new.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: File was removed
      removed <- projr_manifest_file_changed("new.txt", "output", "0.0.2", "0.0.3")
      expect_identical(removed$changed, TRUE)
      expect_identical(removed$change_type, "removed")
      expect_false(is.na(removed$hash_from))
      expect_true(is.na(removed$hash_to))

      # Test: File not found in either version
      not_found <- projr_manifest_file_changed("nonexistent.txt", "output", "0.0.1", "0.0.2")
      expect_identical(nrow(not_found), 0L)
    }
  )
})


test_that("projr_manifest_file_history works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create initial files
      initial_version <- projr_version_get()
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Bump version and modify the file
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("modified v1", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Bump version without modification
      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Bump version and modify again
      projr_version_set("0.0.3")
      writeLines("modified v3", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Get full history
      history <- projr_manifest_file_history("abc.txt", label = "output")
      expect_true(nrow(history) >= 3) # At least first, v0.0.1, and v0.0.3
      expect_true(all(history$fn == "abc.txt"))
      expect_true(all(history$label == "output"))

      # Test: First entry should be first_appearance
      expect_true(history$change_type[1] == "first_appearance")
      expect_identical(history$version[1], paste0("v", initial_version))

      # Test: Should have entries for v0.0.1 and v0.0.3 (when changes occurred)
      expect_true("v0.0.1" %in% history$version)
      expect_true("v0.0.3" %in% history$version)

      # Test: Last entry should be "current"
      expect_true(history$change_type[nrow(history)] == "current")

      # Test: File not found returns empty
      not_found <- projr_manifest_file_history("nonexistent.txt", label = "output")
      expect_identical(nrow(not_found), 0L)
    }
  )
})


test_that("projr_manifest_file_first works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create initial files
      initial_version <- projr_version_get()
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Get first appearance
      first <- projr_manifest_file_first("abc.txt", label = "output")
      expect_identical(nrow(first), 1L)
      expect_identical(first$fn, "abc.txt")
      expect_identical(first$label, "output")
      expect_identical(first$version_first, paste0("v", initial_version))
      expect_false(is.na(first$hash))

      # Bump version and add a new file
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      file.create(file.path(output_dir, "new.txt"))
      writeLines("new content", file.path(output_dir, "new.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: New file's first appearance
      first_new <- projr_manifest_file_first("new.txt", label = "output")
      expect_identical(first_new$version_first, "v0.0.1")

      # Test: Original file still has original first appearance
      first_abc <- projr_manifest_file_first("abc.txt", label = "output")
      expect_identical(first_abc$version_first, paste0("v", initial_version))

      # Test: File not found returns empty
      not_found <- projr_manifest_file_first("nonexistent.txt", label = "output")
      expect_identical(nrow(not_found), 0L)

      # Test: Search without label
      first_no_label <- projr_manifest_file_first("abc.txt")
      expect_identical(nrow(first_no_label), 1L)
      expect_identical(first_no_label$version_first, paste0("v", initial_version))
    }
  )
})


test_that("file query functions handle edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: Empty manifest
      empty_last <- projr_manifest_file_last_change("file.txt", label = "output")
      expect_identical(nrow(empty_last), 0L)

      empty_changed <- projr_manifest_file_changed("file.txt", "output", "0.0.1", "0.0.2")
      expect_identical(nrow(empty_changed), 0L)

      empty_history <- projr_manifest_file_history("file.txt", label = "output")
      expect_identical(nrow(empty_history), 0L)

      empty_first <- projr_manifest_file_first("file.txt", label = "output")
      expect_identical(nrow(empty_first), 0L)

      # Create a single version
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      version <- projr_version_get()

      # Test: File with only one version
      last_single <- projr_manifest_file_last_change("abc.txt", label = "output")
      expect_identical(nrow(last_single), 1L)
      expect_identical(last_single$version_last_change, paste0("v", version))

      history_single <- projr_manifest_file_history("abc.txt", label = "output")
      expect_identical(nrow(history_single), 1L)
      expect_identical(history_single$change_type, "current")

      # Test: NULL version parameters should use defaults
      changed_null <- projr_manifest_file_changed("abc.txt", "output")
      expect_true(is.data.frame(changed_null))

      last_null <- projr_manifest_file_last_change("abc.txt", "output")
      expect_true(is.data.frame(last_null))
    }
  )
})


test_that("file query functions work without label parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files in multiple directories
      .test_content_setup_label(c("output", "raw-data"), safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Create a file with same name in both directories
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)

      writeLines("output data", file.path(output_dir, "data.txt"))
      writeLines("raw data", file.path(raw_data_dir, "data.txt"))

      projr_version_set("0.0.1")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Without label, should find file in both directories
      last_all <- projr_manifest_file_last_change("data.txt")
      expect_true(nrow(last_all) >= 1) # May find in one or both

      first_all <- projr_manifest_file_first("data.txt")
      expect_true(nrow(first_all) >= 1)

      history_all <- projr_manifest_file_history("data.txt")
      expect_true(nrow(history_all) >= 1)

      # Test: With label, should find only in that directory
      last_output <- projr_manifest_file_last_change("data.txt", label = "output")
      expect_identical(nrow(last_output), 1L)
      expect_identical(last_output$label, "output")

      last_raw <- projr_manifest_file_last_change("data.txt", label = "raw-data")
      expect_identical(nrow(last_raw), 1L)
      expect_identical(last_raw$label, "raw-data")
    }
  )
})


test_that("projr_manifest_file_changed handles label parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create same filename in different directories
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)

      writeLines("output v1", file.path(output_dir, "test.txt"))
      writeLines("raw v1", file.path(raw_data_dir, "test.txt"))

      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # Modify only output version
      projr_version_set("0.0.1")
      writeLines("output v2", file.path(output_dir, "test.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: With label specified, should only check that label
      output_changed <- projr_manifest_file_changed("test.txt", "output", v1, "0.0.1")
      expect_identical(nrow(output_changed), 1L)
      expect_identical(output_changed$label, "output")
      expect_identical(output_changed$changed, TRUE)
      expect_identical(output_changed$change_type, "modified")

      # Test: raw-data should be unchanged (but will be re-hashed due to pre-build)
      raw_changed <- projr_manifest_file_changed("test.txt", "raw-data", v1, "0.0.1")
      expect_identical(nrow(raw_changed), 1L)
      expect_identical(raw_changed$label, "raw-data")
      # Hash will be same since file wasn't modified
      expect_identical(raw_changed$changed, FALSE)

      # Test: Without label, should pick first found
      no_label <- projr_manifest_file_changed("test.txt", NULL, v1, "0.0.1")
      expect_identical(nrow(no_label), 1L)
      expect_true(no_label$label %in% c("output", "raw-data"))
    }
  )
})


test_that("projr_manifest_file_history handles single version correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file in single version
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # Test: Single version should have one entry marked as "current"
      history <- projr_manifest_file_history("abc.txt", "output")
      expect_identical(nrow(history), 1L)
      expect_identical(history$change_type, "current")
      expect_identical(history$version, paste0("v", v1))
      expect_false(is.na(history$hash))
    }
  )
})


test_that("projr_manifest_file_first handles multiple labels correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create same filename in different directories at different times
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      raw_data_dir <- projr_path_get_dir("raw-data", safe = FALSE)

      # Create in output first
      writeLines("output", file.path(output_dir, "data.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # Create in raw-data later
      projr_version_set("0.0.1")
      writeLines("raw", file.path(raw_data_dir, "data.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Without label, should find multiple entries
      first_all <- projr_manifest_file_first("data.txt")
      expect_true(nrow(first_all) >= 1)

      # Test: With output label
      first_output <- projr_manifest_file_first("data.txt", "output")
      expect_identical(nrow(first_output), 1L)
      expect_identical(first_output$label, "output")
      expect_identical(first_output$version_first, paste0("v", v1))

      # Test: With raw-data label
      first_raw <- projr_manifest_file_first("data.txt", "raw-data")
      expect_identical(nrow(first_raw), 1L)
      expect_identical(first_raw$label, "raw-data")
      expect_identical(first_raw$version_first, "v0.0.1")
    }
  )
})


test_that("manifest file query functions handle nonexistent files gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create some files but query for nonexistent ones
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      projr_version_set("0.0.1")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: All functions should return 0-row data frames with correct structure
      last_change <- projr_manifest_file_last_change("nonexistent.txt", "output")
      expect_identical(nrow(last_change), 0L)
      expect_true(all(c("label", "fn", "version_last_change", "hash") %in% colnames(last_change)))

      changed <- projr_manifest_file_changed("nonexistent.txt", "output", v1, "0.0.1")
      expect_identical(nrow(changed), 0L)
      expect_true(all(c("label", "fn", "changed", "change_type", "hash_from", "hash_to") %in% colnames(changed)))

      history <- projr_manifest_file_history("nonexistent.txt", "output")
      expect_identical(nrow(history), 0L)
      expect_true(all(c("label", "fn", "version", "hash", "change_type") %in% colnames(history)))

      first <- projr_manifest_file_first("nonexistent.txt", "output")
      expect_identical(nrow(first), 0L)
      expect_true(all(c("label", "fn", "version_first", "hash") %in% colnames(first)))
    }
  )
})


test_that("projr_manifest_file_changed handles file not in both versions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create initial files
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      # Add a new file in v2
      projr_version_set("0.0.1")
      output_dir <- projr_path_get_dir("output", safe = FALSE)
      writeLines("new", file.path(output_dir, "new.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Add another file in v3
      projr_version_set("0.0.2")
      writeLines("newer", file.path(output_dir, "newer.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: File only in version_to (added)
      added <- projr_manifest_file_changed("new.txt", "output", v1, "0.0.1")
      expect_identical(nrow(added), 1L)
      expect_identical(added$changed, TRUE)
      expect_identical(added$change_type, "added")
      expect_true(is.na(added$hash_from))
      expect_false(is.na(added$hash_to))

      # Test: File only in version_from (removed)
      # Remove new.txt and build v0.0.3
      file.remove(file.path(output_dir, "new.txt"))
      projr_version_set("0.0.3")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      removed <- projr_manifest_file_changed("new.txt", "output", "0.0.1", "0.0.3")
      expect_identical(nrow(removed), 1L)
      expect_identical(removed$changed, TRUE)
      expect_identical(removed$change_type, "removed")
      expect_false(is.na(removed$hash_from))
      expect_true(is.na(removed$hash_to))

      # Test: File in neither version (not found)
      not_found <- projr_manifest_file_changed("never_existed.txt", "output", "0.0.2", "0.0.3")
      expect_identical(nrow(not_found), 0L)
    }
  )
})


test_that("projr_manifest_file_history tracks all hash changes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file and modify it multiple times
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      output_dir <- projr_path_get_dir("output", safe = FALSE)

      # Modify in v2
      projr_version_set("0.0.1")
      writeLines("v2", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Don't modify in v3 (same hash)
      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Modify in v4
      projr_version_set("0.0.3")
      writeLines("v4", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Don't modify in v5 (same hash)
      projr_version_set("0.0.4")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: History should include v1, v2, v4 (where hash changed)
      # but NOT v3 or v5 (where hash stayed same)
      history <- projr_manifest_file_history("abc.txt", "output")

      # Should have exactly 3 entries (first, modified, modified)
      expect_identical(nrow(history), 3L)

      # Check versions
      expect_identical(history$version[1], paste0("v", v1))
      expect_identical(history$version[2], "v0.0.1")
      expect_identical(history$version[3], "v0.0.3")

      # Check change types
      # First is "first_appearance", others are "modified"
      # Note: "current" is only used when the last change is the most recent version
      expect_identical(history$change_type[1], "first_appearance")
      expect_identical(history$change_type[2], "modified")
      # Last one is "modified" not "current" because there are later versions (v4, v5)
      # in the manifest that don't appear in history
      expect_identical(history$change_type[3], "modified")

      # Should NOT have v0.0.2 or v0.0.4 since hash didn't change
      expect_false("v0.0.2" %in% history$version)
      expect_false("v0.0.4" %in% history$version)
    }
  )
})


test_that("projr_manifest_file_last_change finds correct last change version", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file and modify it, then keep same for several versions
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)
      v1 <- projr_version_get()

      output_dir <- projr_path_get_dir("output", safe = FALSE)

      # Modify in v2
      projr_version_set("0.0.1")
      writeLines("modified", file.path(output_dir, "abc.txt"))
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Don't modify in v3
      projr_version_set("0.0.2")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Don't modify in v4
      projr_version_set("0.0.3")
      .build_manifest_pre(TRUE)
      .build_manifest_post(TRUE)

      # Test: Last change should be v0.0.1 (not v3 or v4)
      last_change <- projr_manifest_file_last_change("abc.txt", "output")
      expect_identical(nrow(last_change), 1L)
      expect_identical(last_change$version_last_change, "v0.0.1")

      # Test: With version_end, should still find v0.0.1
      last_v2 <- projr_manifest_file_last_change("abc.txt", "output", version_end = "0.0.2")
      expect_identical(last_v2$version_last_change, "v0.0.1")

      # Test: With version_end before the change, should find v1
      last_v1 <- projr_manifest_file_last_change("abc.txt", "output", version_end = v1)
      expect_identical(last_v1$version_last_change, paste0("v", v1))
    }
  )
})
