test_that(".change_get_manifest works", {
  skip_if(.is_test_cran())
  skip("Test needs rework - .change_get_manifest not designed for project vs project comparison")
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_post(TRUE) |> invisible()

      # only one version
      # ----------------
      change_list <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "output"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 3L)
      change_list_raw_data <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "raw-data"
      )
      expect_identical(length(change_list_raw_data), 4L)
      expect_identical(length(change_list_raw_data[["fn_same"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_diff"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_source_extra"]]), 0L)

      # no change, two versions
      # -----------------------
      .version_bump_major() |> invisible()
      .build_manifest_post(TRUE) |> invisible()
      change_list <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "output"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 3L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
      change_list_raw_data <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "raw-data"
      )
      expect_identical(length(change_list_raw_data), 4L)
      expect_identical(length(change_list_raw_data[["fn_same"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_diff"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_source_extra"]]), 0L)

      # added category, three versions
      # ------------------------------
      .version_bump_patch() |> invisible()
      .test_content_setup_label("raw-data")
      .build_manifest_pre(TRUE) |> invisible()
      .build_manifest_post(TRUE) |> invisible()
      expect_true(
        all(c("raw-data", "output") %in% .manifest_read(
          .build_manifest_post_get_path(TRUE)
        )[["label"]])
      )
      change_list <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "output"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 3L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
      change_list_raw_data <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "raw-data"
      )
      expect_identical(length(change_list_raw_data), 4L)
      expect_identical(length(change_list_raw_data[["fn_same"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_diff"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_source_extra"]]), 3L)
    }
  )
})

test_that(".change_get_file works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      path_pre <- .test_content_setup_dir()
      path_post <- .test_dir_create_random()
      change_list <- .change_get_file(
        type_pre = "local",
        remote_pre = path_pre,
        type_post = "local",
        remote_post = path_post
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 4L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
    }
  )
})

test_that(".change_get works for files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      # nothing before or after
      change_list <- .change_get(
        label = "output",
        output_run = FALSE,
        inspect = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)

      # add something
      .version_bump_minor()
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_post(TRUE) |> invisible()
      change_list <- .change_get(
        label = "output",
        output_run = TRUE,
        inspect = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )

      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 4L)
      change_list <- .change_get(
        label = "output",
        output_run = TRUE,
        inspect = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )

      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 4L)

      # remove and change
      .version_bump_major()
      .dir_copy(
        path_dir_from = "_output", path_dir_to = "_output2"
      )
      cat("abc", file = "_output/abc.txt")
      invisible(file.remove("_output/subdir1/def.txt"))
      change_list <- .change_get(
        label = "output",
        output_run = TRUE,
        inspect = "file",
        type = "local",
        remote = "_output2"
      )

      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 2L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 1L)
      expect_identical(length(change_list[["fn_diff"]]), 1L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
    }
  )
})

test_that(".change_get_check validates inputs correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Valid inputs should not error
      expect_silent(.change_get_check(
        label = "output",
        path_dir_local = NULL,
        inspect = "file"
      ))

      # Valid inputs with path_dir_local set
      expect_silent(.change_get_check(
        label = NULL,
        path_dir_local = "/some/path",
        inspect = "manifest"
      ))

      # Missing inspect should error
      expect_error(.change_get_check(
        label = "output",
        path_dir_local = NULL,
        inspect = NULL
      ))

      # Non-string inspect should error
      expect_error(.change_get_check(
        label = "output",
        path_dir_local = NULL,
        inspect = c("file", "manifest")
      ))

      # Missing label when path_dir_local is NULL should error
      expect_error(.change_get_check(
        label = NULL,
        path_dir_local = NULL,
        inspect = "file"
      ))
    }
  )
})

test_that(".change_get_file_dir_local returns remote path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      test_path <- "/tmp/test_path"
      result <- .change_get_file_dir_local(test_path)
      expect_identical(result, test_path)
    }
  )
})

test_that(".change_get_file_dir dispatches correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test local type
      test_path <- "/tmp/test_path"
      result <- .change_get_file_dir("local", test_path)
      expect_identical(result, test_path)
    }
  )
})

test_that(".zero_list_manifest_get returns correct structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .zero_list_manifest_get()

      # Verify it's a list
      expect_true(is.list(result))
      expect_identical(length(result), 4L)

      # Verify names
      expect_true(all(c("fn_dest_extra", "fn_same", "fn_diff", "fn_source_extra") %in% names(result)))

      # Verify all elements are empty character vectors
      expect_identical(length(result$fn_dest_extra), 0L)
      expect_identical(length(result$fn_same), 0L)
      expect_identical(length(result$fn_diff), 0L)
      expect_identical(length(result$fn_source_extra), 0L)

      expect_true(is.character(result$fn_dest_extra))
      expect_true(is.character(result$fn_same))
      expect_true(is.character(result$fn_diff))
      expect_true(is.character(result$fn_source_extra))
    }
  )
})

test_that(".change_get_manifest_check_nothing detects empty manifests", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Both empty
      manifest_empty <- .zero_tbl_get_manifest()
      result <- .change_get_manifest_check_nothing(manifest_empty, manifest_empty)
      expect_true(result)

      # One empty, one not
      manifest_not_empty <- data.frame(
        label = "output",
        fn = "file.txt",
        version = "v1.0.0",
        hash = "abc123",
        stringsAsFactors = FALSE
      )
      result <- .change_get_manifest_check_nothing(manifest_empty, manifest_not_empty)
      expect_false(result)

      result <- .change_get_manifest_check_nothing(manifest_not_empty, manifest_empty)
      expect_false(result)

      # Both not empty
      result <- .change_get_manifest_check_nothing(manifest_not_empty, manifest_not_empty)
      expect_false(result)
    }
  )
})

test_that(".change_get_manifest_get_closest_mismatch_get_manifest_curr works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test manifests
      manifest_pre <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )

      manifest_post <- data.frame(
        label = c("output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file3.txt"),
        version = c("v2.0.0", "v2.0.0", "v2.0.0"),
        hash = c("hash1", "hash2", "hash3"),
        stringsAsFactors = FALSE
      )

      # Test version found in pre manifest
      result <- .change_get_manifest_get_closest_mismatch_get_manifest_curr(
        manifest_pre, manifest_post, "v1.0.0"
      )
      expect_identical(nrow(result), 2L)
      expect_true(all(result$version == "v1.0.0"))

      # Test version found in post manifest
      result <- .change_get_manifest_get_closest_mismatch_get_manifest_curr(
        manifest_pre, manifest_post, "v2.0.0"
      )
      expect_identical(nrow(result), 3L)
      expect_true(all(result$version == "v2.0.0"))

      # Test version not found in either
      result <- .change_get_manifest_get_closest_mismatch_get_manifest_curr(
        manifest_pre, manifest_post, "v3.0.0"
      )
      expect_identical(nrow(result), 0L)
    }
  )
})

test_that(".change_get_manifest_get_closest_mismatch finds correct version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test manifests with multiple versions
      manifest_pre_full <- data.frame(
        label = c("output", "output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0", "v2.0.0", "v2.0.0"),
        hash = c("hash1", "hash2", "hash1_v2", "hash2_v2"),
        stringsAsFactors = FALSE
      )

      manifest_post_full <- data.frame(
        label = c("output", "output", "output", "output", "output", "output"),
        fn = c("file1.txt", "file2.txt", "file1.txt", "file2.txt", "file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0", "v2.0.0", "v2.0.0", "v3.0.0", "v3.0.0"),
        hash = c("hash1", "hash2", "hash1_v2", "hash2_v2", "hash1_v3", "hash2_v3"),
        stringsAsFactors = FALSE
      )

      manifest_post <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v3.0.0", "v3.0.0"),
        hash = c("hash1_v3", "hash2_v3"),
        stringsAsFactors = FALSE
      )

      # Test finding closest mismatch
      result <- .change_get_manifest_get_closest_mismatch(
        version_pre = "1.0.0",
        version_post = "3.0.0",
        manifest_post = manifest_post,
        manifest_pre_full = manifest_pre_full,
        manifest_post_full = manifest_post_full
      )

      # Should return a version string
      expect_true(is.character(result))
      expect_identical(length(result), 1L)
    }
  )
})

test_that(".change_get_manifest_pre_final handles empty version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test directory with files
      test_dir <- file.path(tempdir(), "test_manifest_pre_final")
      dir.create(test_dir, showWarnings = FALSE)
      writeLines("content", file.path(test_dir, "file1.txt"))

      # Test with empty version (should hash the directory directly)
      result <- .change_get_manifest_pre_final(
        version_pre_actual = character(0),
        version_post_actual = "1.0.0",
        manifest_post = .zero_tbl_get_manifest(),
        manifest_pre_full = .zero_tbl_get_manifest(),
        manifest_post_full = .zero_tbl_get_manifest(),
        type_pre = "local",
        remote_pre = test_dir
      )

      # Should return a hash table
      expect_true(is.data.frame(result))
      expect_true("fn" %in% names(result))
      expect_true("hash" %in% names(result))
      expect_identical(nrow(result), 1L)

      # Clean up
      unlink(test_dir, recursive = TRUE)
    }
  )
})

test_that(".change_get_manifest_pre_final handles non-empty version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test manifests
      manifest_pre_full <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )

      manifest_post_full <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v2.0.0", "v2.0.0"),
        hash = c("hash1_new", "hash2_new"),
        stringsAsFactors = FALSE
      )

      manifest_post <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v2.0.0", "v2.0.0"),
        hash = c("hash1_new", "hash2_new"),
        stringsAsFactors = FALSE
      )

      # Test with non-empty version
      result <- .change_get_manifest_pre_final(
        version_pre_actual = "1.0.0",
        version_post_actual = "2.0.0",
        manifest_post = manifest_post,
        manifest_pre_full = manifest_pre_full,
        manifest_post_full = manifest_post_full,
        type_pre = "local",
        remote_pre = "/some/path"
      )

      # Should return filtered manifest
      expect_true(is.data.frame(result))
      expect_true(all(c("label", "fn", "version", "hash") %in% names(result)))
    }
  )
})

test_that(".change_get_hash handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with both empty hash tables
      hash_empty <- .zero_tbl_get_hash()
      result <- .change_get_hash(hash_empty, hash_empty)

      expect_identical(length(result), 4L)
      expect_identical(length(result$fn_dest_extra), 0L)
      expect_identical(length(result$fn_same), 0L)
      expect_identical(length(result$fn_diff), 0L)
      expect_identical(length(result$fn_source_extra), 0L)

      # Test with only pre having files
      hash_pre <- data.frame(
        fn = c("file1.txt"),
        version = c("v1.0.0"),
        hash = c("hash1"),
        stringsAsFactors = FALSE
      )

      result <- .change_get_hash(hash_pre, hash_empty)
      expect_identical(length(result$fn_dest_extra), 1L)
      expect_identical(result$fn_dest_extra, "file1.txt")

      # Test with only post having files
      hash_post <- data.frame(
        fn = c("file2.txt"),
        version = c("v1.0.0"),
        hash = c("hash2"),
        stringsAsFactors = FALSE
      )

      result <- .change_get_hash(hash_empty, hash_post)
      expect_identical(length(result$fn_source_extra), 1L)
      expect_identical(result$fn_source_extra, "file2.txt")
    }
  )
})

test_that(".change_get with unrecognized inspect errors", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      .test_content_setup_label("output", safe = FALSE)

      # Test with invalid inspect value
      expect_error(
        .change_get(
          label = "output",
          output_run = TRUE,
          inspect = "invalid_inspect",
          type = "local",
          remote = .dir_create_tmp_random()
        ),
        "not recognized"
      )
    }
  )
})

test_that(".change_get_dir handles empty directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create two empty directories
      dir_pre <- file.path(tempdir(), "change_test_empty_pre")
      dir_post <- file.path(tempdir(), "change_test_empty_post")

      dir.create(dir_pre, showWarnings = FALSE)
      dir.create(dir_post, showWarnings = FALSE)

      # Compare empty directories
      result <- .change_get_dir(dir_pre, dir_post)

      expect_identical(length(result), 4L)
      expect_identical(length(result$fn_dest_extra), 0L)
      expect_identical(length(result$fn_same), 0L)
      expect_identical(length(result$fn_diff), 0L)
      expect_identical(length(result$fn_source_extra), 0L)

      # Clean up
      unlink(dir_pre, recursive = TRUE)
      unlink(dir_post, recursive = TRUE)
    }
  )
})

test_that(".change_get_check handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Empty string label should error when path_dir_local is NULL
      expect_error(.change_get_check(
        label = "",
        path_dir_local = NULL,
        inspect = "file"
      ))

      # Numeric inspect should error
      expect_error(.change_get_check(
        label = "output",
        path_dir_local = NULL,
        inspect = 123
      ))

      # Vector label should error when path_dir_local is NULL
      expect_error(.change_get_check(
        label = c("output", "cache"),
        path_dir_local = NULL,
        inspect = "file"
      ))
    }
  )
})
