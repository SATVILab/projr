test_that("projr_hash_dir works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # test hashing empty directory
      path_dir_empty <- file.path(tempdir(), "abc")
      .dir_rm(path_dir_empty)
      dir.create(path_dir_empty)
      hash_tbl <- .hash_dir(path_dir_empty)
      expect_identical(nrow(hash_tbl), 0L)
      # test hashing empty directory with a sub-directory
      dir.create(file.path(path_dir_empty, "def"))
      hash_tbl <- .hash_dir(path_dir_empty)
      expect_identical(nrow(hash_tbl), 0L)

      # test hashing non-empty directories
      path_dir <- .test_setup_content_dir()
      hash_tbl <- .hash_dir(path_dir)
      expect_identical(nrow(hash_tbl), 4L)
      expect_identical(length(unique(hash_tbl$hash)), 1L)
    }
  )
})

test_that(".hash_file works with single file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test file
      test_file <- file.path(tempdir(), "test_hash_file.txt")
      writeLines("test content", test_file)
      
      # Hash the file
      hash_result <- .hash_file(test_file)
      
      # Verify it returns a character vector of length 1
      expect_true(is.character(hash_result))
      expect_identical(length(hash_result), 1L)
      expect_true(nchar(hash_result) > 0)
      
      # Verify same content produces same hash
      hash_result2 <- .hash_file(test_file)
      expect_identical(hash_result, hash_result2)
      
      # Verify different content produces different hash
      writeLines("different content", test_file)
      hash_result3 <- .hash_file(test_file)
      expect_false(identical(hash_result, hash_result3))
      
      # Clean up
      unlink(test_file)
    }
  )
})

test_that(".hash_file works with multiple files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create multiple test files
      temp_dir <- file.path(tempdir(), "hash_test_multi")
      dir.create(temp_dir, showWarnings = FALSE)
      
      file1 <- file.path(temp_dir, "file1.txt")
      file2 <- file.path(temp_dir, "file2.txt")
      file3 <- file.path(temp_dir, "file3.txt")
      
      writeLines("content A", file1)
      writeLines("content B", file2)
      writeLines("content A", file3)  # Same as file1
      
      # Hash all files
      hash_result <- .hash_file(c(file1, file2, file3))
      
      # Verify results
      expect_true(is.character(hash_result))
      expect_identical(length(hash_result), 3L)
      expect_true(all(nchar(hash_result) > 0))
      
      # file1 and file3 should have same hash (same content) - use unname() to ignore path names
      expect_identical(unname(hash_result[1]), unname(hash_result[3]))
      
      # file2 should have different hash
      expect_false(identical(unname(hash_result[1]), unname(hash_result[2])))
      
      # Clean up
      unlink(temp_dir, recursive = TRUE)
    }
  )
})

test_that(".hash_file works with different file types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      temp_dir <- file.path(tempdir(), "hash_test_types")
      dir.create(temp_dir, showWarnings = FALSE)
      
      # Empty file
      empty_file <- file.path(temp_dir, "empty.txt")
      file.create(empty_file)
      hash_empty <- .hash_file(empty_file)
      expect_true(is.character(hash_empty))
      expect_identical(length(hash_empty), 1L)
      
      # Binary-like content
      binary_file <- file.path(temp_dir, "binary.dat")
      writeBin(as.raw(1:100), binary_file)
      hash_binary <- .hash_file(binary_file)
      expect_true(is.character(hash_binary))
      expect_identical(length(hash_binary), 1L)
      
      # Large text file
      large_file <- file.path(temp_dir, "large.txt")
      writeLines(rep("line of text", 1000), large_file)
      hash_large <- .hash_file(large_file)
      expect_true(is.character(hash_large))
      expect_identical(length(hash_large), 1L)
      
      # All should have different hashes
      expect_false(identical(hash_empty, hash_binary))
      expect_false(identical(hash_empty, hash_large))
      expect_false(identical(hash_binary, hash_large))
      
      # Clean up
      unlink(temp_dir, recursive = TRUE)
    }
  )
})

test_that(".hash_dir works with nested directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use test helper to create nested structure
      path_dir <- .test_setup_content_dir(dir_sub_lvl = 2)
      
      # Hash the directory
      hash_tbl <- .hash_dir(path_dir)
      
      # Verify structure
      expect_true(is.data.frame(hash_tbl))
      expect_identical(nrow(hash_tbl), 4L)
      expect_true(all(c("fn", "version", "hash") %in% names(hash_tbl)))
      
      # Verify paths are relative
      expect_true(all(!grepl("^/", hash_tbl$fn)))
      expect_true(all(!grepl(path_dir, hash_tbl$fn, fixed = TRUE)))
      
      # Verify files from different levels are included
      expect_true(any(grepl("/", hash_tbl$fn)))  # Has nested files
      expect_true(any(!grepl("/", hash_tbl$fn)))  # Has top-level files
      
      # Clean up
      unlink(path_dir, recursive = TRUE)
    }
  )
})

test_that(".hash_dir works with dir_exc parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create directory with subdirs
      path_dir <- file.path(tempdir(), "hash_test_exc")
      dir.create(path_dir, showWarnings = FALSE)
      dir.create(file.path(path_dir, "keep"))
      dir.create(file.path(path_dir, "exclude"))
      
      writeLines("keep me", file.path(path_dir, "keep", "file1.txt"))
      writeLines("exclude me", file.path(path_dir, "exclude", "file2.txt"))
      writeLines("top level", file.path(path_dir, "file3.txt"))
      
      # Hash without exclusion
      hash_tbl_all <- .hash_dir(path_dir)
      expect_identical(nrow(hash_tbl_all), 3L)
      
      # Hash with exclusion
      hash_tbl_exc <- .hash_dir(path_dir, dir_exc = "exclude")
      expect_identical(nrow(hash_tbl_exc), 2L)
      expect_false(any(grepl("exclude", hash_tbl_exc$fn)))
      expect_true(any(grepl("keep", hash_tbl_exc$fn)))
      
      # Clean up
      unlink(path_dir, recursive = TRUE)
    }
  )
})

test_that(".hash_dir includes hidden files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create directory with hidden file
      path_dir <- file.path(tempdir(), "hash_test_hidden")
      dir.create(path_dir, showWarnings = FALSE)
      
      writeLines("visible", file.path(path_dir, "visible.txt"))
      writeLines("hidden", file.path(path_dir, ".hidden.txt"))
      
      # Hash the directory
      hash_tbl <- .hash_dir(path_dir)
      
      # Verify hidden file is included
      expect_identical(nrow(hash_tbl), 2L)
      expect_true(any(grepl("^\\.hidden", hash_tbl$fn)))
      
      # Clean up
      unlink(path_dir, recursive = TRUE)
    }
  )
})

test_that(".hash_dir sets correct version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set specific version
      projr_version_set("1.2.3")
      
      # Create test directory
      path_dir <- file.path(tempdir(), "hash_test_version")
      dir.create(path_dir, showWarnings = FALSE)
      writeLines("test", file.path(path_dir, "file.txt"))
      
      # Hash with default version (current)
      hash_tbl_default <- .hash_dir(path_dir)
      expect_identical(unique(hash_tbl_default$version), "v1.2.3")
      
      # Hash with custom version
      hash_tbl_custom <- .hash_dir(path_dir, version = "2.0.0")
      expect_identical(unique(hash_tbl_custom$version), "v2.0.0")
      
      # Clean up
      unlink(path_dir, recursive = TRUE)
    }
  )
})

test_that(".change_get_hash detects added files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create hash tables
      hash_pre <- data.frame(
        fn = character(0),
        version = character(0),
        hash = character(0),
        stringsAsFactors = FALSE
      )
      
      hash_post <- data.frame(
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )
      
      # Compare
      result <- .change_get_hash(hash_pre, hash_post)
      
      # Verify structure
      expect_identical(length(result), 4L)
      expect_true(all(c("fn_dest_extra", "fn_same", "fn_diff", "fn_source_extra") %in% names(result)))
      
      # Verify results
      expect_identical(length(result$fn_dest_extra), 0L)
      expect_identical(length(result$fn_same), 0L)
      expect_identical(length(result$fn_diff), 0L)
      expect_identical(length(result$fn_source_extra), 2L)
      expect_true(all(c("file1.txt", "file2.txt") %in% result$fn_source_extra))
    }
  )
})

test_that(".change_get_hash detects removed files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create hash tables
      hash_pre <- data.frame(
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )
      
      hash_post <- data.frame(
        fn = character(0),
        version = character(0),
        hash = character(0),
        stringsAsFactors = FALSE
      )
      
      # Compare
      result <- .change_get_hash(hash_pre, hash_post)
      
      # Verify results
      expect_identical(length(result$fn_dest_extra), 2L)
      expect_identical(length(result$fn_same), 0L)
      expect_identical(length(result$fn_diff), 0L)
      expect_identical(length(result$fn_source_extra), 0L)
      expect_true(all(c("file1.txt", "file2.txt") %in% result$fn_dest_extra))
    }
  )
})

test_that(".change_get_hash detects unchanged files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create identical hash tables
      hash_pre <- data.frame(
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )
      
      hash_post <- data.frame(
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )
      
      # Compare
      result <- .change_get_hash(hash_pre, hash_post)
      
      # Verify results
      expect_identical(length(result$fn_dest_extra), 0L)
      expect_identical(length(result$fn_same), 2L)
      expect_identical(length(result$fn_diff), 0L)
      expect_identical(length(result$fn_source_extra), 0L)
      expect_true(all(c("file1.txt", "file2.txt") %in% result$fn_same))
    }
  )
})

test_that(".change_get_hash detects modified files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create hash tables with same files but different hashes
      hash_pre <- data.frame(
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1", "hash2"),
        stringsAsFactors = FALSE
      )
      
      hash_post <- data.frame(
        fn = c("file1.txt", "file2.txt"),
        version = c("v1.0.0", "v1.0.0"),
        hash = c("hash1_modified", "hash2_modified"),
        stringsAsFactors = FALSE
      )
      
      # Compare
      result <- .change_get_hash(hash_pre, hash_post)
      
      # Verify results
      expect_identical(length(result$fn_dest_extra), 0L)
      expect_identical(length(result$fn_same), 0L)
      expect_identical(length(result$fn_diff), 2L)
      expect_identical(length(result$fn_source_extra), 0L)
      expect_true(all(c("file1.txt", "file2.txt") %in% result$fn_diff))
    }
  )
})

test_that(".change_get_hash handles mixed changes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create hash tables with various changes
      hash_pre <- data.frame(
        fn = c("unchanged.txt", "modified.txt", "removed.txt"),
        version = c("v1.0.0", "v1.0.0", "v1.0.0"),
        hash = c("hash_u", "hash_m_old", "hash_r"),
        stringsAsFactors = FALSE
      )
      
      hash_post <- data.frame(
        fn = c("unchanged.txt", "modified.txt", "added.txt"),
        version = c("v1.0.0", "v1.0.0", "v1.0.0"),
        hash = c("hash_u", "hash_m_new", "hash_a"),
        stringsAsFactors = FALSE
      )
      
      # Compare
      result <- .change_get_hash(hash_pre, hash_post)
      
      # Verify results
      expect_identical(length(result$fn_dest_extra), 1L)
      expect_identical(result$fn_dest_extra, "removed.txt")
      
      expect_identical(length(result$fn_same), 1L)
      expect_identical(result$fn_same, "unchanged.txt")
      
      expect_identical(length(result$fn_diff), 1L)
      expect_identical(result$fn_diff, "modified.txt")
      
      expect_identical(length(result$fn_source_extra), 1L)
      expect_identical(result$fn_source_extra, "added.txt")
    }
  )
})

test_that(".change_get_dir compares two directories", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create two directories with different content
      dir_pre <- file.path(tempdir(), "hash_test_dir_pre")
      dir_post <- file.path(tempdir(), "hash_test_dir_post")
      
      dir.create(dir_pre, showWarnings = FALSE)
      dir.create(dir_post, showWarnings = FALSE)
      
      # Pre directory: file1, file2
      writeLines("content1", file.path(dir_pre, "file1.txt"))
      writeLines("content2_old", file.path(dir_pre, "file2.txt"))
      
      # Post directory: file1 (unchanged), file2 (modified), file3 (added)
      writeLines("content1", file.path(dir_post, "file1.txt"))
      writeLines("content2_new", file.path(dir_post, "file2.txt"))
      writeLines("content3", file.path(dir_post, "file3.txt"))
      
      # Compare
      result <- .change_get_dir(dir_pre, dir_post)
      
      # Verify structure
      expect_identical(length(result), 4L)
      expect_true(all(c("fn_dest_extra", "fn_same", "fn_diff", "fn_source_extra") %in% names(result)))
      
      # Verify results
      expect_identical(length(result$fn_same), 1L)
      expect_identical(result$fn_same, "file1.txt")
      
      expect_identical(length(result$fn_diff), 1L)
      expect_identical(result$fn_diff, "file2.txt")
      
      expect_identical(length(result$fn_source_extra), 1L)
      expect_identical(result$fn_source_extra, "file3.txt")
      
      expect_identical(length(result$fn_dest_extra), 0L)
      
      # Clean up
      unlink(dir_pre, recursive = TRUE)
      unlink(dir_post, recursive = TRUE)
    }
  )
})

test_that(".zero_tbl_get_hash returns correct structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      zero_tbl <- .zero_tbl_get_hash()
      
      # Verify structure
      expect_true(is.data.frame(zero_tbl))
      expect_identical(nrow(zero_tbl), 0L)
      expect_identical(ncol(zero_tbl), 3L)
      expect_true(all(c("fn", "version", "hash") %in% names(zero_tbl)))
      
      # Verify column types
      expect_true(is.character(zero_tbl$fn))
      expect_true(is.character(zero_tbl$version))
      expect_true(is.character(zero_tbl$hash))
    }
  )
})
