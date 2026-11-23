test_that("projr_hash_dir works", {
  skip_if(.is_test_cran())
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
      path_dir <- .test_content_setup_dir()
      hash_tbl <- .hash_dir(path_dir)
      expect_identical(nrow(hash_tbl), 4L)
      expect_identical(length(unique(hash_tbl$hash)), 1L)
    }
  )
})

test_that(".hash_file works with single file", {
  skip_if(.is_test_cran())
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
      expect_identical(hash_result, tools::md5sum(test_file))

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
      writeLines("content A", file3) # Same as file1

      # Hash all files
      hash_result <- .hash_file(c(file1, file2, file3))

      # Verify results
      expect_true(is.character(hash_result))
      expect_identical(length(hash_result), 3L)
      expect_identical(hash_result, tools::md5sum(c(file1, file2, file3)))

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
      expect_false(identical(unname(hash_empty), unname(hash_binary)))
      expect_false(identical(unname(hash_empty), unname(hash_large)))
      expect_false(identical(unname(hash_binary), unname(hash_large)))

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
      path_dir <- .test_content_setup_dir(dir_sub_lvl = 2)

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
      expect_true(any(grepl("/", hash_tbl$fn))) # Has nested files
      expect_true(any(!grepl("/", hash_tbl$fn))) # Has top-level files

      # Clean up
      unlink(path_dir, recursive = TRUE)
    }
  )
})

test_that(".hash_dir works with dir_exc parameter", {
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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

  expect_identical(result, list(
    fn_dest_extra = character(0),
    fn_same = character(0),
    fn_diff = character(0),
    fn_source_extra = c("file1.txt", "file2.txt")
  ))
})

test_that(".change_get_hash detects removed files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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

  expect_identical(result, list(
    fn_dest_extra = c("file1.txt", "file2.txt"),
    fn_same = character(0),
    fn_diff = character(0),
    fn_source_extra = character(0)
  ))
})

test_that(".change_get_hash detects unchanged files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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

  expect_identical(result, list(
    fn_dest_extra = character(0),
    fn_same = c("file1.txt", "file2.txt"),
    fn_diff = character(0),
    fn_source_extra = character(0)
  ))
})

test_that(".change_get_hash detects modified files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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

  expect_identical(result, list(
    fn_dest_extra = character(0),
    fn_same = character(0),
    fn_diff = c("file1.txt", "file2.txt"),
    fn_source_extra = character(0)
  ))
})

test_that(".change_get_hash handles mixed changes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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

  expect_identical(result, list(
    fn_dest_extra = "removed.txt",
    fn_same = "unchanged.txt",
    fn_diff = "modified.txt",
    fn_source_extra = "added.txt"
  ))
})

test_that(".change_get_dir compares two directories", {
  skip_if(.is_test_cran())
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
  expect_identical(.zero_tbl_get_hash(), data.frame(
    fn = character(0),
    version = character(0),
    hash = character(0)
  ))
})

test_that("both engines hash named, binary and Unicode paths exactly as earlier builds did", {
  skip_if(.is_test_select())
  root <- withr::local_tempdir()
  files <- file.path(root, c("empty", "abc", paste0(intToUtf8(233), " large.bin")))
  file.create(files[1])
  writeBin(charToRaw("abc"), files[2])
  writeBin(rep(as.raw(0:255), 8193), files[3])
  paths <- stats::setNames(files[c(3, 1, 2, 3)], c("large", "empty", "text", "repeat"))
  expected <- vapply(paths, digest::digest, character(1), serialize = FALSE, file = TRUE)
  expect_identical(unname(expected[2:3]), c("d41d8cd98f00b204e9800998ecf8427e", "900150983cd24fb0d6963f7d28e17f72"))
  for (engine in c("cpp", "legacy")) {
    expect_identical(.hash_file(paths, engine), expected)
    expect_identical(names(.hash_file(files, engine)), files)
    expect_identical(.hash_file(character(), engine), stats::setNames(character(), character()))
    expect_identical(.hash_file(NULL, engine), character())
  }
  expect_error(.hash_file(c(files, paste0(files[2], ".missing")), "cpp"), "Could not hash file:.*[.]missing")
})

test_that("hashing settings apply across profiles and share directory histories", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project,
    {
      withr::local_envvar(c(PROJR_PROFILE = "sample"))
      expect_identical(.yml_settings_hashing(), list(approach = "cpp", cores = 1L))
      root <- .yml_get("default")
      root$settings <- list(hashing = list(approach = "legacy", cores = 2L))
      .yml_set(root)
      expect_identical(.yml_settings_hash_engine(), "legacy")
      .yml_set(list(settings = list(hashing = list(approach = "cpp"))), "sample")
      expect_identical(.yml_settings_hashing(), list(approach = "cpp", cores = 2L))
      raw_data <- projr_path_get_dir("raw-data")
      fs::dir_create(raw_data)
      writeLines("same content", file.path(raw_data, "engine-test.txt"))
      first <- .manifest_hash_label("raw-data", TRUE, "1.0")
      .manifest_store_write(first)

      # A local profile using the earlier spelling still overrides the named profile.
      .yml_set(list(settings = list(`hash-engine` = "legacy", hashing = list(cores = 1L))), "local")
      expect_identical(.yml_settings_hashing(), list(approach = "legacy", cores = 1L))
      second <- .manifest_hash_label("raw-data", TRUE, "1.1")
      .manifest_store_write(second)
      index <- .manifest_store_index(.path_get("_projr", "manifests"), "raw-data")
      expect_identical(index$directory_version, c("v1.0", "v1.0"))
      expect_length(unique(index$snapshot), 1L)
      expect_identical(first$hash, second$hash)

      .yml_set(list(settings = list(hashing = list(approach = "unknown"))), "local")
      expect_error(projr_yml_check(), "settings[.]hashing[.]approach")
      for (invalid in list(0, -1, 1.5, Inf, TRUE, "2", c(1, 2))) {
        .yml_set(list(settings = list(hashing = list(cores = invalid))), "local")
        expect_error(projr_yml_check(), "settings[.]hashing[.]cores")
      }
      .yml_set(list(settings = list(hashing = "cpp")), "local")
      expect_error(projr_yml_check(), "settings[.]hashing must be a YAML mapping")
      .yml_set(list(settings = "legacy"), "local")
      expect_error(projr_yml_check(), "settings must be a YAML mapping")
    },
    force = TRUE
  )
})

test_that("directory hashing keeps hidden files, nested paths and file order", {
  skip_if(.is_test_select())
  root <- withr::local_tempdir()
  fs::dir_create(file.path(root, c("projr", "sub/projr", "sub/excluded")))
  files <- c(".hidden", "sub/projr/keep", "a", "sub/.hidden", "projr/drop", "sub/excluded/drop")
  fs::file_create(file.path(root, files))
  all_files <- list.files(root, recursive = TRUE, all.files = TRUE)
  filtered <- all_files[!grepl("^(projr|sub/excluded)/", all_files)]
  expect_identical(.hash_dir(root, "1.2.3", c("projr", "sub/excluded"))$fn, filtered)
  expect_identical(.hash_dir(root, "1.2.3")$fn, all_files)
  expect_identical(.hash_dir(root, "1.2.3", ".*"), .zero_tbl_get_hash())
  expect_error(.hash_dir(file.path(root, "absent"), "1.2.3"), "must exist")
})

test_that("directory hashing follows readable links and skips broken ones", {
  skip_if(.is_test_select())
  skip_on_os("windows")
  root <- withr::local_tempdir()
  fs::dir_create(file.path(root, "nested"))
  writeLines("payload", file.path(root, "nested/file"))
  file.symlink("nested", file.path(root, "linked-directory"))
  file.symlink("nested/file", file.path(root, "linked-file"))
  file.symlink("absent", file.path(root, "dangling"))
  expected <- c("linked-directory/file", "linked-file", "nested/file")
  expect_identical(.hash_dir(root, "2.0.0")$fn, expected)
  expect_identical(.hash_dir(root, "2.0.0", "nested")$fn, expected[1:2])
})

test_that("parallel file hashing matches serial hashes and recovers from worker errors", {
  skip_if(.is_test_select())
  root <- withr::local_tempdir()
  withr::local_dir(root)
  paths <- c(short = "a", unicode = paste0(intToUtf8(233), ".bin"), repeat_a = "a")
  writeLines("small", paths[1])
  writeBin(rep(as.raw(0:255), 1000), paths[2])
  for (engine in c("cpp", "legacy")) {
    serial <- .hash_file(paths, engine, 1L)
    expect_identical(.hash_file(paths, engine, 2L), serial)
    expect_error(.hash_file(c(paths, "missing-file"), engine, 2L), "missing-file")
    expect_identical(.hash_file(paths[2:1], engine, 4L), serial[2:1])
    expect_identical(.hash_file(character(), engine, 2L), setNames(character(), character()))
  }
})

test_that("hash workers are limited to the number of files", {
  skip_if(.is_test_select())
  testthat::local_mocked_bindings(
    .hash_file_batch = function(fn, ...) rep(as.character(Sys.getpid()), length(fn))
  )
  pids <- .hash_file(c("a", "b"), "cpp", 4L)
  expect_length(unique(pids), 2L)
  expect_false(as.character(Sys.getpid()) %in% pids)
  expect_identical(unname(.hash_file("a", "cpp", 4L)), as.character(Sys.getpid()))
})
