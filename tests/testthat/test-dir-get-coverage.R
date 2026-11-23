# Tests for improving coverage of R/dir-get.R
# These tests cover functions that were previously untested or under-tested

# ==============================================================================
# Core directory getting functions
# ==============================================================================

test_that(".dir_get works for other labels", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Test various labels with safe=TRUE
      path_output_safe <- .dir_get("output", safe = TRUE)
      expect_true(grepl("projr", path_output_safe))

      # Test with safe=FALSE
      path_output_unsafe <- .dir_get("output", safe = FALSE)
      expect_true(is.character(path_output_unsafe))
      expect_false(identical(path_output_safe, path_output_unsafe))

      # Test with subdirectories
      path_with_sub <- .dir_get("cache", "subdir1", "subdir2", safe = TRUE)
      expect_true(grepl("subdir1", path_with_sub))
      expect_true(grepl("subdir2", path_with_sub))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_tmp_random generates unique paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Generate multiple paths
  path1 <- .dir_get_tmp_random()
  path2 <- .dir_get_tmp_random()

  expect_true(is.character(path1))
  expect_true(is.character(path2))
  expect_false(identical(path1, path2))

  # Should be in tempdir
  expect_true(grepl(tempdir(), path1, fixed = TRUE))
  expect_true(grepl("projr", path1))

  # Test with arguments
  path_with_args <- .dir_get_tmp_random("subdir", "file.txt")
  expect_true(grepl("subdir", path_with_args))
  expect_true(grepl("file.txt", path_with_args))
})

# ==============================================================================
# Label handling functions
# ==============================================================================

test_that(".dir_get_label routes to safe and unsafe correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Safe should go to cache build dir
      path_safe <- .dir_get_label("output", safe = TRUE)
      expect_true(grepl("projr", path_safe))

      # Unsafe should go to direct path
      path_unsafe <- .dir_get_label("output", safe = FALSE)
      expect_false(grepl("projr", path_unsafe))

      # They should be different
      expect_false(identical(path_safe, path_unsafe))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_label_check_not_root allows special labels", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # code, project, and docs labels should be allowed as root
      expect_identical(.dir_get_label_check_not_root(".", "code"), ".")
      expect_identical(.dir_get_label_check_not_root(".", "project"), ".")
      expect_identical(.dir_get_label_check_not_root(".", "docs"), ".")

      # Non-root paths should pass through
      expect_identical(
        .dir_get_label_check_not_root("subdir", "output"),
        "subdir"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_label_check_not_root errors for root with non-special labels", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Get absolute path of project root
      path_proj <- fs::path_abs(.path_get()) |> as.character()

      # Should error for non-special labels when path equals project root
      expect_error(
        .dir_get_label_check_not_root(path_proj, "output"),
        "root directory of the project"
      )
      expect_error(
        .dir_get_label_check_not_root(path_proj, "cache"),
        "root directory of the project"
      )
      expect_error(
        .dir_get_label_check_not_root(path_proj, "raw-data"),
        "root directory of the project"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Safe/unsafe directory functions
# ==============================================================================

test_that(".dir_get_label_safe_check_unsafe identifies safe labels", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Labels that should use safe paths
  expect_true(.dir_get_label_safe_check_unsafe("output"))
  expect_true(.dir_get_label_safe_check_unsafe("docs"))
  expect_true(.dir_get_label_safe_check_unsafe("data"))

  # Labels that should use unsafe paths
  expect_false(.dir_get_label_safe_check_unsafe("raw-data"))
  expect_false(.dir_get_label_safe_check_unsafe("cache"))
  expect_false(.dir_get_label_safe_check_unsafe("project"))
  expect_false(.dir_get_label_safe_check_unsafe("code"))
})

test_that(".dir_get_label_safe works correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # For labels that should be safe
      path_output <- .dir_get_label_safe("output")
      expect_true(grepl("projr", path_output))
      expect_true(grepl("v0.0.0-1", path_output))

      # For labels that should fall back to unsafe
      path_cache <- .dir_get_label_safe("cache")
      expect_true(is.character(path_cache))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_label_safe_path constructs correct paths", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Should include cache auto version path
      path_output <- .dir_get_label_safe_path("output")
      expect_true(grepl("projr", path_output))
      expect_true(grepl("output", path_output))

      # Docs should use unsafe path inside safe structure
      path_docs <- .dir_get_label_safe_path("docs")
      expect_true(grepl("projr", path_docs))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_set_docs_safe works for docs label", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Create a quarto project
      writeLines("project:\n  type: website", "_quarto.yml")

      # Should set docs path for quarto project
      result <- .dir_set_docs_safe("_site", "docs")
      expect_true(result)

      # Non-docs labels should return FALSE
      result_non_docs <- .dir_set_docs_safe("_output", "output")
      expect_false(result_non_docs)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_set_docs_safe_check identifies docs label", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Docs label should return TRUE
  expect_true(.dir_set_docs_safe_check("docs"))

  # Other labels should return FALSE
  expect_false(.dir_set_docs_safe_check("output"))
  expect_false(.dir_set_docs_safe_check("cache"))
  expect_false(.dir_set_docs_safe_check("raw-data"))
})

# ==============================================================================
# Quarto directory functions
# ==============================================================================

test_that(".dir_set_docs_quarto_project sets paths correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Create quarto project
      writeLines("project:\n  type: website", "_quarto.yml")

      # Should set output dir in both _quarto.yml and _projr.yml
      result <- .dir_set_docs_quarto_project("custom_site")
      expect_true(result)

      # Verify it was set in _quarto.yml
      quarto_yml <- yaml::read_yaml("_quarto.yml")
      expect_identical(quarto_yml$project$`output-dir`, "custom_site")

      # Verify it was set in _projr.yml
      projr_yml <- .yml_get(NULL)
      expect_identical(projr_yml$directories$docs$path, "docs")
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Bookdown directory functions
# ==============================================================================

test_that(".dir_set_docs_bookdown sets paths correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Create bookdown project
      writeLines("book_filename: 'test'", "_bookdown.yml")

      # Should set output dir in both _bookdown.yml and _projr.yml
      .dir_set_docs_bookdown("custom_book")

      # Verify it was set in _bookdown.yml
      bd_yml <- yaml::read_yaml("_bookdown.yml")
      expect_identical(bd_yml$output_dir, "custom_book")

      # Verify it was set in _projr.yml
      projr_yml <- .yml_get(NULL)
      expect_identical(projr_yml$directories$docs$path, "docs")
    },
    force = TRUE,
    quiet = TRUE
  )
})

# ==============================================================================
# Markdown directory functions
# ==============================================================================

test_that(".path_get_cache_auto_dir works correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Without create, should return path (but parent may be created by .init)
      path_no_create <- .path_get_cache_auto_dir(create = FALSE, profile = NULL)
      expect_true(is.character(path_no_create))

      # With create, should create directory
      path_create <- .path_get_cache_auto_dir(
        "newsubdir",
        create = TRUE,
        profile = NULL
      )
      expect_true(dir.exists(path_create))

      # With subdirectories
      path_sub <- .path_get_cache_auto_dir(
        "sub1",
        "sub2",
        create = FALSE,
        profile = NULL
      )
      expect_true(grepl("sub1", path_sub))
      expect_true(grepl("sub2", path_sub))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_get_cache_auto works correctly for files", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Without create, should return path but not create parent dir
      path_no_create <- .path_get_cache_auto(
        "file.txt",
        create = FALSE,
        profile = NULL
      )
      expect_true(is.character(path_no_create))
      expect_true(grepl("file.txt", path_no_create))

      # With create, should create parent directory
      path_create <- .path_get_cache_auto(
        "subdir",
        "file.txt",
        create = TRUE,
        profile = NULL
      )
      expect_true(dir.exists(dirname(path_create)))
      expect_true(grepl("file.txt", path_create))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_cache_auto_version works correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Should include version in path
      path <- .dir_get_cache_auto_version(create = FALSE, profile = NULL)
      expect_true(is.character(path))
      expect_true(grepl("projr", path))
      expect_true(grepl("v0\\.0\\.0-1", path))

      # With subdirectories
      path_sub <- .dir_get_cache_auto_version(
        "subdir",
        create = FALSE,
        profile = NULL
      )
      expect_true(grepl("subdir", path_sub))

      # With create
      path_create <- .dir_get_cache_auto_version(
        "testdir",
        create = TRUE,
        profile = NULL
      )
      expect_true(dir.exists(path_create))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".path_get_cache_auto_version works correctly for files", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Should include version in path
      path <- .path_get_cache_auto_version(
        "file.txt",
        create = FALSE,
        profile = NULL
      )
      expect_true(is.character(path))
      expect_true(grepl("projr", path))
      expect_true(grepl("v0\\.0\\.0-1", path))
      expect_true(grepl("file.txt", path))

      # With create, should create parent directory
      path_create <- .path_get_cache_auto_version(
        "subdir",
        "file.txt",
        create = TRUE,
        profile = NULL
      )
      expect_true(dir.exists(dirname(path_create)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_get_cache_auto_path returns cache directory path", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Should return the cache directory path from yml
      path <- .dir_get_cache_auto_path(profile = NULL)
      expect_true(is.character(path))
      expect_true(length(path) == 1)
    },
    force = TRUE,
    quiet = TRUE
  )
})
