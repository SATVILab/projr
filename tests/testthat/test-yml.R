test_that("basic yml functions work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))
  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- fn_vec

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      yml.int <- .yml_get_default_raw()
      expect_identical(class(.yml_get_default_raw()), "list")
      expect_identical(class(.yml_bd_get()), "list")
      expect_identical(class(.desc_get()), c("matrix", "array"))
      yml.min <- list(
        "directories" = NULL, "build" = list()
      )
      .yml_set(yml.min)
      expect_identical(.yml_get_default_raw(), yml.min)
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_identical(class(.yml_get_default_raw()), "list")
      expect_identical(.yml_bd_get(), list())
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_yml_check works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      expect_true(projr_yml_check())
      projr_yml_dest_add_local(
        title = "test",
        content = "raw-data",
        path = "_outputting",
        send_inspect = "file",
        send_strategy = "upload-all"
      )
      expect_true(projr_yml_check())
    }
  )
})

test_that("projr_yml_dest_add_* functions work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(!.is_string(.auth_get_github_pat_find()), "GITHUB_PAT not available")
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_yml_unset_remote()
      projr_yml_dest_add_local(
        title = "test", content = "raw-data", path = "_archive"
      )
      expect_true(!is.null(.yml_dest_get_type("local", "default")))

      projr_yml_dest_add_github(
        title = "test", content = "raw-data"
      )
      expect_true(!is.null(.yml_dest_get_type("github", "default")))
    }
  )
})

test_that("projr_yml_get works with profile parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test getting default profile
      yml_default <- projr_yml_get(profile = "default")
      expect_true(is.list(yml_default))
      expect_true("directories" %in% names(yml_default))

      # Test getting with NULL profile (uses active)
      yml_null <- projr_yml_get(profile = NULL)
      expect_true(is.list(yml_null))

      # Test with check parameter
      yml_checked <- projr_yml_get(profile = "default", check = FALSE)
      expect_true(is.list(yml_checked))
    }
  )
})

test_that(".yml_get routes to correct profile function", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with NULL profile (should use .yml_get_null)
      yml_null <- .yml_get(NULL)
      expect_true(is.list(yml_null))

      # Test with specific profile
      yml_default <- .yml_get("default")
      expect_true(is.list(yml_default))
    }
  )
})

test_that(".yml_get_profile switches between profile types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test "local" keyword
      yml_local <- .yml_get_profile("local")
      expect_true(is.list(yml_local))

      # Test "default" keyword
      yml_default <- .yml_get_profile("default")
      expect_true(is.list(yml_default))
      expect_true("directories" %in% names(yml_default))

      # Test specific profile (should return empty list if doesn't exist)
      yml_custom <- .yml_get_profile("nonexistent")
      expect_true(is.list(yml_custom))
    }
  )
})

test_that(".yml_get_null merges root, profile, and local", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test merging logic
      yml_merged <- .yml_get_null()
      expect_true(is.list(yml_merged))
      expect_true("directories" %in% names(yml_merged))
      expect_true("build" %in% names(yml_merged))
    }
  )
})

test_that(".yml_merge handles three-way merge with precedence", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test basic merging with different precedence levels
  yml_default <- list(a = 1, b = 2, c = list(x = 1, y = 2))
  yml_profile <- list(b = 3, c = list(y = 3, z = 4))
  yml_local <- list(c = list(z = 5))

  result <- .yml_merge(yml_default, yml_profile, yml_local)

  # Local should override profile and default for nested lists
  expect_equal(result$a, 1) # From default (only place)
  expect_equal(result$b, 3) # From profile (overrides default)
  expect_equal(result$c$x, 1) # From default
  expect_equal(result$c$y, 3) # From profile
  expect_equal(result$c$z, 5) # From local (highest precedence)

  # Test with non-list elements
  yml_default2 <- list(val = "default")
  yml_profile2 <- list(val = "profile")
  yml_local2 <- list(val = "local")

  result2 <- .yml_merge(yml_default2, yml_profile2, yml_local2)
  expect_equal(result2$val, "local") # Local takes precedence

  # Test with NULL values
  # When profile has NULL and local is empty, it falls back to default
  yml_default3 <- list(a = 1, b = 2)
  yml_profile3 <- list(b = NULL)
  yml_local3 <- list()

  result3 <- .yml_merge(yml_default3, yml_profile3, yml_local3)
  expect_equal(result3$a, 1)
  # Based on merge logic: local=NULL, profile=NULL -> returns default
  expect_equal(result3$b, 2)
})

test_that(".yml_get_default_raw reads yaml file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_raw <- .yml_get_default_raw()
      expect_true(is.list(yml_raw))
      expect_true("directories" %in% names(yml_raw))

      # Test when file doesn't exist
      fs::file_move("_projr.yml", "_projr.yml.bak")
      yml_empty <- .yml_get_default_raw()
      expect_identical(yml_empty, list())
      fs::file_move("_projr.yml.bak", "_projr.yml")
    }
  )
})

test_that(".yml_get_default filters top-level keys", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_default <- .yml_get_default()
      expect_true(is.list(yml_default))

      # Should only contain allowed top-level keys
      allowed_keys <- c("directories", "build", "dev", "metadata")
      expect_true(all(names(yml_default) %in% allowed_keys))
    }
  )
})

test_that(".yml_get_local gets local filtered yml", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test getting local (should be empty if no local file)
      yml_local <- .yml_get_local()
      expect_true(is.list(yml_local))

      # Create a local file and test
      writeLines("build:\n  git: false", "_projr-local.yml")
      yml_local2 <- .yml_get_local()
      expect_true(is.list(yml_local2))
      if (file.exists("_projr-local.yml")) {
        unlink("_projr-local.yml")
      }
    }
  )
})

test_that(".yml_get_profile_spec handles profile vectors", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with NULL (uses active profile)
      yml_spec <- .yml_get_profile_spec(NULL)
      expect_true(is.list(yml_spec))

      # Test with specific profile string
      yml_spec2 <- .yml_get_profile_spec("default")
      expect_true(is.list(yml_spec2))
    }
  )
})

test_that(".yml_get_profile_ind gets individual profile", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test getting default profile
      yml_ind <- .yml_get_profile_ind("default")
      expect_true(is.list(yml_ind))
      expect_true("directories" %in% names(yml_ind))

      # Test getting non-existent profile
      yml_none <- .yml_get_profile_ind("nonexistent")
      expect_identical(yml_none, list())
    }
  )
})

test_that(".yml_get_profile_ind_raw reads raw profile file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with "default" - should read _projr.yml
      yml_raw <- .yml_get_profile_ind_raw("default")
      expect_true(is.list(yml_raw))

      # Test with custom profile that doesn't exist
      yml_custom <- .yml_get_profile_ind_raw("custom")
      expect_identical(yml_custom, list())

      # Create custom profile and test
      writeLines("build:\n  git: false", "_projr-custom.yml")
      yml_custom2 <- .yml_get_profile_ind_raw("custom")
      expect_true(is.list(yml_custom2))
      expect_true("build" %in% names(yml_custom2))
      unlink("_projr-custom.yml")
    }
  )
})

test_that(".yml_get_profile_list_min_3 pads to minimum 3", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with 1 profile
  profile_vec1 <- c("default")
  result1 <- .yml_get_profile_list_min_3(profile_vec1)
  expect_equal(length(result1), 3)

  # Test with 2 profiles
  profile_vec2 <- c("default", "local")
  result2 <- .yml_get_profile_list_min_3(profile_vec2)
  expect_equal(length(result2), 3)

  # Test with 3 profiles (no padding needed)
  profile_vec3 <- c("default", "custom", "local")
  result3 <- .yml_get_profile_list_min_3(profile_vec3)
  expect_equal(length(result3), 3)
})

test_that(".yml_merge_list merges list of 3 profiles", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  profile_list <- list(
    list(a = 1), # Highest precedence (position 1)
    list(b = 2), # Middle precedence (position 2)
    list(c = 3) # Lowest precedence (position 3)
  )

  result <- .yml_merge_list(profile_list)
  expect_true(is.list(result))
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
  expect_equal(result$c, 3)
})

test_that(".yml_merge_list_add adds profiles to existing merge", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_existing <- list(x = 1, y = 2)
  profile_list <- list(
    list(y = 3), # Position 1 (merged with existing)
    list(z = 4), # Position 2
    list(w = 5) # Position 3
  )

  result <- .yml_merge_list_add(yml_existing, profile_list)
  expect_true(is.list(result))
  expect_equal(result$x, 1) # From existing
  expect_equal(result$y, 2) # Existing takes precedence as "local"
})

test_that(".yml_get_filter_top_level filters to allowed keys", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_input <- list(
    directories = list(output = "_output"),
    build = list(git = TRUE),
    invalid_key = "should_be_removed",
    metadata = list(version = "0.0.1"),
    another_invalid = 123
  )

  result <- .yml_get_filter_top_level(yml_input)
  expect_true(is.list(result))
  expect_false("invalid_key" %in% names(result))
  expect_false("another_invalid" %in% names(result))
  expect_true("directories" %in% names(result))
  expect_true("build" %in% names(result))
  expect_true("metadata" %in% names(result))
})

test_that(".yml_get_filter_top_level_ind matches individual keys", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_input <- list(
    first = 1,
    directories = list(output = "_output"),
    build = list(git = TRUE),
    last = 999
  )

  # Test finding "directories"
  pos_dirs <- .yml_get_filter_top_level_ind(yml_input, "directories")
  expect_equal(pos_dirs, 2)

  # Test finding non-existent key
  pos_none <- .yml_get_filter_top_level_ind(yml_input, "nonexistent")
  expect_null(pos_none)

  # Test finding first match when multiple exist
  yml_dup <- list(test = 1, test = 2, other = 3)
  pos_dup <- .yml_get_filter_top_level_ind(yml_dup, "test")
  expect_equal(pos_dup, 1)
})

test_that(".yml_get_path resolves profile paths correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test default profile
      path_default <- .yml_get_path("default")
      expect_true(grepl("_projr\\.yml$", path_default))

      # Test custom profile
      path_custom <- .yml_get_path("myprofile")
      expect_true(grepl("_projr-myprofile\\.yml$", path_custom))

      # Test with NULL/missing
      path_null <- .yml_get_path(NULL)
      expect_true(grepl("_projr\\.yml$", path_null))
    }
  )
})

test_that(".yml_set_root writes to root yml file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      test_yml <- list(
        directories = list(output = list(path = "_test_output")),
        build = list(git = TRUE)
      )

      result <- .yml_set_root(test_yml)
      expect_true(result)

      # Verify it was written
      yml_read <- .yml_get_default_raw()
      expect_equal(yml_read$directories$output$path, "_test_output")
    }
  )
})

test_that(".yml_complete adds missing keys with defaults", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with non-NULL default
  yml_input <- list(a = 1)
  result <- .yml_complete(yml_input, "b", 2)
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)

  # Test with NULL default
  yml_input2 <- list(a = 1)
  result2 <- .yml_complete(yml_input2, "b", NULL)
  expect_equal(result2$a, 1)
  expect_null(result2$b)
  expect_true("b" %in% names(result2))

  # Test when key already exists (should not override)
  yml_input3 <- list(a = 1)
  result3 <- .yml_complete(yml_input3, "a", 999)
  expect_equal(result3$a, 1) # Should keep original value
})

test_that(".yml_complete_default adds default value", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_input <- list(existing = "value")

  # Add new key with default
  result <- .yml_complete_default(yml_input, "newkey", "default_value")
  expect_equal(result$newkey, "default_value")
  expect_equal(result$existing, "value")

  # Key already exists, should not override
  result2 <- .yml_complete_default(yml_input, "existing", "new_default")
  expect_equal(result2$existing, "value")
})

test_that(".yml_complete_null adds NULL entry", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_input <- list(a = 1, b = 2)

  # Add NULL entry
  result <- .yml_complete_null(yml_input, "c")
  expect_true("c" %in% names(result))
  expect_null(result$c)

  # Already has the key, should not override
  yml_with_c <- list(a = 1, c = "existing")
  result2 <- .yml_complete_null(yml_with_c, "c")
  expect_equal(result2$c, "existing")
})

test_that(".yml_order reorders keys correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  yml_unordered <- list(
    metadata = list(version = "0.0.1"),
    other = "value",
    build = list(git = TRUE),
    directories = list(output = "_output")
  )

  result <- .yml_order(yml_unordered)

  # Check that preferred keys come first
  names_result <- names(result)
  dirs_pos <- which(names_result == "directories")
  build_pos <- which(names_result == "build")
  other_pos <- which(names_result == "other")

  expect_true(dirs_pos < build_pos) # directories before build
  expect_true(build_pos < other_pos) # build before other keys

  # All keys should still be present
  expect_equal(length(result), length(yml_unordered))
})
