test_that(".yml_metadata_get_version_format_get and _set", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_init <- .yml_get_default_raw()
      expect_identical(.yml_metadata_get_version_format(NULL), "major.minor.patch-dev")
      yml_projr <- yml_projr_init
      yml_projr[["metadata"]][["version-format"]] <- "major.minor-dev"
      .yml_set(yml_projr)
      expect_identical(.yml_metadata_get_version_format(NULL), "major.minor-dev")
      .yml_set(yml_projr_init)
      yml_projr <- .yml_get_default_raw()
      expect_error(.yml_metadata_set_version_format("abc", NULL))
      expect_error(.yml_metadata_set_version_format("abc"))
      .yml_metadata_set_version_format("major.dev", NULL)
      expect_identical(.yml_metadata_get_version_format(NULL), "major.dev")
      expect_error(
        .yml_metadata_set_version_format(c("abc"), NULL)
      )
      expect_error(
        .yml_metadata_set_version_format(c("major.dev", "major.minor-dev"), NULL)
      )
      expect_error(
        .yml_metadata_set_version_format(1, NULL)
      )
      expect_error(
        .yml_metadata_set_version_format(profile = NULL)
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

fn_base <- "abc2k1l432jda:12dk[[2314-ak34129V19"
fn_mmp <- paste0(fn_base, "V6.9.9-2")
v_mmp_e <- "6.9.9-2"
v_mmp_l1 <- "7.9.9-2"
v_mmp_l2 <- "7.9.9"
v_mmp_s <- "6.9.4"
vf_mmpd <- "major.minor.patch-dev"
vf_mmpp <- "major.minor.patch.dev"
fn_mm <- paste0(fn_base, "V10.95-0")
v_mm_e <- "10.95"
v_mm_l1 <- "11.95-3"
v_mm_l2 <- "11.95"
v_mm_s <- "0.95-12"
vf_mmd <- "major.minor-dev"
vf_mmp <- "major.minor.dev"
fn_m <- paste0(fn_base, "V0-12")
v_m_e <- "0-12"
v_m_l1 <- "11-3"
v_m_l2 <- "11"
v_m_s <- "0-3"
vf_md <- "major-dev"
vf_mp <- "major.dev"
"version-format" <- vf_mmd
fn <- fn_mm

cv <- c("major", "minor", "patch", "dev")
svd <- c(".", ".", "-")
svp <- c(".", ".", ".")

fn <- fn_mmp

test_that(".version_format_list_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_metadata_set_version_format("major.minor.patch-dev", NULL)
      expect_identical(
        .version_format_list_get(NULL),
        list("component" = cv, "sep" = svd)
      )
      .yml_metadata_set_version_format("major.minor.patch.dev", NULL)
      expect_identical(
        .version_format_list_get(NULL),
        list("component" = cv, "sep" = svp)
      )
      .yml_metadata_set_version_format("major.minor-dev", NULL)
      expect_identical(
        .version_format_list_get(NULL),
        list("component" = cv[-3], "sep" = svd[-2])
      )
      .yml_metadata_set_version_format("major.minor.dev", NULL)
      expect_identical(
        .version_format_list_get(NULL),
        list("component" = cv[-3], "sep" = svp[-2])
      )
      .yml_metadata_set_version_format("major-dev", NULL)
      expect_identical(
        .version_format_list_get(NULL),
        list("component" = cv[-c(2:3)], "sep" = svd[-c(1:2)])
      )
      .yml_metadata_set_version_format("major.dev", NULL)
      expect_identical(
        .version_format_list_get(NULL),
        list("component" = cv[-c(2:3)], "sep" = svp[-c(1:2)])
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".version_format_check works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.version_format_check("0.0.0-1"))
      expect_true(.version_format_check("0.0.0"))
      expect_error(.version_format_check("0.0.0.1"))
      expect_error(.version_format_check("0.0-1"))
      expect_error(.version_format_check(1))
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that("projr_version_get works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(
        .version_current_vec_get(), c(rep(0L, 3), 1L)
      )
      projr_version_set("7.9.2-1")
      expect_identical(projr_version_get(), "7.9.2-1")
      projr_version_set("7.9.2")
      expect_identical(projr_version_get(), "7.9.2")
      expect_identical(.version_get(dev_force = TRUE), "7.9.2-1")
      expect_identical(projr_version_get(), "7.9.2")
      .yml_metadata_set_version_format("major.dev", NULL)

      projr_version_set("0.1")
      expect_identical(projr_version_get(), "0.1")
      expect_error(projr_version_set())
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_run_onwards_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.42.33-1")

      expect_identical(
        .version_run_onwards_get("major"),
        list(
          desc = c(
            run = "1.0.0", failure = "0.42.33-1", success = "1.0.0"
          )
        )
      )
      projr_version_set("0.42.33-9001")
      expect_identical(
        .version_run_onwards_get("major"),
        list(
          desc = c(
            run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0"
          )
        )
      )
      projr_version_set("0.42.33-1")
      expect_identical(
        .version_run_onwards_get("dev"),
        list(
          desc = c(
            run = "0.42.33-2", failure = "0.42.33-2", success = "0.42.33-2"
          )
        )
      )
      expect_identical(
        .version_run_onwards_get(NULL),
        list(
          desc = c(
            run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
          )
        )
      )
      projr_version_set("0.42.33")
      expect_identical(
        .version_run_onwards_get(NULL),
        list(
          desc = c(
            run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
          )
        )
      )
      projr_version_set("0.42.33")
      expect_identical(
        .version_run_onwards_get("patch"),
        list(
          desc = c(
            run = "0.42.34", failure = "0.42.33-1", success = "0.42.34"
          )
        )
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that("projr_version_set works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_metadata_set_version_format("major.dev", NULL)
      projr_version_set("1.2")
      desc <- .desc_get()
      expect_identical(desc[1, "Version"][[1]], "1.2")
      projr_version_set("1.4")
      desc <- .desc_get()
      expect_identical(desc[1, "Version"][[1]], "1.4")
      invisible(.version_bump_dev())
      desc <- .desc_get()
      expect_identical(desc[1, "Version"][[1]], "1.5")
      expect_error(invisible(.version_bump_dev("does_not_exist")))
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that("checking min version works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  expect_identical(
    .version_comp_vec_min_get(version_min = "patch"),
    .version_comp_vec_min_get(version_min = "any")
  )
  expect_identical(
    .version_comp_vec_min_get(version_min = "major"),
    "major"
  )
  expect_identical(
    .version_comp_vec_min_get(version_min = "minor"),
    c("major", "minor")
  )
  expect_false(
    .version_comp_min_check(
      bump_component = "patch",
      version_min = "major"
    )
  )
  expect_false(
    .version_comp_min_check(
      bump_component = "patch",
      version_min = "minor"
    )
  )
  expect_true(
    .version_comp_min_check(
      bump_component = "patch",
      version_min = "patch"
    )
  )
  expect_true(
    .version_comp_min_check(
      bump_component = "major",
      version_min = "major"
    )
  )
  expect_true(
    .version_comp_min_check(
      bump_component = "minor",
      version_min = "minor"
    )
  )
  expect_false(
    .version_comp_min_check(
      bump_component = "minor",
      version_min = "major"
    )
  )
})


test_that(".version_copy_dir works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("1.2.3")
      target_dir <- file.path(tempdir(), "version_test")
      dir.create(target_dir, showWarnings = FALSE)

      # Copy version to directory
      .version_copy_dir(target_dir)

      # Check VERSION file was created
      version_file <- file.path(target_dir, "VERSION")
      expect_true(file.exists(version_file))

      # Check content
      version_content <- readLines(version_file, warn = FALSE)
      expect_identical(version_content, "v1.2.3")

      # Test with dev version
      projr_version_set("2.3.4-5")
      .version_copy_dir(target_dir)
      version_content <- readLines(version_file, warn = FALSE)
      expect_identical(version_content, "v2.3.4-5")

      # Cleanup
      unlink(target_dir, recursive = TRUE)
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_set_desc works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set version in DESCRIPTION
      .version_set_desc("3.4.5")
      desc <- .desc_get()
      expect_identical(desc[1, "Version"][[1]], "3.4.5")

      # Set version with dev component (should be stripped)
      .version_set_desc("3.4.6-7")
      desc <- .desc_get()
      expect_identical(desc[1, "Version"][[1]], "3.4.6-7")

      # Set version with 'v' prefix (should be stripped)
      .version_set_desc("v4.5.6")
      desc <- .desc_get()
      expect_identical(desc[1, "Version"][[1]], "4.5.6")
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_set_file works with different parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with only_if_exists = FALSE (default behavior for new files)
      if (file.exists("VERSION")) file.remove("VERSION")
      result <- .version_set_file("1.2.3", only_if_exists = FALSE)
      expect_true(result)
      expect_true(file.exists("VERSION"))
      version_content <- readLines("VERSION", warn = FALSE)
      expect_identical(version_content, "v1.2.3")

      # Test with only_if_exists = TRUE when file exists
      result <- .version_set_file("2.3.4", only_if_exists = TRUE)
      expect_true(result)
      version_content <- readLines("VERSION", warn = FALSE)
      expect_identical(version_content, "v2.3.4")

      # Test with only_if_exists = TRUE when file doesn't exist
      file.remove("VERSION")
      result <- .version_set_file("3.4.5", only_if_exists = TRUE)
      expect_false(result)
      expect_false(file.exists("VERSION"))

      # Test with custom path_dir
      custom_dir <- file.path(tempdir(), "custom_version")
      .version_set_file("4.5.6", path_dir = custom_dir, only_if_exists = FALSE)
      expect_true(file.exists(file.path(custom_dir, "VERSION")))
      version_content <- readLines(file.path(custom_dir, "VERSION"), warn = FALSE)
      expect_identical(version_content, "v4.5.6")

      # Cleanup
      unlink(custom_dir, recursive = TRUE)
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_current_vec_get_dev works correctly", {
  skip_if(.is_test_select())

  # Test with "dev" as last component
  version_format_dev <- c("major", "minor", "patch", "dev")
  result <- .version_current_vec_get_dev(version_format_dev)
  expect_identical(result, "1")

  # Test with numeric dev component (e.g., 9000)
  version_format_numeric <- c("major", "minor", "patch", "9000")
  result <- .version_current_vec_get_dev(version_format_numeric)
  expect_identical(result, "9000")

  # Test with "1" as dev component
  version_format_one <- c("major", "minor", "1")
  result <- .version_current_vec_get_dev(version_format_one)
  expect_identical(result, "1")
})


test_that(".version_current_vec_get_init_desc works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set version with dot separator
      projr_version_set("1.2.3")
      result <- .version_current_vec_get_init_desc()
      expect_identical(result, c("1", "2", "3"))

      # Set version with dash separator
      projr_version_set("4.5.6-7")
      result <- .version_current_vec_get_init_desc()
      expect_identical(result, c("4", "5", "6", "7"))

      # Set version with mixed separators
      projr_version_set("8.9.10-11")
      result <- .version_current_vec_get_init_desc()
      expect_identical(result, c("8", "9", "10", "11"))
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_chr_get works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with default format (major.minor.patch-dev)
      version_vec <- c(1, 2, 3, 4)
      result <- .version_chr_get(version_vec)
      expect_identical(result, "1.2.3-4")

      # Test with production version (no dev component)
      version_vec_prod <- c(1, 2, 3)
      result <- .version_chr_get(version_vec_prod)
      expect_identical(result, "1.2.3")

      # Test with different version format
      .yml_metadata_set_version_format("major.minor-dev", NULL)
      version_vec_mm <- c(5, 6, 7)
      result <- .version_chr_get(version_vec_mm)
      expect_identical(result, "5.6-7")

      # Test with single component
      .yml_metadata_set_version_format("major-dev", NULL)
      version_vec_m <- c(8, 9)
      result <- .version_chr_get(version_vec_m)
      expect_identical(result, "8-9")

      # Reset to default
      .yml_metadata_set_version_format("major.minor.patch-dev", NULL)
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_run_onwards_get_dev works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with dev version
      version_vec <- c(1, 2, 3, 4)
      version_format_sep <- c(".", ".", "-")
      result <- .version_run_onwards_get_dev(version_vec, version_format_sep)

      expect_true(is.list(result))
      expect_true("desc" %in% names(result))
      expect_identical(result$desc[["run"]], "1.2.3-4")
      expect_identical(result$desc[["failure"]], "1.2.3-4")
      expect_identical(result$desc[["success"]], "1.2.3-4")

      # Test with production version that needs dev appended
      version_vec_prod <- c(1, 2, 3)
      result_prod <- .version_run_onwards_get_dev(version_vec_prod, version_format_sep)
      expect_identical(result_prod$desc[["run"]], "1.2.3-1")
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_run_onwards_get_dev_append_dev works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      version_format_sep <- c(".", ".", "-")

      # Test when version already has dev component
      version_vec_with_dev <- c(1, 2, 3, 4)
      result <- .version_run_onwards_get_dev_append_dev(version_vec_with_dev, version_format_sep)
      expect_identical(result, c(1, 2, 3, 4))

      # Test when version needs dev component (default format uses "dev")
      version_vec_no_dev <- c(1, 2, 3)
      result_no_dev <- .version_run_onwards_get_dev_append_dev(version_vec_no_dev, version_format_sep)
      expect_identical(result_no_dev, c(1, 2, 3, "1"))

      # Test with shorter format (major.minor-dev)
      .yml_metadata_set_version_format("major.minor-dev", NULL)
      version_format_sep_short <- c(".", "-")
      version_vec_short <- c(2, 3)
      result_short <- .version_run_onwards_get_dev_append_dev(version_vec_short, version_format_sep_short)
      expect_identical(result_short, c(2, 3, "1"))

      # Reset to default
      .yml_metadata_set_version_format("major.minor.patch-dev", NULL)
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_run_onwards_get_bump works correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      version_vec <- c(1, 2, 3, 4)
      version_format_comp <- c("major", "minor", "patch", "dev")
      version_format_sep <- c(".", ".", "-")

      # Test major bump
      result_major <- .version_run_onwards_get_bump(
        version_vec, version_format_comp, version_format_sep, "major"
      )
      expect_identical(result_major$desc[["run"]], "2.0.0")
      expect_identical(result_major$desc[["failure"]], "1.2.3-4")
      expect_identical(result_major$desc[["success"]], "2.0.0")

      # Test minor bump
      result_minor <- .version_run_onwards_get_bump(
        version_vec, version_format_comp, version_format_sep, "minor"
      )
      expect_identical(result_minor$desc[["run"]], "1.3.0")
      expect_identical(result_minor$desc[["failure"]], "1.2.3-4")

      # Test patch bump
      result_patch <- .version_run_onwards_get_bump(
        version_vec, version_format_comp, version_format_sep, "patch"
      )
      expect_identical(result_patch$desc[["run"]], "1.2.4")
      expect_identical(result_patch$desc[["failure"]], "1.2.3-4")
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".version_run_onwards_get_bump_update_vec works correctly", {
  skip_if(.is_test_select())

  # Test major bump
  version_vec <- c(1, 2, 3)
  version_format_comp <- c("major", "minor", "patch")
  result_major <- .version_run_onwards_get_bump_update_vec(
    version_vec, version_format_comp, "major"
  )
  expect_identical(result_major, c(2, 0, 0))

  # Test minor bump
  result_minor <- .version_run_onwards_get_bump_update_vec(
    version_vec, version_format_comp, "minor"
  )
  expect_identical(result_minor, c(1, 3, 0))

  # Test patch bump
  result_patch <- .version_run_onwards_get_bump_update_vec(
    version_vec, version_format_comp, "patch"
  )
  expect_identical(result_patch, c(1, 2, 4))

  # Test when bump is the last component (should not reset anything)
  version_vec2 <- c(5, 6, 7)
  result_last <- .version_run_onwards_get_bump_update_vec(
    version_vec2, version_format_comp, "patch"
  )
  expect_identical(result_last, c(5, 6, 8))
})
