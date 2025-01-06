test_that(".projr_yml_metadata_get_version_format_get and _set", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_init <- .projr_yml_get_default_raw()
      expect_identical(.projr_yml_metadata_get_version_format(NULL), "major.minor.patch-dev")
      yml_projr <- yml_projr_init
      yml_projr[["metadata"]][["version-format"]] <- "major.minor-dev"
      .projr_yml_set(yml_projr)
      expect_identical(.projr_yml_metadata_get_version_format(NULL), "major.minor-dev")
      .projr_yml_set(yml_projr_init)
      yml_projr <- .projr_yml_get_default_raw()
      expect_error(.projr_yml_metadata_set_version_format("abc", NULL))
      expect_error(.projr_yml_metadata_set_version_format("abc"))
      .projr_yml_metadata_set_version_format("major.dev", NULL)
      expect_identical(.projr_yml_metadata_get_version_format(NULL), "major.dev")
      expect_error(
        .projr_yml_metadata_set_version_format(c("abc"), NULL)
        )
      expect_error(
        .projr_yml_metadata_set_version_format(c("major.dev", "major.minor-dev"), NULL)
        )
      expect_error(
        .projr_yml_metadata_set_version_format(1, NULL))
      expect_error(
        .projr_yml_metadata_set_version_format(profile = NULL)
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

test_that(".projr_version_format_list_get works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_yml_metadata_set_version_format("major.minor.patch-dev", NULL)
      expect_identical(
        .projr_version_format_list_get(NULL),
        list("component" = cv, "sep" = svd)
      )
      .projr_yml_metadata_set_version_format("major.minor.patch.dev", NULL)
      expect_identical(
        .projr_version_format_list_get(NULL),
        list("component" = cv, "sep" = svp)
      )
      .projr_yml_metadata_set_version_format("major.minor-dev", NULL)
      expect_identical(
        .projr_version_format_list_get(NULL),
        list("component" = cv[-3], "sep" = svd[-2])
      )
      .projr_yml_metadata_set_version_format("major.minor.dev", NULL)
      expect_identical(
        .projr_version_format_list_get(NULL),
        list("component" = cv[-3], "sep" = svp[-2])
      )
      .projr_yml_metadata_set_version_format("major-dev", NULL)
      expect_identical(
        .projr_version_format_list_get(NULL),
        list("component" = cv[-c(2:3)], "sep" = svd[-c(1:2)])
      )
      .projr_yml_metadata_set_version_format("major.dev", NULL)
      expect_identical(
        .projr_version_format_list_get(NULL),
        list("component" = cv[-c(2:3)], "sep" = svp[-c(1:2)])
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_version_format_check works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.projr_version_format_check("0.0.0-1"))
      expect_true(.projr_version_format_check("0.0.0"))
      expect_error(.projr_version_format_check("0.0.0.1"))
      expect_error(.projr_version_format_check("0.0-1"))
      expect_error(.projr_version_format_check(1))
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that("projr_version_get works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(
        .projr_version_current_vec_get(), c(rep(0L, 3), 1L)
      )
      projr_version_set("7.9.2-1")
      expect_identical(projr_version_get(), "7.9.2-1")
      projr_version_set("7.9.2")
      expect_identical(projr_version_get(), "7.9.2")
      expect_identical(.projr_version_get(dev_force = TRUE), "7.9.2-1")
      expect_identical(projr_version_get(), "7.9.2")
      .projr_yml_metadata_set_version_format("major.dev", NULL)

      projr_version_set("0.1")
      expect_identical(projr_version_get(), "0.1")
      expect_error(projr_version_set())
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".projr_version_run_onwards_get works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.42.33-1")
      # debugonce(.projr_version_run_onwards_get)
      # debugonce(.projr_version_run_onwards_get_output)
      expect_identical(
        .projr_version_run_onwards_get("major"),
        list(
          desc = c(
            run = "1.0.0", failure = "0.42.33-1", success = "1.0.0"
          )
        )
      )
      projr_version_set("0.42.33-9001")
      expect_identical(
        .projr_version_run_onwards_get("major"),
        list(
          desc = c(
            run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0"
          )
        )
      )
      projr_version_set("0.42.33-1")
      expect_identical(
        .projr_version_run_onwards_get("dev"),
        list(
          desc = c(
            run = "0.42.33-2", failure = "0.42.33-2", success = "0.42.33-2"
          )
        )
      )
      expect_identical(
        .projr_version_run_onwards_get(NULL),
        list(
          desc = c(
            run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
          )
        )
      )
      projr_version_set("0.42.33")
      expect_identical(
        .projr_version_run_onwards_get(NULL),
        list(
          desc = c(
            run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
          )
        )
      )
      projr_version_set("0.42.33")
      expect_identical(
        .projr_version_run_onwards_get("patch"),
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
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_yml_metadata_set_version_format("major.dev", NULL)
      projr_version_set("1.2")
      desc <- .projr_desc_get()
      expect_identical(desc[1, "Version"][[1]], "1.2")
      projr_version_set("1.4")
      desc <- .projr_desc_get()
      expect_identical(desc[1, "Version"][[1]], "1.4")
      invisible(.projr_version_bump_dev())
      desc <- .projr_desc_get()
      expect_identical(desc[1, "Version"][[1]], "1.5")
      expect_error(invisible(.projr_version_bump_dev("does_not_exist")))
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that("checking min version works", {
  skip_if(.is_test_select())
  expect_identical(
    .projr_version_comp_vec_min_get(version_min = "patch"),
    .projr_version_comp_vec_min_get(version_min = "any")
  )
  expect_identical(
    .projr_version_comp_vec_min_get(version_min = "major"),
    "major"
  )
  expect_identical(
    .projr_version_comp_vec_min_get(version_min = "minor"),
    c("major", "minor")
  )
  expect_false(
    .projr_version_comp_min_check(
      bump_component = "patch",
      version_min = "major"
    )
  )
  expect_false(
    .projr_version_comp_min_check(
      bump_component = "patch",
      version_min = "minor"
    )
  )
  expect_true(
    .projr_version_comp_min_check(
      bump_component = "patch",
      version_min = "patch"
    )
  )
  expect_true(
    .projr_version_comp_min_check(
      bump_component = "major",
      version_min = "major"
    )
  )
  expect_true(
    .projr_version_comp_min_check(
      bump_component = "minor",
      version_min = "minor"
    )
  )
  expect_false(
    .projr_version_comp_min_check(
      bump_component = "minor",
      version_min = "major"
    )
  )
})
