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
version_format <- vf_mmd
fn <- fn_mm

cv <- c("major", "minor", "patch", "dev")
svd <- c(".", ".", "-")
svp <- c(".", ".", ".")

fn <- fn_mmp

test_that(".get_version_format_list works", {

  # check that .get_version works
  expect_identical(
    .get_version_format_list(vf_mmpd),
    list("components" = cv, "sep" = svd)
  )
  expect_identical(
    .get_version_format_list(vf_mmpp),
    list("components" = cv, "sep" = svp)
  )
  expect_identical(
    .get_version_format_list(vf_mmd),
    list("components" = cv[-3], "sep" = svd[-2])
  )
  expect_identical(
    .get_version_format_list(vf_mmp),
    list("components" = cv[-3], "sep" = svp[-2])
  )
  expect_identical(
    .get_version_format_list(vf_md),
    list("components" = cv[-(2:3)], "sep" = svd[-(1:2)])
  )
  expect_identical(
    .get_version_format_list(vf_mp),
    list("components" = cv[-(2:3)], "sep" = svp[-(1:2)])
  )
})

test_that("get_proj_nm works", {
  # check that .get_version works
  expect_identical(
    .get_proj_nm(fn_mmp, vf_mmpd),
    fn_base
  )
  expect_identical(
    .get_proj_nm(fn_mmp, vf_mmpd),
    fn_base
  )
  expect_identical(
    .get_proj_nm(fn_mm, vf_mmd),
    fn_base
  )
  expect_identical(
    .get_proj_nm(fn_mm, vf_mmd),
    fn_base
  )
  expect_identical(
    .get_proj_nm(fn_m, vf_md),
    fn_base
  )
  expect_identical(
    .get_proj_nm(fn_m, vf_md),
    fn_base
  )
})

test_that(".get_version_orig_vec works", {
  # mmp
  expect_identical(
    .get_version_orig_vec(
      fn = fn_mmp,
      version_desc = v_mmp_e,
      proj_nm = fn_base
    ),
    c(6L, 9L, 9L, 2L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = fn_mmp,
      version_desc = v_mmp_l1,
      proj_nm = fn_base
    ),
    c(7L, 9L, 9L, 2L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = fn_mmp,
      version_desc = v_mmp_l2,
      proj_nm = fn_base
    ),
    c(7L, 9L, 9L, 1L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = gsub("\\-2$", "-10005", fn_mmp),
      version_desc = v_mmp_l2,
      proj_nm = fn_base
    ),
    c(7L, 9L, 9L, 9000L)
  )
  # mm
  expect_identical(
    .get_version_orig_vec(
      fn = fn_mm,
      version_desc = v_mm_e,
      proj_nm = fn_base
    ),
    c(10L, 95L, 0L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = fn_mm,
      version_desc = v_mm_l1,
      proj_nm = fn_base
    ),
    c(11L, 95L, 3L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = fn_mm,
      version_desc = v_mm_l2,
      proj_nm = fn_base
    ),
    c(11L, 95L, 1L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = gsub("\\-0$", "-9000", fn_mm),
      version_desc = v_mm_l2,
      proj_nm = fn_base
    ),
    c(11L, 95L, 9000L)
  )
  # m
  expect_identical(
    .get_version_orig_vec(
      fn = fn_m,
      version_desc = v_m_e,
      proj_nm = fn_base
    ),
    c(0L, 12L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = fn_m,
      version_desc = v_m_l1,
      proj_nm = fn_base
    ),
    c(11L, 3L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = fn_m,
      version_desc = v_m_l2,
      proj_nm = fn_base
    ),
    c(11L, 1L)
  )
  expect_identical(
    .get_version_orig_vec(
      fn = gsub("\\-12$", "-9003", fn_m),
      version_desc = v_m_l2,
      proj_nm = fn_base
    ),
    c(11L, 9000L)
  )
})

test_that(".get_version_run_on works", {
  expect_identical(
    {
      .get_version_run_on(
        version_orig_vec = c(0, 42, 33, 1),
        bump_component = "major",
        version_format_list = list(
          components = cv,
          sep = svd
        )
      )
    },
    list(
      desc = c(
        run = "1.0.0", failure = "0.42.33-1", success = "1.0.0"
      ),
      bd = c(
        run = "1.0.0", failure = "0.42.33-1", success = "1.0.0-1"
      )
    )
  )
  expect_identical(
    {
      .get_version_run_on(
        version_orig_vec = c(0, 42, 33, 9001),
        bump_component = "major",
        version_format_list = list(
          components = cv,
          sep = svd
        )
      )
    },
    list(
      desc = c(
        run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0"
      ),
      bd = c(
        run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0-9000"
      )
    )
  )
  expect_identical(
    {
      .get_version_run_on(
        version_orig_vec = c(0, 42, 33, 1),
        bump_component = "dev",
        version_format_list = list(
          components = cv,
          sep = svd
        )
      )
    },
    list(
      desc = c(
        run = "0.42.33-2", failure = "0.42.33-2", success = "0.42.33-2"
      ),
      bd = c(
        run = "0.42.33-2", failure = "0.42.33-2", success = "0.42.33-2"
      )
    )
  )
  expect_identical(
    {
      .get_version_run_on(
        version_orig_vec = c(0, 42, 33, 1),
        bump_component = NULL,
        version_format_list = list(
          components = cv,
          sep = svd
        )
      )
    },
    list(
      desc = c(
        run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
      ),
      bd = c(
        run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
      )
    )
  )
})


test_that("projr_version_set works", {
  dir_test <- file.path(tempdir(), "projr_set_version")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  dir.create(dir_test)
  dir_projr <- system.file("project_structure", package = "projr")

  invisible(
    file.copy(
      file.path(dir_projr, "DESCRIPTION"), file.path(dir_test)
    )
  )
  invisible(
    file.copy(
      file.path(dir_projr, "_bookdown.yml"), file.path(dir_test)
    )
  )
  invisible(
    file.copy(
      file.path(dir_projr, "_projr.yml"), file.path(dir_test)
    )
  )

  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("1.2")
      yml_bd <- yaml::read_yaml(
        rprojroot::is_r_package$find_file("_bookdown.yml")
      )
      desc_file <- read.dcf(
        rprojroot::is_r_package$find_file("DESCRIPTION")
      )
      expect_identical(yml_bd$book_filename, "reportV1.2")
      expect_identical(basename(yml_bd$output_dir), "reportV1.2")
      expect_identical(desc_file[1, "Version"][[1]], "1.2")

      projr_version_set("1.3", where = "DESCRIPTION")
      yml_bd <- yaml::read_yaml(
        rprojroot::is_r_package$find_file("_bookdown.yml")
      )
      desc_file <- read.dcf(
        rprojroot::is_r_package$find_file("DESCRIPTION")
      )
      expect_identical(yml_bd$book_filename, "reportV1.2")
      expect_identical(basename(yml_bd$output_dir), "reportV1.2")
      expect_identical(desc_file[1, "Version"][[1]], "1.3")
      projr_version_set("1.4", where = "bookdown")
      desc_file <- read.dcf("DESCRIPTION")
      yml_bd <- yaml::read_yaml(
        rprojroot::is_r_package$find_file("_bookdown.yml")
      )
      desc_file <- read.dcf(
        rprojroot::is_r_package$find_file("DESCRIPTION")
      )
      expect_identical(yml_bd$book_filename, "reportV1.4")
      expect_identical(basename(yml_bd$output_dir), "reportV1.4")
      expect_identical(desc_file[1, "Version"][[1]], "1.3")
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
