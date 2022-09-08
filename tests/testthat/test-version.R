fn_base <- "abc2k1l432jda:12dk[[2314-ak34129V19"
fn_mmp <- paste0(fn_base, "V6.9.9-2")
vf_mmpd <- "major.minor.patch-dev"
vf_mmpp <- "major.minor.patch.dev"
fn_mm <- paste0(fn_base, "V10.95-0")
vf_mmd <- "major.minor-dev"
vf_mmp <- "major.minor.dev"
fn_m <- paste0(fn_base, "V0-12")
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

test_that(".get_version_current works", {
  expect_identical(
    .get_version_current(
      fn = fn_mmp,
      proj_nm = fn_base
    ),
    c(6L, 9L, 9L, 2L)
  )
  expect_identical(
    .get_version_current(
      fn = fn_mm,
      proj_nm = fn_base
    ),
    c(10L, 95L, 0L)
  )
  expect_identical(
    .get_version_current(
      fn = fn_m,
      proj_nm = fn_base
    ),
    c(0L, 12L)
  )
})

test_that(".get_version_final works", {
  expect_identical(
    .get_version_final(
      version_orig = c(0, 42, 33, 1),
      bump_component = "minor",
      version_format_list = list(
        components = cv,
        sep = svd
      )
    ),
    c("dev" = "0.43.0-0", "no_dev" = "0.43.0")
  )

  expect_identical(
    .get_version_final(
      version_orig = c(0, 42, 33, 1),
      bump_component = "major",
      version_format_list = list(
        components = cv,
        sep = svd
      )
    ),
    c("dev" = "1.0.0-0", "no_dev" = "1.0.0")
  )

  expect_identical(
    .get_version_final(
      version_orig = c(0, 42, 33, 1),
      bump_component = "patch",
      version_format_list = list(
        components = cv,
        sep = svp
      )
    ),
    c("dev" = "0.42.34.0", "no_dev" = "0.42.34")
  )
})

test_that(".get_version_and_fn_final works", {
  expect_identical(
    .get_version_and_fn_final(
      version_format = "major.minor-dev",
      fn_orig = "acbV10.1-1",
      bump_component = "minor"
    ),
    c(fn = "acbV10.2-0", version = "10.2-0")
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
