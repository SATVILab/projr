
test_that("projr_version_format_get and _set", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(projr_version_format_get(), "major.minor.patch-dev")
      yml_projr <- .projr_yml_get()
      yml_projr[["version_format"]] <- "abc"
      .projr_yml_set(yml_projr)
      expect_error(projr_version_format_get())
      projr_version_format_set("major.dev")
      expect_identical(projr_version_format_get(), "major.dev")
      expect_error(projr_version_format_set(c("abc")))
      expect_error(projr_version_format_set(c("major.dev", "major.minor-dev")))
      expect_error(projr_version_format_set(1))
      expect_error(projr_version_format_set())
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
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
version_format <- vf_mmd
fn <- fn_mm

cv <- c("major", "minor", "patch", "dev")
svd <- c(".", ".", "-")
svp <- c(".", ".", ".")

fn <- fn_mmp

test_that(".projr_version_format_list_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(projr_name_get(), "test_projr")
      projr_version_format_set("major.minor.patch-dev")
      expect_identical(
        .projr_version_format_list_get(),
        list("components" = cv, "sep" = svd)
      )
      projr_version_format_set("major.minor.patch.dev")
      expect_identical(
        .projr_version_format_list_get(),
        list("components" = cv, "sep" = svp)
      )
      projr_version_format_set("major.minor-dev")
      expect_identical(
        .projr_version_format_list_get(),
        list("components" = cv[-3], "sep" = svd[-2])
      )
      projr_version_format_set("major.minor.dev")
      expect_identical(
        .projr_version_format_list_get(),
        list("components" = cv[-3], "sep" = svp[-2])
      )
      projr_version_format_set("major-dev")
      expect_identical(
        .projr_version_format_list_get(),
        list("components" = cv[-c(2:3)], "sep" = svd[-c(1:2)])
      )
      projr_version_format_set("major.dev")
      expect_identical(
        .projr_version_format_list_get(),
        list("components" = cv[-c(2:3)], "sep" = svp[-c(1:2)])
      )
      yml_projr <- .projr_yml_get()
      yml_projr[["version_format"]] <- "abc"
      .projr_yml_set(yml_projr)
      # browser()
      # expect_error(.projr_version_format_list_get())
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".projr_version_format_check works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
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
  unlink(dir_test, recursive = TRUE)
})


test_that("projr_version_get works", {
  dir_test <- file.path(tempdir(), paste0("report"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(.projr_version_current_vec_get(), c(rep(0L, 3), 1L))
      projr_version_set("7.9.2-1", "DESCRIPTION")
      expect_identical(projr_version_get(), "7.9.2-1")
      projr_version_set("8.0.0-1", "bookdown")
      expect_identical(projr_version_get(), "8.0.0-1")
      projr_version_format_set("major.dev")
      projr_version_set("0.1", "DESCRIPTION")
      projr_version_set("1.2", "bookdown")
      expect_identical(projr_version_get(), "1.2")
      expect_error(projr_version_set())
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that(".projr_version_run_onwards_get works", {
  dir_test <- file.path(tempdir(), paste0("report"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.42.33-1")
      expect_identical(
        .projr_version_run_onwards_get("major"),
        list(
          desc = c(
            run = "1.0.0", failure = "0.42.33-1", success = "1.0.0"
          ),
          bd = c(
            run = "1.0.0", failure = "0.42.33-1", success = "1.0.0-1"
          )
        )
      )
      projr_version_set("0.42.33-9001")
      expect_identical(
        .projr_version_run_onwards_get("major"),
        list(
          desc = c(
            run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0"
          ),
          bd = c(
            run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0-9000"
          )
        )
      )
      projr_version_set("0.42.33-1")
      expect_identical(
        .projr_version_run_onwards_get("dev"),
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
        .projr_version_run_onwards_get(NULL),
        list(
          desc = c(
            run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
          ),
          bd = c(
            run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
          )
        )
      )
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that("projr_version_set works", {
  dir_test <- file.path(tempdir(), paste0("report"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_format_set("major.dev")
      projr_version_set("1.2", where = c("bookdown", "DESCRIPTION"))
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(yml_bd$book_filename, "reportV1.2")
      expect_identical(basename(yml_bd$output_dir), "reportV1.2")
      expect_identical(desc[1, "Version"][[1]], "1.2")

      projr_version_set("1.3", where = "DESCRIPTION")
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(yml_bd$book_filename, "reportV1.2")
      expect_identical(basename(yml_bd$output_dir), "reportV1.2")
      expect_identical(desc[1, "Version"][[1]], "1.3")
      projr_version_set("1.4", where = "bookdown")
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(yml_bd$book_filename, "reportV1.4")
      expect_identical(basename(yml_bd$output_dir), "reportV1.4")
      expect_identical(desc[1, "Version"][[1]], "1.3")
      invisible(projr_version_bump())
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(basename(yml_bd$output_dir), "reportV1.5")
      expect_identical(desc[1, "Version"][[1]], "1.3")
      invisible(projr_version_bump(
        component = "major", where = c("bookdown", "DESCRIPTION")
      ))
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(basename(yml_bd$output_dir), "reportV2.1")
      expect_identical(desc[1, "Version"][[1]], "2.1")
      projr_version_set("0.9291", where = c("bookdown", "DESCRIPTION"))
      invisible(projr_version_bump(
        component = "major", where = c("bookdown", "DESCRIPTION")
      ))
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(basename(yml_bd$output_dir), "reportV1.9000")
      expect_identical(desc[1, "Version"][[1]], "1.9000")
      projr_version_format_set("major.minor-dev")
      projr_version_set("1.2-3", where = c("bookdown", "DESCRIPTION"))
      invisible(projr_version_bump(
        component = "major", where = c("bookdown", "DESCRIPTION")
      ))
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(basename(yml_bd$output_dir), "reportV2.0-1")
      expect_identical(desc[1, "Version"][[1]], "2.0-1")
      projr_version_set("1.2-3", where = c("bookdown", "DESCRIPTION"))
      invisible(projr_version_bump(
        component = "major", where = c("bookdown", "DESCRIPTION"),
        onto_dev = FALSE
      ))
      yml_bd <- .projr_yml_bd_get()
      desc <- .projr_desc_get()
      expect_identical(basename(yml_bd$output_dir), "reportV2.0")
      expect_identical(desc[1, "Version"][[1]], "2.0")
      expect_error(invisible(projr_version_bump("does_not_exist")))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
