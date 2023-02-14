test_that("projr_build_dev works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

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
      projr_init()
      projr_build_dev(quiet = TRUE)
      projr_version_get()
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.0-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})


test_that("projr_build_copy_output_direct works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

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
      projr_init()
      yml_projr_init <- .projr_yml_get()
      yml_bd_init <- .projr_yml_bd_get()
      # run when there are no files in dir_output
      expect_true(.projr_build_copy_output_direct(output_run = TRUE))
      invisible({
        file.create(
          projr_path_get("output", "a.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("output", "b.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("output", "dir_c", "c.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("output", "dir_d", "d.txt", output_safe = TRUE)
        )
      })

      # test that files are not zipped in safe directory
      # and directories are zipped
      # -------------------------------------
      expect_true(.projr_build_copy_output_direct(output_run = FALSE))
      dir_output_safe <- projr_path_get("output", output_safe = TRUE)
      expect_true(file.exists(file.path(dir_output_safe, "a.txt")))
      expect_true(file.exists(file.path(dir_output_safe, "b.txt")))
      expect_true(file.exists(file.path(dir_output_safe, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_output_safe, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_output_safe, "dir_c")))
      expect_false(file.exists(file.path(dir_output_safe, "dir_d")))

      # test that files are coped over to output directory
      # and directories are zipped
      # -------------------------------------
      expect_true(.projr_build_copy_output_direct(output_run = TRUE))
      expect_false(file.exists(file.path(dir_output_safe, "a.txt")))
      expect_false(file.exists(file.path(dir_output_safe, "b.txt")))
      expect_false(file.exists(file.path(dir_output_safe, "dir_c.zip")))
      expect_false(file.exists(file.path(dir_output_safe, "dir_d.zip")))
      dir_output_final <- projr_path_get("output", output_safe = FALSE)
      expect_true(file.exists(file.path(dir_output_final, "a.txt")))
      expect_true(file.exists(file.path(dir_output_final, "b.txt")))
      expect_true(file.exists(file.path(dir_output_final, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_output_final, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_output_final, "dir_c")))
      expect_false(file.exists(file.path(dir_output_final, "dir_d")))

      .projr_build_copy_output_direct(output_run = FALSE)

      expect_false(.projr_build_output(output_run = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_build_copy_pkg works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

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
      projr_init()
      yml_projr_init <- .projr_yml_get()

      # don't build
      # ---------------------
      expect_false(.projr_build_copy_pkg())
      yml_projr <- yml_projr_init
      yml_projr[["build-output"]][["copy-to-output"]] <- list()
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_copy_pkg())
      .projr_yml_set(yml_projr_init)

      # build
      # ----------------------
      yml_projr[["build-output"]][["copy-to-output"]] <- list(package = TRUE)
      .projr_yml_set(yml_projr)
      expect_true(.projr_build_copy_pkg(TRUE))
      expect_true(file.exists("_output/report_0.0.0-1.tar.gz"))
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_build_copy_dir works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

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
      projr_init()
      yml_projr_init <- .projr_yml_get()
      invisible({
        file.create(
          projr_path_get("data-raw", "a.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("data-raw", "b.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("data-raw", "dir_c", "c.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("data-raw", "dir_d", "d.txt", output_safe = TRUE)
        )
      })
      invisible({
        file.create(
          projr_path_get("cache", "a.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("cache", "b.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("cache", "dir_c", "c.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("cache", "dir_d", "d.txt", output_safe = TRUE)
        )
      })
      invisible({
        file.create(
          projr_path_get("bookdown", "a.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("bookdown", "b.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("bookdown", "dir_c", "c.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get("bookdown", "dir_d", "d.txt", output_safe = TRUE)
        )
        file.create(
          projr_path_get(
            "bookdown",
            paste0(projr_name_get(), "V", projr_version_get()),
            "c.txt",
            output_safe = TRUE
          )
        )
        file.create(
          projr_path_get("bookdown", "dir_d", "d.txt", output_safe = TRUE)
        )
      })

      # check that nothing is copied across when FALSE
      # -------------------

      yml_projr <- yml_projr_init
      yml_projr[["build-output"]][["copy-to-output"]] <- list(
        `data-raw` = FALSE
      )
      .projr_yml_set(yml_projr)
      expect_true(.projr_build_copy_dir())
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", output_safe = FALSE)
      ))
      .projr_yml_set(yml_projr_init)

      # check that they're copied across correctly when true
      # -------------------
      yml_projr[["build-output"]][["copy-to-output"]] <- list(
        `data-raw` = TRUE,
        cache = TRUE,
        bookdown = TRUE
      )
      .projr_yml_set(yml_projr)
      expect_true(.projr_build_copy_dir(output_run = TRUE))
      expect_true(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "cache.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", output_safe = TRUE)
      ))
      unlink(projr_dir_get("output", output_safe = TRUE), recursive = TRUE)
      unlink(projr_dir_get("output", output_safe = FALSE), recursive = TRUE)
      expect_true(.projr_build_copy_dir(output_run = FALSE))
      expect_true(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = TRUE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = TRUE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "cache.zip", output_safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", output_safe = FALSE)
      ))
      if (FALSE) {
        file.copy(
          projr_path_get("output", "data-raw.zip", output_safe = TRUE),
          file.path(Sys.getenv("HOME"), "data-raw.zip")
        )
        file.copy(
          projr_path_get("output", "bookdown.zip", output_safe = TRUE),
          file.path(Sys.getenv("HOME"), "bookdown.zip")
        )
      }

      # check that they're copied across correctly when version-dependent
      # -------------------
      unlink(projr_dir_get("output", output_safe = TRUE), recursive = TRUE)
      unlink(projr_dir_get("output", output_safe = FALSE), recursive = TRUE)
      yml_projr[["build-output"]][["copy-to-output"]] <- list(
        `data-raw` = TRUE,
        cache = "patch",
        bookdown = "minor"
      )
      .projr_yml_set(yml_projr)
      expect_true(
        .projr_build_copy_dir(output_run = TRUE, bump_component = "patch")
      )
      expect_true(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "cache.zip", output_safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", output_safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "bookdown.zip", output_safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", output_safe = TRUE)
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_build_archive works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

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
      projr_init()
      yml_projr_init <- .projr_yml_get()
      # do nothing when not output
      expect_false(.projr_build_archive(output_run = FALSE))
      version_run_on_list <- .projr_version_run_onwards_get(
        bump_component = "patch"
      )
      # do nothing when nothing saved
      expect_false(.projr_build_archive(
        output_run = FALSE,
        version_run_on_list = version_run_on_list
      ))

      # save files to output
      invisible({
        file.create(
          projr_path_get("output", "a.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "b.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "dir_c", "c.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "dir_d", "d.txt", output_safe = FALSE)
        )
      })

      .projr_build_archive(
        output_run = TRUE,
        version_run_on_list = version_run_on_list
      )

      dir_archive <- projr_dir_get(
        label = "archive",
        paste0("v", version_run_on_list$desc[["success"]])
      )
      expect_true(file.exists(file.path(dir_archive, "a.txt")))
      expect_true(file.exists(file.path(dir_archive, "b.txt")))
      expect_true(file.exists(file.path(dir_archive, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_archive, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_archive, "dir_c")))
      expect_false(dir.exists(file.path(dir_archive, "dir_d")))
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})
