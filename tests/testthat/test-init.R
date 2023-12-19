test_that("projr_init works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(projr_init())
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("_projr.yml"))
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("index.Rmd"))
      expect_true(dir.exists("R"))
      projr_init()
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("_projr.yml"))
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("index.Rmd"))
      expect_true(dir.exists("R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_yml works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.projr_init_yml())
      expect_true(file.exists("_projr.yml"))
      invisible(file.remove("_projr.yml"))
      path_yml <- file.path(tempdir(), "_projr.yml")
      yaml::write_yaml(list("abc" = 1), path_yml)
      Sys.setenv("PROJR_PATH_YML" = path_yml)
      expect_true(.projr_init_yml(NULL))
      expect_true(file.exists("_projr.yml"))
      expect_identical(yaml::read_yaml("_projr.yml"), list("abc" = 1))
      invisible(file.remove("_projr.yml"))

      path_yml_2 <- file.path(tempdir(), "_projr2.yml")
      yaml::write_yaml(list("abc" = 2), path_yml_2)
      expect_true(.projr_init_yml(path_yml_2))
      expect_true(file.exists("_projr.yml"))
      expect_identical(yaml::read_yaml("_projr.yml"), list("abc" = 2))
      invisible(file.remove("_projr.yml"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_description works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh"
      )
      expect_true(.projr_init_description(nm_list))
      expect_true(file.exists("DESCRIPTION"))
      desc <- read.dcf("DESCRIPTION")
      expect_identical(desc[[1]], "testProjr2")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_dep works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh"
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_dep())
      expect_true(file.exists("_dependencies.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_ignore works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh"
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_ignore())
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_renv works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh"
      )
      .projr_init_description(nm_list)
      file.create(file.path(dir_test, ".Rprofile"))
      writeLines('source("renv/activate.R")', file.path(dir_test, ".Rprofile"))
      expect_true(.projr_init_renv(FALSE, FALSE))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_license works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh"
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_license(nm_list))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_init_readme works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh"
      )
      .projr_init_description(nm_list)
      .projr_init_readme(nm_list)
      expect_true(file.exists("README.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_bookdown works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  file.create(file.path(dir_test, "_dependencies.R"))

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "bookdown",
        format = NULL,
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh",
        filename = NULL
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_engine_bookdown(nm_list))
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("index.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_quarto_project works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  file.create(file.path(dir_test, "_dependencies.R"))

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "quarto_project",
        format = "book",
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh",
        filename = NULL
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_engine_quarto_project(nm_list))
      expect_true(file.exists("_quarto.yml"))
      expect_true(file.exists("index.qmd"))
      invisible(file.remove("_quarto.yml"))
      invisible(file.remove("index.qmd"))
      nm_list[["format"]] <- "website"
      .projr_init_description(nm_list)
      expect_true(.projr_init_engine_quarto_project(nm_list))
      expect_true(file.exists("_quarto.yml"))
      expect_true(file.exists("index.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_quarto_document works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  file.create(file.path(dir_test, "_dependencies.R"))

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "quarto_document",
        format = "book",
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh",
        filename = "test"
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_engine_quarto_document(nm_list))
      expect_true(!file.exists("_quarto.yml"))
      expect_true(file.exists("test.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_rmd works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .projr_dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  file.create(file.path(dir_test, "_dependencies.R"))

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "quarto_document",
        format = "book",
        pkg = "testProjr2",
        gh = "MiguelRodo",
        first = "Tarzan",
        last = "Climber",
        email = "fruit@palm_tree.am.zn",
        title = "Urgh",
        filename = "abc"
      )
      .projr_init_description(nm_list)
      expect_true(.projr_init_engine_rmd(nm_list))
      expect_true(!file.exists("_bookdown.yml"))
      expect_true(file.exists("abc.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
