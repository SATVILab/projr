test_that(.init works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true.init())
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("_projr.yml"))
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("index.Rmd"))
      expect_true(dir.exists("R"))
     .init()
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

test_that(".init_yml works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.init_yml())
      expect_true(file.exists("_projr.yml"))
      invisible(file.remove("_projr.yml"))
      path_yml <- file.path(tempdir(), "_projr.yml")
      yaml::write_yaml(list("abc" = 1), path_yml)
      Sys.setenv(.PATH_YML" = path_yml)
      expect_true(.init_yml(NULL))
      Sys.unsetenv(.PATH_YML")
      expect_true(file.exists("_projr.yml"))
      expect_identical(yaml::read_yaml("_projr.yml"), list("abc" = 1))
      invisible(file.remove("_projr.yml"))

      path_yml_2 <- file.path(tempdir(), "_projr2.yml")
      yaml::write_yaml(list("abc" = 2), path_yml_2)
      expect_true(.init_yml(path_yml_2))
      expect_true(file.exists("_projr.yml"))
      expect_identical(yaml::read_yaml("_projr.yml"), list("abc" = 2))
      invisible(file.remove("_projr.yml"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_description works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      expect_true(.init_description(nm_list))
      expect_true(file.exists("DESCRIPTION"))
      desc <- read.dcf("DESCRIPTION")
      expect_identical(desc[[1]], "testProjr2")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_dep works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_dep())
      expect_true(file.exists("_dependencies.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_ignore works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_ignore())
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_renv works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      file.create(file.path(dir_test, ".Rprofile"))
      writeLines('source("renv/activate.R")', file.path(dir_test, ".Rprofile"))
      expect_true(.init_renv(FALSE, FALSE))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_license works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_license(nm_list))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_readme works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      .init_readme(nm_list)
      expect_true(file.exists("README.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(.init_bookdown works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_engine_bookdown(nm_list))
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("index.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(.init_quarto_project works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_engine_quarto_project(nm_list))
      expect_true(file.exists("_quarto.yml"))
      expect_true(file.exists("index.qmd"))
      invisible(file.remove("_quarto.yml"))
      invisible(file.remove("index.qmd"))
      nm_list[["format"]] <- "website"
      .init_description(nm_list)
      expect_true(.init_engine_quarto_project(nm_list))
      expect_true(file.exists("_quarto.yml"))
      expect_true(file.exists("index.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(.init_quarto_document works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_engine_quarto_document(nm_list))
      expect_true(!file.exists("_quarto.yml"))
      expect_true(file.exists("test.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(.init_rmd works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
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
      .init_description(nm_list)
      expect_true(.init_engine_rmd(nm_list))
      expect_true(!file.exists("_bookdown.yml"))
      expect_true(file.exists("abc.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
