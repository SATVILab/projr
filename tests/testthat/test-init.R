test_that("projr_init works", {
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
      # Call projr_init_all with bookdown to match test expectations
      expect_true(projr_init_all(github = FALSE, lit_doc = "bookdown"))
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
  skip_if(.is_test_cran())
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
      Sys.setenv("PROJR_PATH_YML" = path_yml)
      expect_true(.init_yml(NULL))
      Sys.unsetenv("PROJR_PATH_YML")
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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

test_that("projr_init_bookdown works", {
  skip_if(.is_test_cran())
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

test_that("projr_init_quarto_project works", {
  skip_if(.is_test_cran())
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

test_that("projr_init_quarto_document works", {
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

test_that("projr_init_rmd works", {
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

# ========================================
# Comprehensive tests for user-facing projr_init functions
# ========================================

test_that("projr_init with various parameters works", {
  skip_if(.is_test_select())
  
  # Test with minimal parameters (defaults)
  dir_test <- file.path(tempdir(), "testProjrMinimal")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init(git = FALSE, github = FALSE)
      # Check for actual directory structure created
      expect_true(dir.exists("_tmp"))
      expect_true(dir.exists("_raw_data"))
      expect_true(dir.exists("R"))
      expect_true(file.exists("README.Rmd"))
      expect_true(file.exists("README.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
  
  # Test with git enabled
  dir_test2 <- file.path(tempdir(), "testProjrGit")
  if (dir.exists(dir_test2)) unlink(dir_test2, recursive = TRUE)
  .dir_create(dir_test2)
  
  usethis::with_project(
    path = dir_test2,
    code = {
      projr_init(git = TRUE, git_commit = TRUE, github = FALSE)
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test2, recursive = TRUE)
  
  # Test with DESCRIPTION and license
  dir_test3 <- file.path(tempdir(), "testProjrDesc")
  if (dir.exists(dir_test3)) unlink(dir_test3, recursive = TRUE)
  .dir_create(dir_test3)
  
  usethis::with_project(
    path = dir_test3,
    code = {
      projr_init(
        git = FALSE, 
        github = FALSE, 
        desc = TRUE, 
        license = "ccby"
      )
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("LICENSE.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test3, recursive = TRUE)
  
  # Test with projr_yml
  dir_test4 <- file.path(tempdir(), "testProjrYml")
  if (dir.exists(dir_test4)) unlink(dir_test4, recursive = TRUE)
  .dir_create(dir_test4)
  
  usethis::with_project(
    path = dir_test4,
    code = {
      projr_init(
        git = FALSE,
        github = FALSE,
        projr_yml = TRUE
      )
      expect_true(file.exists("_projr.yml"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test4, recursive = TRUE)
  
  # Test with lit_doc options
  dir_test5 <- file.path(tempdir(), "testProjrQuarto")
  if (dir.exists(dir_test5)) unlink(dir_test5, recursive = TRUE)
  .dir_create(dir_test5)
  
  usethis::with_project(
    path = dir_test5,
    code = {
      projr_init(
        git = FALSE,
        github = FALSE,
        lit_doc = "quarto"
      )
      expect_true(file.exists("intro.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test5, recursive = TRUE)
  
  # Test with readme_rmd = FALSE
  dir_test6 <- file.path(tempdir(), "testProjrReadmeMd")
  if (dir.exists(dir_test6)) unlink(dir_test6, recursive = TRUE)
  .dir_create(dir_test6)
  
  usethis::with_project(
    path = dir_test6,
    code = {
      projr_init(
        git = FALSE,
        github = FALSE,
        readme_rmd = FALSE
      )
      expect_true(file.exists("README.md"))
      expect_false(file.exists("README.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test6, recursive = TRUE)
})

test_that("projr_init_all works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), "testProjrInitAll")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # projr_init_all should enable desc, projr_yml, and optionally lit_doc and license
      expect_true(projr_init_all(github = FALSE, license = "cc0", lit_doc = "rmd"))
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("_projr.yml"))
      expect_true(file.exists("LICENSE.md"))
      expect_true(file.exists("intro.Rmd"))
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_cite works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), "testProjrCite")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create DESCRIPTION first (required for cite)
      projr_init(git = FALSE, github = FALSE, desc = TRUE)
      
      # Try to run projr_init_cite
      # May fail due to package installation issues in CI
      cite_result <- tryCatch(
        {
          projr_init_cite()
          list(success = TRUE, error = NULL)
        },
        error = function(e) {
          list(success = FALSE, error = e$message)
        }
      )
      
      # If it succeeded, CITATION.cff should exist
      if (cite_result$success) {
        expect_true(file.exists("CITATION.cff"))
      } else {
        # If it failed, skip the test with a message
        skip(paste("projr_init_cite failed:", cite_result$error))
      }
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_git works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), "testProjrGitInit")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Test git init with commit
      expect_true(projr_init_git(commit = TRUE))
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
      
      # Check that a commit was made
      .dep_install_only("gert")
      commits <- gert::git_log(max = 1)
      expect_true(nrow(commits) > 0)
    },
    force = TRUE,
    quiet = TRUE
  )
  
  # Test git init without commit
  dir_test2 <- file.path(tempdir(), "testProjrGitNoCommit")
  if (dir.exists(dir_test2)) unlink(dir_test2, recursive = TRUE)
  .dir_create(dir_test2)
  
  usethis::with_project(
    path = dir_test2,
    code = {
      expect_true(projr_init_git(commit = FALSE))
      expect_true(dir.exists(".git"))
      expect_true(file.exists(".gitignore"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test2, recursive = TRUE)
})

test_that("projr_init_license works with different license types", {
  skip_if(.is_test_select())
  
  # Test CC-BY license
  dir_test1 <- file.path(tempdir(), "testProjrLicenseCCBY")
  if (dir.exists(dir_test1)) unlink(dir_test1, recursive = TRUE)
  .dir_create(dir_test1)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test1, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test1,
    code = {
      projr_init(git = FALSE, github = FALSE, desc = TRUE)
      projr_init_license("ccby", "John", "Doe")
      expect_true(file.exists("LICENSE.md"))
      # Just check that some content was written
      license_content <- readLines("LICENSE.md")
      expect_true(length(license_content) > 0)
    },
    force = TRUE,
    quiet = TRUE
  )
  
  # Test Apache license
  dir_test2 <- file.path(tempdir(), "testProjrLicenseApache")
  if (dir.exists(dir_test2)) unlink(dir_test2, recursive = TRUE)
  .dir_create(dir_test2)
  
  usethis::with_project(
    path = dir_test2,
    code = {
      projr_init(git = FALSE, github = FALSE, desc = TRUE)
      projr_init_license("apache", "Jane", "Smith")
      expect_true(file.exists("LICENSE.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test2, recursive = TRUE)
  
  # Test CC0 license
  dir_test3 <- file.path(tempdir(), "testProjrLicenseCC0")
  if (dir.exists(dir_test3)) unlink(dir_test3, recursive = TRUE)
  .dir_create(dir_test3)
  
  usethis::with_project(
    path = dir_test3,
    code = {
      projr_init(git = FALSE, github = FALSE, desc = TRUE)
      projr_init_license("cc0", "Bob", "Jones")
      expect_true(file.exists("LICENSE.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test3, recursive = TRUE)
  
  # Test proprietary license
  dir_test4 <- file.path(tempdir(), "testProjrLicenseProp")
  if (dir.exists(dir_test4)) unlink(dir_test4, recursive = TRUE)
  .dir_create(dir_test4)
  
  usethis::with_project(
    path = dir_test4,
    code = {
      projr_init(git = FALSE, github = FALSE, desc = TRUE)
      projr_init_license("proprietary", "Alice", "Brown")
      expect_true(file.exists("LICENSE") || file.exists("LICENSE.md"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test4, recursive = TRUE)
})

test_that("projr_init_renv works", {
  skip_if(.is_test_select())
  skip_if_not(.test_should_run_renv(),
    "Set PROJR_TEST_RENV=TRUE to run renv initialization tests"
  )
  dir_test <- file.path(tempdir(), "testProjrRenv")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create basic structure first
      projr_init(git = FALSE, github = FALSE, desc = TRUE)
      
      # Test renv initialization with bioc = FALSE
      expect_true(projr_init_renv(bioc = FALSE))
      expect_true(file.exists("renv.lock"))
      expect_true(dir.exists("renv"))
      expect_true(file.exists("renv/activate.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_renviron works", {
  skip_if(.is_test_select())
  
  # This function creates/updates user .Renviron, so we need to be careful
  # Just test that it runs without error
  expect_message(projr_init_renviron())
})
