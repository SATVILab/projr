test_that("projr_build_dev works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

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
      expect_identical(basename(yml_bd$output_dir), "docs")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".projr_build_clear_pre and _post works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

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
      # pre
      # ------------------------
      path_safe <- projr_path_get_dir("output", "a", safe = TRUE)
      path_output_final <- projr_path_get_dir("output", "a", safe = FALSE)
      path_docs <- projr_path_get_dir("docs", "b")
      path_data <- projr_path_get_dir("project", "data", "c")
      .projr_build_clear_pre(FALSE)
      expect_false(dir.exists(path_safe))
      expect_true(dir.exists(path_output_final))
      expect_false(dir.exists(path_docs))
      expect_true(dir.exists(path_data))

      # post
      # ------------------------
      # cache
      path_dir <- projr_path_get_dir("cache", "projr")
      .projr_build_clear_post(FALSE)
      expect_true(dir.exists(path_dir))
      .projr_build_clear_post(TRUE)
      expect_true(dir.exists(path_dir))
      # cache
      path_safe <- projr_path_get_dir("output", "a", safe = TRUE)
      path_output_final <- projr_path_get_dir("output", "a", safe = FALSE)
      .projr_build_clear_post(TRUE)
      expect_true(dir.exists(path_safe))
      expect_false(dir.exists(path_output_final))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".projr_build_copy_to_unsafe works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

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
      browser()
      projr_init()
      yml_projr_init <- .projr_yml_get_root_full()
      yml_bd_init <- .projr_yml_bd_get()
      # run when there are no files in dir_output
      debugonce(.projr_build_copy_to_unsafe)
      debugonce(.dir_copy_tree)
      expect_true(.projr_build_copy_to_unsafe(output_run = TRUE))
      invisible({
        file.create(
          projr_path_get("output", "a.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("output", "b.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("output", "dir_c", "c.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("output", "dir_d", "d.txt", safe = TRUE)
        )
      })

      # test that files are not zipped in safe directory
      # and directories are zipped
      # -------------------------------------
      expect_true(.projr_build_copy_to_unsafe(output_run = FALSE))
      dir_safe <- projr_path_get("output", safe = TRUE)
      expect_true(file.exists(file.path(dir_safe, "a.txt")))
      expect_true(file.exists(file.path(dir_safe, "b.txt")))
      expect_true(file.exists(file.path(dir_safe, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_safe, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_safe, "dir_c")))
      expect_false(file.exists(file.path(dir_safe, "dir_d")))

      # test that files are coped over to output directory
      # and directories are zipped
      # -------------------------------------
      browser()
      expect_true(.projr_build_copy_to_unsafe(output_run = TRUE))
      expect_false(file.exists(file.path(dir_safe, "a.txt")))
      expect_false(file.exists(file.path(dir_safe, "b.txt")))
      expect_false(file.exists(file.path(dir_safe, "dir_c.zip")))
      expect_false(file.exists(file.path(dir_safe, "dir_d.zip")))
      dir_output_final <- projr_path_get("output", safe = FALSE)
      expect_true(file.exists(file.path(dir_output_final, "a.txt")))
      expect_true(file.exists(file.path(dir_output_final, "b.txt")))
      expect_true(file.exists(file.path(dir_output_final, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_output_final, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_output_final, "dir_c")))
      expect_false(file.exists(file.path(dir_output_final, "dir_d")))

      .projr_build_copy_to_unsafe(output_run = FALSE)

      expect_false(.projr_build_copy(output_run = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_copy_pkg works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

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
      yml_projr_init <- .projr_yml_get_root_full()

      # don't build
      # ---------------------
      yml_projr_error <- yml_projr_init
      yml_projr_error[["build"]][["package"]] <- FALSE
      .projr_yml_set(yml_projr_error)
      expect_false(.projr_build_copy_pkg(TRUE))
      .projr_yml_set(yml_projr_init)

      # errors
      # ---------------------
      # numeric value
      yml_projr_error <- yml_projr_init
      yml_projr_error[["build"]][["package"]] <- 1
      .projr_yml_set(yml_projr_error)
      expect_error(.projr_build_copy_pkg(TRUE))
      .projr_yml_set(yml_projr_init)
      # double-logical
      yml_projr_error <- yml_projr_init
      yml_projr_error[["build"]][["package"]] <- list(TRUE, TRUE)
      .projr_yml_set(yml_projr_error)
      expect_error(.projr_build_copy_pkg(TRUE))
      .projr_yml_set(yml_projr_init)
      # directory missing
      yml_projr_error <- yml_projr_init
      yml_projr_error[["build"]][["package"]] <- "abc"
      .projr_yml_set(yml_projr_error)
      expect_error(.projr_build_copy_pkg(TRUE))
      .projr_yml_set(yml_projr_init)
      # directory invalid (doesn't begin with output, basically)
      yml_projr_error <- yml_projr_init
      yml_projr_error[["build"]][["package"]] <- "data-raw"
      .projr_yml_set(yml_projr_error)
      expect_error(.projr_build_copy_pkg(TRUE))
      .projr_yml_set(yml_projr_init)

      # build
      # ----------------------
      # package: TRUE
      yml_projr_run <- yml_projr_init
      yml_projr_run[["build"]][["package"]] <- TRUE
      .projr_yml_set(yml_projr_run)
      expect_true(.projr_build_copy_pkg(TRUE))
      expect_true(file.exists("_output/report_0.0.0-1.tar.gz"))
      .projr_yml_set(yml_projr_init)
      # package: character
      yml_projr_run[["build"]][["package"]] <- c("outputPkg", "outputPkg2")
      yml_projr_run[["directories"]][["outputPkg"]] <-
        list(path = "_outputPkg")
      yml_projr_run[["directories"]][["outputPkg2"]] <-
        list(path = "_outputPkg2")
      .projr_yml_set(yml_projr_run)
      dir.create("_outputPkg")
      file.create("_outputPkg/report_0.0.0-1.tar.gz")
      expect_true(.projr_build_copy_pkg(TRUE))
      expect_true(file.exists("_outputPkg/report_0.0.0-1.tar.gz"))
      expect_true(file.exists("_outputPkg2/report_0.0.0-1.tar.gz"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_copy_dir works when outputting", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

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
      yml_projr_init <- .projr_yml_get_root_full()
      invisible({
        file.create(
          projr_path_get("data-raw", "a.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("data-raw", "b.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("data-raw", "dir_c", "c.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("data-raw", "dir_d", "d.txt", safe = TRUE)
        )
      })
      invisible({
        file.create(
          projr_path_get("cache", "a.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("cache", "b.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("cache", "dir_c", "c.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("cache", "dir_d", "d.txt", safe = TRUE)
        )
      })
      invisible({
        file.create(
          projr_path_get("docs", "a.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("docs", "b.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("docs", "dir_c", "c.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("docs", "dir_d", "d.txt", safe = TRUE)
        )
        file.create(
          projr_path_get(
            "docs",
            paste0(projr_name_get(), "V", projr_version_get()),
            "c.txt",
            safe = TRUE
          )
        )
        file.create(
          projr_path_get("docs", "dir_d", "d.txt", safe = TRUE)
        )
      })

      # check that nothing is copied across when FALSE
      # -------------------

      unlink(projr_dir_get("output", safe = TRUE), recursive = TRUE)
      unlink(projr_dir_get("output", safe = FALSE), recursive = TRUE)

      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["data-raw"]] <- list(
        path = "_data_raw", output = FALSE
      )
      yml_projr[["directories"]][["docs"]] <- list(
        path = "docs", output = FALSE
      )
      yml_projr[["directories"]][["cache"]] <- list(
        path = "_tmp", output = FALSE
      )
      .projr_yml_set(yml_projr)
      expect_true(.projr_build_copy_dir(output_run = TRUE))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "docs.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", safe = FALSE)
      ))
      .projr_yml_set(yml_projr_init)

      # check that they're copied across correctly when true
      # -------------------

      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["data-raw"]] <- list(
        path = "_data_raw", output = TRUE
      )
      yml_projr[["directories"]][["docs"]] <- list(
        path = "docs", output = TRUE
      )
      yml_projr[["directories"]][["cache"]] <- list(
        path = "_tmp", output = TRUE
      )
      .projr_yml_set(yml_projr)
      projr:::.projr_build_copy_dir(output_run = TRUE)
      expect_true(.projr_build_copy_dir(output_run = TRUE))
      expect_true(file.exists(
        projr_path_get("output", "data-raw.zip", safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "docs.zip", safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "cache.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "docs.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", safe = TRUE)
      ))
      unlink(projr_dir_get("output", safe = TRUE), recursive = TRUE)
      unlink(projr_dir_get("output", safe = FALSE), recursive = TRUE)
      expect_true(.projr_build_copy_dir(output_run = FALSE))
      expect_true(file.exists(
        projr_path_get("output", "data-raw.zip", safe = TRUE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "docs.zip", safe = TRUE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "cache.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "docs.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", safe = FALSE)
      ))


      # check that they're copied across correctly when
      # to different folders
      # -------------------

      unlink(projr_dir_get("output", safe = TRUE), recursive = TRUE)
      unlink(projr_dir_get("output", safe = FALSE), recursive = TRUE)
      yml_projr[["directories"]] <- list(
        `data-raw` = list("path" = "_data_raw", output = "output"),
        cache = list("path" = "_tmp", output = c("output", "output2")),
        docs = list("path" = "docs", output = FALSE),
        output = list("path" = "_output"),
        output2 = list("path" = "_output2"),
        archive = list("path" = "_archive")
      )
      .projr_yml_set(yml_projr)
      expect_true(.projr_build_copy_dir(output_run = TRUE))
      expect_true(file.exists(
        projr_path_get("output", "data-raw.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "docs.zip", safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "cache.zip", safe = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output2", "cache.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "data-raw.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "docs.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", safe = TRUE)
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_frontmatter_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
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
      .projr_init_description(dir_test, nm_list)
      # no frontmatter
      writeLines(c("# Introduction", "abc"), con = "test.qmd")
      expect_identical(.projr_build_frontmatter_get("test.qmd"), list())
      writeLines(
        c(
          "---",
          "title: abc",
          "---",
          "# Introduction", "abc"
        ),
        con = "test.qmd"
      )
      expect_identical(
        .projr_build_frontmatter_get("test.qmd"),
        list(title = "abc")
      )
      writeLines(
        c(
          "---",
          "title: abc",
          "engine: knitr",
          "format:",
          "  pdf: default",
          "  docx: default",
          "---",
          "# Introduction", "abc"
        ),
        con = "test.qmd"
      )
      expect_identical(
        .projr_build_frontmatter_get("test.qmd"),
        list(
          title = "abc",
          engine = "knitr",
          format = list(
            pdf = "default",
            docx = "default"
          )
        )
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_build_copy_docs_quarto_format_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
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
      .projr_init_description(dir_test, nm_list)
      expect_identical(
        .projr_build_copy_docs_quarto_format_get(list()),
        "html"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_format_get(list("title" = "abc")),
        "html"
      )
      yml_frontmatter <- list(
        title = "abc",
        engine = "knitr",
        format = list(
          pdf = "default",
          docx = "default"
        )
      )
      expect_identical(
        .projr_build_copy_docs_quarto_format_get(yml_frontmatter),
        "pdf"
      )
      yml_frontmatter <- list(
        title = "abc",
        engine = "knitr",
        format = "pdf"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_format_get(yml_frontmatter),
        "pdf"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".projr_build_copy_docs_quarto_fn_prefix/suffix/path_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # prefix
      expect_identical(
        .projr_build_copy_docs_quarto_fn_prefix_get(list(), "test.qmd"),
        "test"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_fn_prefix_get(
          list(title = "abc"), "test.qmd"
        ),
        "test"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_fn_prefix_get(
          list(title = "abc", `output-file` = "def"),
          "test.qmd"
        ),
        "def"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_fn_prefix_get(
          list(title = "abc", `output-file` = "def"),
          "test.qmd"
        ),
        "def"
      )
      # suffix
      expect_identical(
        .projr_build_copy_docs_quarto_fn_suffix_get("html"), "html"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_fn_suffix_get("revealjs"), "html"
      )
      expect_identical(
        .projr_build_copy_docs_quarto_fn_suffix_get("beamer"), "pdf"
      )
      # paths
      expect_identical(
        .projr_build_copy_docs_quarto_path_get("html", "abc"),
        c("abc_files", "abc.html")
      )
      expect_identical(
        .projr_build_copy_docs_quarto_path_get("revealjs", "abc"),
        c("abc_files", "abc.html")
      )
      expect_identical(
        .projr_build_copy_docs_quarto_path_get("pdf", "def"),
        "def.pdf"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".projr_build_copy_docs_quarto_format_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
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
      .projr_init_description(dir_test, nm_list)
      writeLines(c("# Introduction", "abc"), con = "test.qmd")
      invisible(file.create("test.html"))
      dir.create("test_files")
      invisible(file.create("test_files/abc.txt"))
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      unlink(dir_docs, recursive = TRUE)
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      # invisible(file.create(file.path(dir_docs, "test.html")))
      .projr_build_copy_docs_quarto(FALSE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_true(file.exists(file.path(dir_docs, "test_files/abc.txt")))
      # safe output
      dir_docs <- projr_path_get_dir("docs", safe = FALSE)
      invisible(file.create("test.html"))
      dir.create("test_files")
      invisible(file.create("test_files/abc.txt"))
      .projr_build_copy_docs_quarto(TRUE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_true(file.exists(file.path(dir_docs, "test_files/abc.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".projr_build_copy_docs_rmd_format_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
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
  # nm_list <- list(
  #   engine = "quarto_document",
  #   format = "book",
  #   pkg = "testProjr2",
  #   gh = "MiguelRodo",
  #   first = "Tarzan",
  #   last = "Climber",
  #   email = "fruit@palm_tree.am.zn",
  #   title = "Urgh",
  #   filename = "test"
  # )
  # .projr_init_description(dir_test, nm_list)

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
      .projr_init_description(dir_test, nm_list)
      expect_identical(
        .projr_build_copy_docs_rmd_format_get(list()),
        "html_document"
      )
      expect_identical(
        .projr_build_copy_docs_rmd_format_get(list("title" = "abc")),
        "html_document"
      )
      yml_frontmatter <- list(
        title = "abc",
        output = list(
          pdf_document = "default",
          word_document = "default"
        )
      )
      expect_identical(
        .projr_build_copy_docs_rmd_format_get(yml_frontmatter),
        "pdf_document"
      )
      yml_frontmatter <- list(
        title = "abc",
        engine = "knitr",
        output = "pdf_document"
      )
      expect_identical(
        .projr_build_copy_docs_rmd_format_get(yml_frontmatter),
        "pdf_document"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_build_copy_docs_rmd_fn_prefix/suffix/path_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)

  withr::defer(unlink(dir_test, recursive = TRUE))
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
      .projr_init_description(dir_test, nm_list)
      # prefix
      expect_identical(
        .projr_build_copy_docs_rmd_fn_prefix_get("test.Rmd"),
        "test"
      )
      expect_identical(
        .projr_build_copy_docs_rmd_fn_prefix_get(
          "test.Rmd"
        ),
        "test"
      )

      # suffix
      expect_identical(
        .projr_build_copy_docs_rmd_fn_suffix_get("html"), "html"
      )
      expect_identical(
        .projr_build_copy_docs_rmd_fn_suffix_get("html_document"), "html"
      )
      expect_identical(
        .projr_build_copy_docs_rmd_fn_suffix_get("tufte::tufte_handout"), "pdf"
      )

      # paths
      expect_identical(
        .projr_build_copy_docs_rmd_path_get("html", "abc"),
        "abc.html"
      )
      expect_identical(
        .projr_build_copy_docs_rmd_path_get("pdf", "abc"),
        "abc.pdf"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_build_copy_docs_rmd_format_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
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
      .projr_init_description(dir_test, nm_list)
      writeLines(c("# Introduction", "abc"), con = "test.Rmd")
      invisible(file.create("test.html"))
      dir.create("test_files")
      invisible(file.create("test_files/abc.txt"))
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      invisible(file.create(file.path(dir_docs, "test.html")))
      .projr_build_copy_docs_rmd(FALSE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_false(file.exists(file.path(dir_docs, "test_files/abc.txt")))
      dir_docs <- projr_path_get_dir("docs", safe = FALSE)
      invisible(file.create(file.path(dir_docs, "test.html")))
      .projr_build_copy_docs_rmd(TRUE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_false(file.exists(file.path(dir_docs, "test_files/abc.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
})
