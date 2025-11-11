# clearing
# ------------------------E

test_that(".build_clear_pre and _post works", {
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
      .init_full()
      # pre
      # ------------------------
      path_safe <- projr_path_get_dir("output", "a", safe = TRUE)
      path_output_final <- projr_path_get_dir("output", "a", safe = FALSE)
      path_docs <- projr_path_get_dir("docs", "b")
      path_data <- projr_path_get_dir("project", "data", "c")
      .build_clear_pre(output_run = TRUE, clear_output = "pre")
      # After clearing with clear_output="pre", both safe and unsafe output dirs should exist but be empty
      expect_true(dir.exists(path_safe))
      expect_true(dir.exists(path_output_final))
      # Docs directory should still exist (not cleared by build_clear_pre)
      expect_true(dir.exists(path_docs))
      # Data directory should still exist (not an output directory)
      expect_true(dir.exists(path_data))

      # post
      # ------------------------
      # cache
      path_dir <- projr_path_get_dir("cache", "projr")
      .build_clear_post(output_run = FALSE, clear_output = "never")
      expect_true(dir.exists(path_dir))
      .build_clear_post(output_run = TRUE, clear_output = "never")
      expect_true(dir.exists(path_dir))
      # cache
      path_safe <- projr_path_get_dir("output", "a", safe = TRUE)
      path_output_final <- projr_path_get_dir("output", "a", safe = FALSE)
      .build_clear_post(output_run = TRUE, clear_output = "post")
      expect_true(dir.exists(path_safe))
      expect_false(dir.exists(path_output_final))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_to_unsafe works", {
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
      .init()
      yml_projr_init <- .yml_get_default_raw()
      yml_bd_init <- .yml_bd_get()
      # run when there are no files in dir_output
      expect_true(.build_copy_to_unsafe(output_run = TRUE))
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
      expect_false(.build_copy_to_unsafe(output_run = FALSE))
      dir_safe <- projr_path_get("output", safe = TRUE)
      expect_true(file.exists(file.path(dir_safe, "a.txt")))
      expect_true(file.exists(file.path(dir_safe, "b.txt")))
      expect_true(dir.exists(file.path(dir_safe, "dir_c")))
      expect_true(dir.exists(file.path(dir_safe, "dir_d")))

      # test that files are coped over to output directory
      # and directories are zipped
      # -------------------------------------
      expect_true(.build_copy_to_unsafe(output_run = TRUE))
      expect_false(file.exists(file.path(dir_safe, "a.txt")))
      expect_false(file.exists(file.path(dir_safe, "b.txt")))
      expect_false(file.exists(file.path(dir_safe, "dir_c.zip")))
      expect_false(file.exists(file.path(dir_safe, "dir_d.zip")))
      dir_output_final <- projr_path_get("output", safe = FALSE)
      expect_true(file.exists(file.path(dir_output_final, "a.txt")))
      expect_true(file.exists(file.path(dir_output_final, "b.txt")))
      expect_true(dir.exists(file.path(dir_output_final, "dir_c")))
      expect_true(dir.exists(file.path(dir_output_final, "dir_d")))

      .build_copy_to_unsafe(output_run = FALSE)

      expect_true(.build_copy(output_run = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_copy_pkg works", {
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
      .init()
      yml_projr_init <- .yml_get_default_raw()

      # don't build
      # ---------------------
      yml.error <- yml_projr_init
      yml.error[["build"]][["package"]] <- FALSE
      .yml_set(yml.error)
      expect_false(.build_copy_pkg(TRUE))
      .yml_set(yml_projr_init)


      # build
      # ----------------------
      # package: TRUE
      .yml_dir_set_pkg(TRUE, "output", "default")
      # ensure there is something to build the package out of
      x <- "1"
      dir.create("data", showWarnings = FALSE)
      saveRDS(x, "data/x.rds")
      dir.create("inst")
      file.create("inst/f1")
      expect_true(.build_copy_pkg(TRUE))

      expect_true(file.exists("_output/pkg/report_0.0.0-1.tar.gz"))
      .yml_set(yml_projr_init)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_copy_dir works when outputting", {
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
      .init()
      yml_projr_init <- .yml_get_default_raw()
      invisible({
        file.create(
          projr_path_get("raw-data", "a.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("raw-data", "b.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("raw-data", "dir_c", "c.txt", safe = TRUE)
        )
        file.create(
          projr_path_get("raw-data", "dir_d", "d.txt", safe = TRUE)
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
          .path_get(
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

      unlink(projr_path_get_dir("output", safe = TRUE), recursive = TRUE)
      unlink(projr_path_get_dir("output", safe = FALSE), recursive = TRUE)

      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["raw-data"]] <- list(
        path = "_raw_data", output = FALSE
      )
      yml_projr[["directories"]][["docs"]] <- list(
        path = "docs", output = FALSE
      )
      yml_projr[["directories"]][["cache"]] <- list(
        path = "_tmp", output = FALSE
      )
      .yml_set(yml_projr)
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_false(file.exists(
        projr_path_get("output", "raw-data.zip", safe = TRUE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "raw-data.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "docs.zip", safe = FALSE)
      ))
      expect_false(file.exists(
        projr_path_get("output", "cache.zip", safe = FALSE)
      ))
      .yml_set(yml_projr_init)

      # check that they're copied across correctly when true
      # -------------------

      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["raw-data"]] <- list(
        path = "_raw_data", output = TRUE
      )
      yml_projr[["directories"]][["cache"]] <- list(
        path = "_tmp", output = TRUE
      )
      .yml_set(yml_projr)
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_true(dir.exists(
        projr_path_get("output", "raw-data", safe = FALSE)
      ))

      # check that they're copied across correctly when
      # to different folders
      # -------------------
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["raw-data"]] <- list(
        path = "_raw_data", output = TRUE
      )
      yml_projr[["directories"]][["cache"]] <- list(
        path = "_tmp", output = "output2"
      )
      .yml_set(yml_projr)
      .yml_dir_add_label(
        path = "_output2", label = "output2", profile = "default"
      )
      .dir_rm("_output")
      .dir_rm("_output2")
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_true(dir.exists(
        projr_path_get("output", "raw-data", safe = FALSE, create = FALSE)
      ))
      expect_false(dir.exists(
        projr_path_get("output", "cache", safe = FALSE, create = FALSE)
      ))
      expect_true(dir.exists(
        projr_path_get("output2", "raw-data", safe = FALSE, create = FALSE)
      ))
      expect_true(dir.exists(
        projr_path_get("output2", "cache", safe = FALSE, create = FALSE)
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_frontmatter_get works", {
  skip_if(.is_test_select())
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
      .init_description(nm_list)
      # no frontmatter
      writeLines(c("# Introduction", "abc"), con = "test.qmd")
      expect_identical(.build_frontmatter_get("test.qmd"), list())
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
        .build_frontmatter_get("test.qmd"),
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
        .build_frontmatter_get("test.qmd"),
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

test_that(".build_copy_docs_quarto_format_get works", {
  skip_if(.is_test_select())
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
      .init_description(nm_list)
      expect_identical(
        .build_copy_docs_quarto_format_get(list()),
        "html"
      )
      expect_identical(
        .build_copy_docs_quarto_format_get(list("title" = "abc")),
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
        .build_copy_docs_quarto_format_get(yml_frontmatter),
        "pdf"
      )
      yml_frontmatter <- list(
        title = "abc",
        engine = "knitr",
        format = "pdf"
      )
      expect_identical(
        .build_copy_docs_quarto_format_get(yml_frontmatter),
        "pdf"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".build_copy_docs_quarto_fn_prefix/suffix/path_get works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # prefix
      expect_identical(
        .build_copy_docs_quarto_fn_prefix_get(list(), "test.qmd"),
        "test"
      )
      expect_identical(
        .build_copy_docs_quarto_fn_prefix_get(
          list(title = "abc"), "test.qmd"
        ),
        "test"
      )
      expect_identical(
        .build_copy_docs_quarto_fn_prefix_get(
          list(title = "abc", `output-file` = "def"),
          "test.qmd"
        ),
        "def"
      )
      expect_identical(
        .build_copy_docs_quarto_fn_prefix_get(
          list(title = "abc", `output-file` = "def"),
          "test.qmd"
        ),
        "def"
      )
      # suffix
      expect_identical(
        .build_copy_docs_quarto_fn_suffix_get("html"), "html"
      )
      expect_identical(
        .build_copy_docs_quarto_fn_suffix_get("revealjs"), "html"
      )
      expect_identical(
        .build_copy_docs_quarto_fn_suffix_get("beamer"), "pdf"
      )
      # paths
      expect_identical(
        .build_copy_docs_quarto_path_get("html", "abc"),
        c("abc_files", "abc.html")
      )
      expect_identical(
        .build_copy_docs_quarto_path_get("revealjs", "abc"),
        c("abc_files", "abc.html")
      )
      expect_identical(
        .build_copy_docs_quarto_path_get("pdf", "def"),
        "def.pdf"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})


test_that(".build_copy_docs_quarto_format_get works", {
  skip_if(.is_test_select())
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
      .init_description(nm_list)
      writeLines(c("# Introduction", "abc"), con = "test.qmd")
      invisible(file.create("test.html"))
      dir.create("test_files")
      invisible(file.create("test_files/abc.txt"))
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      unlink(dir_docs, recursive = TRUE)
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      # invisible(file.create(file.path(dir_docs, "test.html")))
      .build_copy_docs_quarto(FALSE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_true(file.exists(file.path(dir_docs, "test_files/abc.txt")))
      # safe output
      dir_docs <- projr_path_get_dir("docs", safe = FALSE)
      invisible(file.create("test.html"))
      dir.create("test_files")
      invisible(file.create("test_files/abc.txt"))
      .build_copy_docs_quarto(TRUE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_true(file.exists(file.path(dir_docs, "test_files/abc.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".build_copy_docs_rmd_format_get works", {
  skip_if(.is_test_select())
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
  # .init_description(nm_list)

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
      expect_identical(
        .build_copy_docs_rmd_format_get(list()),
        "html_document"
      )
      expect_identical(
        .build_copy_docs_rmd_format_get(list("title" = "abc")),
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
        .build_copy_docs_rmd_format_get(yml_frontmatter),
        "pdf_document"
      )
      yml_frontmatter <- list(
        title = "abc",
        engine = "knitr",
        output = "pdf_document"
      )
      expect_identical(
        .build_copy_docs_rmd_format_get(yml_frontmatter),
        "pdf_document"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_rmd_fn_prefix/suffix/path_get works", {
  skip_if(.is_test_select())
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
      .init_description(nm_list)
      # prefix
      expect_identical(
        .build_copy_docs_rmd_fn_prefix_get("test.Rmd"),
        "test"
      )
      expect_identical(
        .build_copy_docs_rmd_fn_prefix_get(
          "test.Rmd"
        ),
        "test"
      )

      # suffix
      expect_identical(
        .build_copy_docs_rmd_fn_suffix_get("html"), "html"
      )
      expect_identical(
        .build_copy_docs_rmd_fn_suffix_get("html_document"), "html"
      )
      expect_identical(
        .build_copy_docs_rmd_fn_suffix_get("tufte::tufte_handout"), "pdf"
      )

      # paths
      expect_identical(
        .build_copy_docs_rmd_path_get("html", "abc"),
        "abc.html"
      )
      expect_identical(
        .build_copy_docs_rmd_path_get("pdf", "abc"),
        "abc.pdf"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_rmd_format_get works", {
  skip_if(.is_test_select())
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
      .init_description(nm_list)
      writeLines(c("# Introduction", "abc"), con = "test.Rmd")
      invisible(file.create("test.html"))
      dir.create("test_files")
      invisible(file.create("test_files/abc.txt"))
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      invisible(file.create(file.path(dir_docs, "test.html")))
      .build_copy_docs_rmd(FALSE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_false(file.exists(file.path(dir_docs, "test_files/abc.txt")))
      dir_docs <- projr_path_get_dir("docs", safe = FALSE)
      invisible(file.create(file.path(dir_docs, "test.html")))
      .build_copy_docs_rmd(TRUE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_false(file.exists(file.path(dir_docs, "test_files/abc.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      version_run_on_list <- .version_run_onwards_get("patch")

      # bookdowwn
      .build_engine(
        file = NULL,
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )
      expect_true(file.exists("docs/reportV0.0.0-1/index.html"))
      # now rmarkdown only
      .file_rm("_bookdown.yml")
      .file_rm("_output.yml")
      .build_engine(
        file = NULL,
        version_run_on_list = version_run_on_list,
        args_engine = list()
      )
      expect_true(file.exists("index.html"))
      # now test quarto files
      .file_rm("index.Rmd")
      .file_rm("index.html")
      file.create("quarto.qmd") |> invisible()
      writeLines(
        c("---", "title: quarto", "---", "", "## Quarto", "", "abc", ""),
        con = "quarto.qmd"
      )
      .build_engine(
        file = NULL, version_run_on_list = version_run_on_list, args_engine = list()
      )
      expect_true(file.exists("quarto.html"))
      # now test quarto project
      .file_rm("quarto.html")
      file.create("quarto.yml") |> invisible()
      writeLines(
        c("project", "project:", '  title: "quarto_project"', ""),
        con = "quarto.yml"
      )
      .build_engine(
        file = NULL, version_run_on_list = version_run_on_list, args_engine = list()
      )
      expect_true(dir.exists("docs/reportV0.0.0-1"))
    }
  )
})

test_that("CHANGELOG.md is excluded from docs copying", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("test_projr_changelog"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple test scenario
      source_dir <- file.path(dir_test, "source")
      dest_dir <- file.path(dir_test, "dest")
      dir.create(source_dir)
      
      # Create some files including CHANGELOG.md
      writeLines("Test content", file.path(source_dir, "test.html"))
      writeLines("# CHANGELOG\n\n- Test entry", file.path(source_dir, "CHANGELOG.md"))
      writeLines("Other content", file.path(source_dir, "other.txt"))
      
      # Test the dir_move_exact function with CHANGELOG.md exclusion
      .dir_move_exact(source_dir, dest_dir, dir_exc = "CHANGELOG.md")
      
      # Verify that CHANGELOG.md was excluded but other files were copied
      expect_true(file.exists(file.path(dest_dir, "test.html")))
      expect_true(file.exists(file.path(dest_dir, "other.txt")))
      expect_false(file.exists(file.path(dest_dir, "CHANGELOG.md")))
    },
    force = TRUE,
    quiet = TRUE
  )
})
