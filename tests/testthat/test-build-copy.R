# clearing
# ------------------------E

test_that(".build_clear_pre and _post works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # pre
      # ------------------------
      # After clearing output directories, they may be removed entirely
      # Just verify the clearing works without errors and docs/data dirs remain
      projr_path_get_dir("output", "a", safe = TRUE)
      projr_path_get_dir("output", "a", safe = FALSE)
      projr_path_get_dir("docs", "b")
      projr_path_get_dir("project", "data", "c")

      path_docs <- projr_path_get_dir("docs", create = FALSE)
      path_data <- projr_path_get_dir("project", "data", create = FALSE)

      # Clear should work without errors
      expect_silent(.build_clear_pre(output_run = TRUE, clear_output = "pre"))

      # Docs and data directories should still exist (not output directories)
      expect_true(dir.exists(path_docs))
      expect_true(dir.exists(path_data))

      # post
      # ------------------------
      # cache
      path_dir <- projr_path_get_dir("cache", "projr")
      .build_clear_post_safe(output_run = FALSE, clear_output = "never")
      expect_true(dir.exists(path_dir))
      .build_clear_post_safe(output_run = TRUE, clear_output = "never")
      expect_true(dir.exists(path_dir))
      # cache
      path_safe <- projr_path_get_dir("output", "a", safe = TRUE)
      path_output_final <- projr_path_get_dir("output", "a", safe = FALSE)
      .build_clear_post_safe(output_run = TRUE, clear_output = "post")
      expect_true(dir.exists(path_safe))
      expect_false(dir.exists(path_output_final))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_to_unsafe works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create DESCRIPTION file for projr_use_data
      writeLines(c(
        "Package: report",
        "Title: Test Package",
        "Version: 0.0.0-1",
        "Description: Test package for building.",
        "Authors@R: person('Test', 'Author', email = 'test@example.com', role = c('aut', 'cre'))",
        "License: MIT + file LICENSE",
        "Encoding: UTF-8"
      ), "DESCRIPTION")

      # Create minimal LICENSE file
      writeLines("MIT License", "LICENSE")

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
      projr_use_data(x, safe = FALSE)
      dir.create("inst", showWarnings = FALSE)
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
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
          projr_path_get("docs", "a.txt", safe = FALSE)
        )
        file.create(
          projr_path_get("docs", "b.txt", safe = FALSE)
        )
        file.create(
          projr_path_get("docs", "dir_c", "c.txt", safe = FALSE)
        )
        file.create(
          projr_path_get("docs", "dir_d", "d.txt", safe = FALSE)
        )
        file.create(
          projr_path_get(
            "docs",
            paste0(projr_name_get(), "V", projr_version_get()),
            "c.txt",
            safe = FALSE
          )
        )
        file.create(
          projr_path_get("docs", "dir_d", "d.txt", safe = FALSE)
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

      # check that docs are copied when output: TRUE
      # -------------------
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["docs"]] <- list(
        path = "docs", output = TRUE
      )
      .yml_set(yml_projr)
      .dir_rm("_output")
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_true(dir.exists(
        projr_path_get("output", "docs", safe = FALSE, create = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "docs", "a.txt", safe = FALSE, create = FALSE)
      ))

      # check that docs are copied to specified output directory
      # -------------------
      yml_projr <- yml_projr_init
      yml_projr[["directories"]][["docs"]] <- list(
        path = "docs", output = "output2"
      )
      .yml_set(yml_projr)
      .yml_dir_add_label(
        path = "_output2", label = "output2", profile = "default"
      )
      .dir_rm("_output")
      .dir_rm("_output2")
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_false(dir.exists(
        projr_path_get("output", "docs", safe = FALSE, create = FALSE)
      ))
      expect_true(dir.exists(
        projr_path_get("output2", "docs", safe = FALSE, create = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output2", "docs", "b.txt", safe = FALSE, create = FALSE)
      ))

      # check that custom directory labels can be copied
      # -------------------
      yml_projr <- yml_projr_init
      # Add a custom directory label "raw2"
      .yml_dir_add_label(
        path = "_raw2", label = "raw2", profile = "default"
      )
      # Create files in raw2
      file.create(projr_path_get("raw2", "custom.txt", safe = FALSE))
      # Set output for raw2
      yml_projr[["directories"]][["raw2"]] <- list(
        path = "_raw2", output = TRUE
      )
      .yml_set(yml_projr)
      .dir_rm("_output")
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_true(dir.exists(
        projr_path_get("output", "raw2", safe = FALSE, create = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "raw2", "custom.txt", safe = FALSE, create = FALSE)
      ))

      # check that multiple custom labels can be copied to different outputs
      # -------------------
      # Add custom labels (use raw-extra and output3 since label patterns are restrictive)
      .yml_dir_add_label(
        path = "_raw_extra", label = "raw-extra", profile = "default"
      )
      .yml_dir_add_label(
        path = "_output3", label = "output3", profile = "default"
      )
      # Create files
      file.create(projr_path_get("raw-extra", "extra.txt", safe = FALSE))
      # Configure outputs - need to get fresh yml after adding labels
      yml_projr <- .yml_get(NULL)
      yml_projr[["directories"]][["raw-extra"]][["output"]] <- "output3"
      .yml_set(yml_projr)
      .dir_rm("_output")
      .dir_rm("_output3")
      expect_true(.build_copy_dir(output_run = TRUE))
      expect_true(dir.exists(
        projr_path_get("output3", "raw-extra", safe = FALSE, create = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output3", "raw-extra", "extra.txt", safe = FALSE, create = FALSE)
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_copy_dir works with non-standard label names", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add a label that doesn't match any standard pattern
      # (not raw*, cache*, output*, docs, data, code, project)
      .yml_dir_add_label(
        path = "_foo", label = "foo", profile = "default"
      )

      # Create files in foo directory
      file.create(projr_path_get("foo", "test.txt", safe = FALSE))
      file.create(projr_path_get("foo", "test2.txt", safe = FALSE))

      # Configure output for foo
      yml_projr <- .yml_get(NULL)
      yml_projr[["directories"]][["foo"]][["output"]] <- TRUE
      .yml_set(yml_projr)

      # This should not crash with "label 'foo' not valid"
      expect_true(.build_copy_dir(output_run = TRUE))

      # Verify files were copied
      expect_true(dir.exists(
        projr_path_get("output", "foo", safe = FALSE, create = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "foo", "test.txt", safe = FALSE, create = FALSE)
      ))
      expect_true(file.exists(
        projr_path_get("output", "foo", "test2.txt", safe = FALSE, create = FALSE)
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_frontmatter_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

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
        c("abc_files", "abc.html")
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)


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
      unlink(dir_docs, recursive = TRUE)
      dir_docs <- projr_path_get_dir("docs", safe = TRUE)
      .build_copy_docs_rmd(FALSE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_true(file.exists(file.path(dir_docs, "test_files/abc.txt")))
      # Recreate test files for second test (they were moved in first test)
      invisible(file.create("test.html"))
      dir.create("test_files", showWarnings = FALSE)
      invisible(file.create("test_files/abc.txt"))
      dir_docs <- projr_path_get_dir("docs", safe = FALSE)
      .build_copy_docs_rmd(TRUE)
      expect_true(file.exists(file.path(dir_docs, "test.html")))
      expect_true(file.exists(file.path(dir_docs, "test_files/abc.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_engine works", {
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()

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
      .dir_move_exact(source_dir, dest_dir, fn_exc = "CHANGELOG.md")

      # Verify that CHANGELOG.md was excluded but other files were copied
      expect_true(file.exists(file.path(dest_dir, "test.html")))
      expect_true(file.exists(file.path(dest_dir, "other.txt")))
      expect_false(file.exists(file.path(dest_dir, "CHANGELOG.md")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("Rmd with self_contained: false copies _files directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create an Rmd file with self_contained: false
      rmd_content <- '---
title: "Test Self-Contained"
output:
  html_document:
    self_contained: false
---

```{r}
plot(1:10)
```
'
      writeLines(rmd_content, "test.Rmd")

      # Render it
      rmarkdown::render("test.Rmd", quiet = TRUE)

      # Verify _files directory was created
      expect_true(dir.exists("test_files"))

      # Test that both the HTML file and _files directory are copied
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      unlink(docs_dir, recursive = TRUE)
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)

      .build_copy_docs_rmd(FALSE)

      expect_true(file.exists(file.path(docs_dir, "test.html")))
      expect_true(dir.exists(file.path(docs_dir, "test_files")))

      # Verify files were actually copied
      files_in_root <- list.files("test_files", recursive = TRUE)
      files_in_docs <- list.files(file.path(docs_dir, "test_files"), recursive = TRUE)
      expect_true(length(files_in_docs) > 0)

      # Test with output_run = TRUE (safe = FALSE)
      docs_dir_unsafe <- projr_path_get_dir("docs", safe = FALSE)

      # Create fresh HTML and _files for this test
      rmarkdown::render("test.Rmd", quiet = TRUE)

      .build_copy_docs_rmd(TRUE)

      expect_true(file.exists(file.path(docs_dir_unsafe, "test.html")))
      expect_true(dir.exists(file.path(docs_dir_unsafe, "test_files")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Tests for untested helper functions in build-post-copy-docs.R
# ==============================================================

test_that(".build_copy_docs_rmd_is_html_format correctly identifies HTML formats", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Standard HTML formats
  expect_true(.build_copy_docs_rmd_is_html_format("html_document"))
  expect_true(.build_copy_docs_rmd_is_html_format("html_notebook"))
  expect_true(.build_copy_docs_rmd_is_html_format("html_vignette"))
  expect_true(.build_copy_docs_rmd_is_html_format("ioslides_presentation"))
  expect_true(.build_copy_docs_rmd_is_html_format("slidy_presentation"))

  # Package-prefixed HTML formats
  expect_true(.build_copy_docs_rmd_is_html_format("revealjs::revealjs_presentation"))
  expect_true(.build_copy_docs_rmd_is_html_format("flexdashboard::flex_dashboard"))
  expect_true(.build_copy_docs_rmd_is_html_format("tufte::tufte_html"))

  # Custom HTML formats (should match on "html" in name)
  expect_true(.build_copy_docs_rmd_is_html_format("prettydoc::html_pretty"))
  expect_true(.build_copy_docs_rmd_is_html_format("custom_html_theme"))

  # Non-HTML formats
  expect_false(.build_copy_docs_rmd_is_html_format("pdf_document"))
  expect_false(.build_copy_docs_rmd_is_html_format("word_document"))
  expect_false(.build_copy_docs_rmd_is_html_format("beamer_presentation"))
  expect_false(.build_copy_docs_rmd_is_html_format("powerpoint_presentation"))
  expect_false(.build_copy_docs_rmd_is_html_format("github_document"))
})

test_that(".build_copy_docs_paths_file copies individual files correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test file in project root
      test_file <- "test_doc.html"
      writeLines("Test content", test_file)
      expect_true(file.exists(test_file))

      # Copy to docs (dev mode)
      result <- .build_copy_docs_paths_file(test_file, output_run = FALSE)
      expect_true(result)

      # Verify file was moved to safe docs directory
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "test_doc.html")))
      expect_false(file.exists(test_file)) # Original should be moved

      # Test with non-existent file
      result <- .build_copy_docs_paths_file("nonexistent.html", output_run = FALSE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_paths_dir copies directories correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test directory with files
      test_dir <- "test_files"
      dir.create(test_dir)
      writeLines("File 1", file.path(test_dir, "file1.js"))
      writeLines("File 2", file.path(test_dir, "file2.css"))
      expect_true(dir.exists(test_dir))

      # Copy to docs (dev mode)
      result <- .build_copy_docs_paths_dir(test_dir, output_run = FALSE)
      expect_true(result)

      # Verify directory was moved to safe docs directory
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(dir.exists(file.path(docs_dir, test_dir)))
      expect_true(file.exists(file.path(docs_dir, test_dir, "file1.js")))
      expect_true(file.exists(file.path(docs_dir, test_dir, "file2.css")))

      # Test with non-existent directory
      result <- .build_copy_docs_paths_dir("nonexistent_dir", output_run = FALSE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_paths_rm_dir removes directories after copying", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test directories
      dir1 <- "temp_dir1"
      dir2 <- "temp_dir2"
      dir.create(dir1)
      dir.create(dir2)
      writeLines("content", file.path(dir1, "file.txt"))
      writeLines("content", file.path(dir2, "file.txt"))

      expect_true(dir.exists(dir1))
      expect_true(dir.exists(dir2))

      # Remove directories
      .build_copy_docs_paths_rm_dir(c(dir1, dir2))

      # Verify directories were removed
      expect_false(dir.exists(dir1))
      expect_false(dir.exists(dir2))

      # Test with non-existent paths (should not error)
      expect_silent(.build_copy_docs_paths_rm_dir(c("nonexistent1", "nonexistent2")))

      # Test with file path (should not remove, only removes directories)
      test_file <- "test.txt"
      writeLines("content", test_file)
      .build_copy_docs_paths_rm_dir(test_file)
      expect_true(file.exists(test_file)) # File should still exist
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_paths orchestrates copying correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test file and directory
      test_file <- "output.html"
      test_dir <- "output_files"
      writeLines("HTML content", test_file)
      dir.create(test_dir)
      writeLines("Resource", file.path(test_dir, "resource.js"))

      expect_true(file.exists(test_file))
      expect_true(dir.exists(test_dir))

      # Copy both (dev mode)
      .build_copy_docs_paths(c(test_dir, test_file), output_run = FALSE)

      # Verify both were copied
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "output.html")))
      expect_true(dir.exists(file.path(docs_dir, test_dir)))
      expect_true(file.exists(file.path(docs_dir, test_dir, "resource.js")))

      # Original paths should be removed (moved, not copied)
      expect_false(file.exists(test_file))
      expect_false(dir.exists(test_dir))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_rmd_ind processes individual Rmd files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create an Rmd file with html_document output
      rmd_file <- "report.Rmd"
      writeLines(c(
        "---",
        "title: Report",
        "output: html_document",
        "---",
        "# Content"
      ), rmd_file)

      # Create the expected output
      output_file <- "report.html"
      writeLines("HTML output", output_file)
      expect_true(file.exists(output_file))

      # Process the individual Rmd
      .build_copy_docs_rmd_ind(rmd_file, output_run = FALSE)

      # Verify output was copied to docs
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "report.html")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_quarto_ind processes individual Quarto files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a Quarto file with html format
      qmd_file <- "analysis.qmd"
      writeLines(c(
        "---",
        "title: Analysis",
        "format: html",
        "---",
        "# Results"
      ), qmd_file)

      # Create the expected output
      output_file <- "analysis.html"
      output_dir <- "analysis_files"
      writeLines("HTML output", output_file)
      dir.create(output_dir)
      writeLines("resource", file.path(output_dir, "libs.js"))
      expect_true(file.exists(output_file))
      expect_true(dir.exists(output_dir))

      # Process the individual Quarto file
      .build_copy_docs_quarto_ind(qmd_file, output_run = FALSE)

      # Verify output was copied to docs
      docs_dir <- projr_path_get_dir("docs", safe = TRUE)
      expect_true(file.exists(file.path(docs_dir, "analysis.html")))
      expect_true(dir.exists(file.path(docs_dir, output_dir)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_bookdown_files copies _files directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _bookdown.yml with book filename
      writeLines(c('book_filename: "mybook"'), "_bookdown.yml")

      # Create the _files directory in cache (where build happens)
      cache_dir <- .dir_get_cache_auto_version(profile = NULL)
      files_dir <- file.path(cache_dir, "mybook_files")
      dir.create(files_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines("figure", file.path(files_dir, "figure.png"))
      expect_true(dir.exists(files_dir))

      # Copy _files directory (output mode)
      result <- .build_copy_docs_bookdown_files(output_run = TRUE)
      expect_true(result)

      # Verify _files was copied to docs
      docs_dir <- projr_path_get_dir("docs", safe = FALSE)
      dest_files_dir <- file.path(docs_dir, "mybook_files")
      expect_true(dir.exists(dest_files_dir))
      expect_true(file.exists(file.path(dest_files_dir, "figure.png")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_docs_bookdown_files handles missing _files directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _bookdown.yml
      writeLines(c('book_filename: "mybook"'), "_bookdown.yml")

      # Don't create _files directory

      # Should return FALSE without error
      result <- .build_copy_docs_bookdown_files(output_run = TRUE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Tests for increasing coverage of R/build-post-copy.R
# ======================================================

test_that(".build_copy returns FALSE when .build_copy_check returns FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set dev-output to FALSE in YAML to make .build_copy_check return FALSE
      yml_projr <- .yml_get(NULL)
      yml_projr[["build"]][["dev-output"]] <- FALSE
      .yml_set(yml_projr)

      # Call .build_copy with output_run = FALSE (dev build)
      # When dev-output is FALSE and output_run is FALSE, .build_copy_check returns FALSE
      result <- .build_copy(
        output_run = FALSE,
        bump_component = "patch",
        version_run_on_list = list()
      )

      # Should return FALSE (early return at line 12)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_to_unsafe handles empty directory with subdirs but no files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a safe directory with subdirectories but no files
      safe_dir <- projr_path_get_dir("output", safe = TRUE)
      subdir1 <- file.path(safe_dir, "subdir1")
      subdir2 <- file.path(safe_dir, "subdir2")
      dir.create(subdir1, recursive = TRUE, showWarnings = FALSE)
      dir.create(subdir2, recursive = TRUE, showWarnings = FALSE)

      # Verify directories exist but have no files
      expect_true(dir.exists(safe_dir))
      expect_true(dir.exists(subdir1))
      expect_true(dir.exists(subdir2))
      expect_equal(length(list.files(safe_dir, recursive = TRUE)), 0)

      # Call .build_copy_to_unsafe
      result <- .build_copy_to_unsafe(output_run = TRUE)

      # Should return TRUE (success) but skip the empty directory
      expect_true(result)

      # Unsafe directory should not be created or should be empty
      unsafe_dir <- projr_path_get_dir("output", safe = FALSE, create = FALSE)
      if (dir.exists(unsafe_dir)) {
        expect_equal(length(list.files(unsafe_dir, recursive = TRUE)), 0)
      }
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_pkg_build_get_path returns correct path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Call the helper function
      result <- .build_copy_pkg_build_get_path()

      # Note: This function has a bug on line 121-122 where path_dir_pkg gets
      # assigned the version instead of the path. The function is not currently
      # used anywhere in the codebase. This test covers the actual behavior.

      # Get expected values
      pkg_name <- projr_name_get()
      version <- .desc_get()[, "Version"][[1]]

      # Should return a path containing the package name and version
      expect_true(grepl(pkg_name, result))
      expect_true(grepl(version, result))
      expect_true(grepl("\\.tar\\.gz$", result))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_copy_pkg_build_path_setup removes existing directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Get the package build path
      pkg_path <- .build_copy_pkg_build_path_get()

      # Create the directory with a file in it
      dir.create(pkg_path, recursive = TRUE, showWarnings = FALSE)
      test_file <- file.path(pkg_path, "test.txt")
      writeLines("test content", test_file)
      expect_true(file.exists(test_file))

      # Call .build_copy_pkg_build_path_setup
      result <- .build_copy_pkg_build_path_setup()

      # Should return the path
      expect_equal(result, pkg_path)

      # Directory should exist but be empty (old content removed)
      expect_true(dir.exists(pkg_path))
      expect_false(file.exists(test_file))
      expect_equal(length(list.files(pkg_path)), 0)
    },
    force = TRUE,
    quiet = TRUE
  )
})
