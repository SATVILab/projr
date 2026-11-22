test_that("engine steps keep explicit order and put the automatic project first", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(dir_test,
    {
      file.create("a.R", "b.R", "z.qmd", "a.qmd", "chapter.Rmd")
      file.create("hook.R")
      config <- .yml_get("default")
      config[["build"]][["hooks"]] <- list(post = "hook.R")
      .yml_set(config, "default")
      scripts <- c("b.R", "index.Rmd", "a.R", "chapter.Rmd", "a.qmd")
      steps <- .engines_get(scripts)
      expect_identical(names(steps), c("r", "r", "bookdown", "quarto_document"))
      expect_identical(steps[[3L]], c("index.Rmd", "chapter.Rmd"))
      expect_identical(unlist(steps[1:2], use.names = FALSE), c("b.R", "a.R"))
      expect_identical(names(.engines_get("a.R")), "r")

      automatic <- .engines_get()
      expect_identical(names(automatic), c("bookdown", "r", "r"))
      expect_null(automatic[[1L]])
      expect_identical(basename(unlist(automatic)), c("a.R", "b.R"))

      unlink("_bookdown.yml")
      steps <- .engines_get(.scripts_get_final(c("z.qmd", "a.R", "*.qmd")))
      expect_identical(names(steps), c("quarto_document", "r", "quarto_document"))
      expect_identical(basename(unlist(steps)), c("z.qmd", "a.R", "a.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

for (project in c("quarto_project", "quarto_book", "bookdown")) {
  test_that(paste(project, "uses selected chapters through post-build hooks"), {
    skip_if(.is_test_cran())
    skip_if(.is_test_select())
    dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

    usethis::with_project(dir_test,
      {
        is_quarto <- project != "bookdown"
        is_dev <- project == "quarto_book"
        .test_setup_project_lit_docs(if (is_quarto) "quarto_project" else "bookdown")
        if (project == "quarto_book") {
          .yml_quarto_set(list(
            project = list(type = "book"),
            book = list(title = "Test book", chapters = "index.qmd", appendices = "excluded.qmd")
          ))
        }
        extension <- if (is_quarto) "qmd" else "Rmd"
        config_file <- if (is_quarto) "_quarto.yml" else "_bookdown.yml"
        chapters <- paste0(c("index", "chapter"), ".", extension)
        if (project == "quarto_project") {
          dir.create("reports")
          chapters[[2L]] <- file.path("reports", chapters[[2L]])
        }
        excluded <- paste0("excluded.", extension)
        render_field <- if (project == "quarto_book") {
          '[["book"]][["chapters"]]'
        } else if (is_quarto) {
          '[["project"]][["render"]]'
        } else {
          '[["rmd_files"]]'
        }
        for (index in seq_along(chapters)) {
          log_path <- if (dirname(chapters[[index]]) == ".") "order.txt" else "../order.txt"
          writeLines(c(
            "---",
            paste("title: Chapter", index),
            "---",
            "",
            paste("# Chapter", index),
            "",
            "```{r}",
            paste0("cat('chapter", index, "\\n', file = '", log_path, "', append = TRUE)"),
            "```"
          ), chapters[[index]])
        }
        writeLines(c("# Excluded", "", "```{r}", "stop('excluded chapter ran')", "```"), excluded)
        writeLines("cat('first\n', file = 'order.txt', append = TRUE)", "first.R")
        writeLines("cat('middle\n', file = 'order.txt', append = TRUE)", "middle.R")
        writeLines("cat('last\n', file = 'order.txt', append = TRUE)", "last.R")
        writeLines(c(
          paste0("selection <- yaml::read_yaml('", config_file, "')", render_field),
          "writeLines(selection, 'post-selection.txt')"
        ), "check-selection.R")

        config <- .yml_get("default")
        config[["directories"]][["docs"]][["path"]] <- "published/site"
        selection <- list(
          scripts = c("first.R", chapters[[1L]], "middle.R", chapters[[2L]], "last.R"),
          hooks = list(post = "check-selection.R")
        )
        if (is_dev) {
          config[["dev"]] <- selection
        } else {
          config[["build"]][["scripts"]] <- selection[["scripts"]]
          config[["build"]][["hooks"]] <- selection[["hooks"]]
          config[["build"]][["git"]] <- FALSE
        }
        .yml_set(config, "default")

        if (is_dev) {
          projr_build_dev()
        } else {
          projr_build_patch(msg = "Test selected chapters")
        }

        expect_identical(readLines("order.txt"), c("first", "middle", "chapter1", "chapter2", "last"))
        expect_identical(readLines("post-selection.txt"), chapters)
        if (is_quarto) {
          expect_null(.yml_quarto_get()[["project"]][["render"]])
          if (project == "quarto_book") {
            expect_identical(.yml_quarto_get()[["book"]][["appendices"]], excluded)
          }
        } else {
          expect_identical(.yml_bd_get()[["rmd_files"]], "index.Rmd")
        }
        docs <- projr_path_get_dir("docs", safe = is_dev)
        expect_true(file.exists(file.path(docs, "index.html")))
        if (project == "quarto_project") {
          expect_true(file.exists(file.path(docs, "reports", "chapter.html")))
        }
      },
      force = TRUE,
      quiet = TRUE
    )
  })
}

test_that("a failed render restores project selection and ignores new TeX files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(dir_test,
    {
      dir.create("reports")
      writeLines("# Extra chapter", "reports/chapter.Rmd")
      writeLines("authored source", "reports/source.tex")
      writeLines(c(
        "# Failing chapter",
        "",
        "```{r}",
        "writeLines('generated', 'reports/unexpected.tex')",
        "stop('expected build failure')",
        "```"
      ), "index.Rmd")
      expect_error(projr_build_dev(scripts = c("index.Rmd", "reports/chapter.Rmd")), "expected build failure")
      expect_identical(.yml_bd_get()[["rmd_files"]], "index.Rmd")
      expect_identical(readLines("reports/unexpected.tex"), "generated")
      projr_ignore_auto()
      git_ignored <- function(path) {
        system2("git", c("check-ignore", "--quiet", "--", path)) == 0L
      }
      expect_true(git_ignored("reports/unexpected.tex"))
      expect_false(git_ignored("reports/source.tex"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
