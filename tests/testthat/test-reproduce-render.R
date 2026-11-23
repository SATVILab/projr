test_that("replay renders production scripts between its hooks using empty outputs", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  usethis::with_project(
    project,
    {
      fs::dir_create(c("inputs", "artifacts", "scratch", ".projr/manifests"))
      writeLines("original", "inputs/input.txt")
      writeLines("old", "artifacts/stale.txt")
      writeLines("old", "scratch/.hidden")
      writeLines("record", ".projr/manifests/FORMAT")
      config <- c(
        "directories:",
        "  raw-data: {path: inputs}",
        "  cache: {path: scratch}",
        "  output: {path: artifacts, include: raw-data}",
        "build:",
        "  scripts: fail.R",
        "  hooks: {pre: pre.R, post: post.R}",
        "  local: {shared: {}}",
        "dev: {scripts: fail.R}",
        "remotes: {local: {shared: {path: elsewhere}}}"
      )
      writeLines(config, "_projr.yml")
      writeLines("build: {scripts: [run.R, run.R]}", "_projr-production.yml")
      writeLines("stop('wrong scripts')", "fail.R")
      writeLines(
        c(
          "stopifnot(readLines('VERSION') == 'v1.2.3.9000')",
          "writeLines('pre', 'order')"
        ),
        "pre.R"
      )
      writeLines(
        c(
          "stopifnot(!file.exists('scratch/.hidden'), !file.exists('artifacts/stale.txt'))",
          "cat('run', file = 'order', append = TRUE, sep = '\\n')",
          "writeLines(projr::projr_version_get(), projr::projr_path_get('output', 'version.txt'))"
        ),
        "run.R"
      )
      writeLines(
        c(
          "stopifnot(file.exists('artifacts/raw-data/input.txt'))",
          "cat('post', file = 'order', append = TRUE, sep = '\\n')",
          "cat('Hook: ran\\n', file = 'DESCRIPTION', append = TRUE)"
        ),
        "post.R"
      )
      initial <- "Package: replayfixture\r\nVersion: 1.2.3.9000\r\nTitle: Fixture\r\n"
      writeBin(charToRaw(initial), "DESCRIPTION")
      writeLines("v1.2.3.9000", "VERSION")
      profile_before <- Sys.getenv("PROJR_PROFILE", unset = NA_character_)
      expect_true(.reproduce_render("v1.2.3", profile = "production"))
      expect_identical(readLines("artifacts/version.txt"), "1.2.3")
      expect_identical(readLines("order"), c("pre", "run", "run", "post"))
      expect_identical(readLines("_projr.yml"), config)
      expect_identical(readLines("VERSION"), "v1.2.3.9000")
      hook_newline <- if (.Platform$OS.type == "windows") "\r\n" else "\n"
      expect_identical(
        readBin("DESCRIPTION", "raw", file.size("DESCRIPTION")),
        charToRaw(paste0(initial, "Hook: ran", hook_newline))
      )
      expect_identical(
        Sys.getenv("PROJR_PROFILE", unset = NA_character_),
        profile_before
      )
      expect_identical(readLines(".projr/manifests/FORMAT"), "record")
      expect_false(dir.exists("elsewhere"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("replay refuses directories that escape the clone or contain its inputs", {
  skip_if(.is_test_select())
  parent <- withr::local_tempdir()
  project <- file.path(parent, "checkout")
  fs::dir_create(c(project, file.path(parent, "external")))
  usethis::with_project(
    project,
    {
      fs::dir_create("working/inputs")
      writeLines("keep", "working/inputs/data.csv")
      writeLines("keep", "../external/data.csv")
      writeLines("v1.0.0", "VERSION")
      writeLines("stop('should not run')", "script.R")
      for (path in c(
        "../external",
        "working",
        "working/inputs/out",
        ".projr/history",
        "."
      )) {
        writeLines(
          c(
            "directories:",
            "  raw-data: {path: working/inputs}",
            "  cache: {path: scratch}",
            paste0("  output: {path: '", path, "'}"),
            "build: {scripts: script.R}"
          ),
          "_projr.yml"
        )
        expect_error(
          .reproduce_render("v1.0.0"),
          "outside|[Oo]verlapping|root directory"
        )
        expect_identical(readLines("../external/data.csv"), "keep")
        expect_identical(readLines("working/inputs/data.csv"), "keep")
      }
      writeLines(
        c(
          "directories:",
          "  raw-data: {path: working}",
          "build: {scripts: working/inputs/analysis.R}"
        ),
        "_projr.yml"
      )
      writeLines("1 + 1", "working/inputs/analysis.R")
      expect_error(.reproduce_check_dirs(), "overlapping protected files")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("the isolated analysis session shares hook state, renders nested documents and closes on error", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  withr::local_envvar(c(
    PROJR_REPRODUCE_ISOLATED = "TRUE",
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
  ))
  analysis <- NULL
  pre <- .build_pre_hooks_run
  testthat::local_mocked_bindings(.build_pre_hooks_run = function(...) {
    analysis <<- getOption("projr.reproduce.session")
    pre(...)
  })
  project <- withr::local_tempdir()
  usethis::with_project(
    project,
    {
      fs::dir_create("analysis")
      writeLines(
        c(
          "options(replay_pid = Sys.getpid())",
          "Sys.setenv(REPLAY_HOOK = 'pre')"
        ),
        "pre.R"
      )
      writeLines(
        c(
          "stopifnot(Sys.getenv('REPLAY_HOOK') == 'pre')",
          "options(analysis_ran = TRUE)"
        ),
        "analysis.R"
      )
      writeLines(
        c(
          "stopifnot(getOption('document_ran') == TRUE)",
          "writeLines(as.character(Sys.getpid()), 'post-pid')"
        ),
        "post.R"
      )
      writeLines(
        c(
          "directories:",
          "  cache: {path: scratch}",
          "  docs: {path: _docs}",
          "build: {scripts: [analysis.R, analysis/report.Rmd], hooks: {pre: pre.R, post: post.R}}"
        ),
        "_projr.yml"
      )
      writeLines(
        c(
          "---",
          "output: md_document",
          "---",
          "```{r}",
          "stopifnot(basename(getwd()) == 'analysis')",
          "stopifnot(getOption('analysis_ran') == TRUE, identical(Sys.getpid(), getOption('replay_pid')))",
          "options(document_ran = TRUE)",
          "'replayed document'",
          "```"
        ),
        "analysis/report.Rmd"
      )
      .reproduce_render("v1.0.0")
      expect_true(any(grepl(
        "replayed document",
        readLines("_docs/report.md"),
        fixed = TRUE
      )))
      expect_false(file.exists("VERSION"))
      expect_false(analysis$is_alive())
      expect_false(as.integer(readLines("post-pid")) == Sys.getpid())
      writeLines("v1.0.0-1", "VERSION")
      writeLines("stop('renderer failed')", "fail.R")
      writeLines(
        c(
          "directories:",
          "  cache: {path: scratch}",
          "build: {scripts: fail.R}"
        ),
        "_projr.yml"
      )
      expect_error(.reproduce_render("v1.0.0"), "renderer failed")
      expect_identical(readLines("VERSION"), "v1.0.0-1")
      expect_false(analysis$is_alive())
      expect_null(getOption("projr.reproduce.session"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("replay recomputes frozen Quarto projects and restores their YAML bytes", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if_not_installed("quarto")
  skip_if_not(quarto::quarto_available())
  project <- withr::local_tempdir()
  usethis::with_project(
    project,
    {
      writeLines(
        c(
          "directories:",
          "  cache: {path: cache}",
          "  docs: {path: site}",
          "build: {scripts: report.qmd}"
        ),
        "_projr.yml"
      )
      contents <- "# keep this comment\r\nproject: {type: website}\r\nexecute: {freeze: true}\r\n"
      writeBin(charToRaw(contents), "_quarto.yml")
      writeLines(
        c(
          "---",
          "title: Replay",
          "---",
          "```{r}",
          "cat('rendered', file = 'runs', sep = '\\n', append = TRUE)",
          "print(readLines('input.txt'))",
          "```"
        ),
        "report.qmd"
      )
      for (value in c("first-run-data", "second-run-data")) {
        writeLines(value, "input.txt")
        .reproduce_render("v1.2.3")
        expect_true(any(grepl(
          value,
          readLines("site/report.html", warn = FALSE),
          fixed = TRUE
        )))
        expect_identical(
          readBin("_quarto.yml", "raw", file.size("_quarto.yml")),
          charToRaw(contents)
        )
      }
      expect_identical(readLines("runs"), c("rendered", "rendered"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("replay detects output paths below a linked parent before clearing any directory", {
  skip_if(.is_test_select())
  skip_on_os("windows")
  parent <- withr::local_tempdir()
  project <- file.path(parent, "checkout")
  fs::dir_create(c(project, file.path(parent, "external")))
  usethis::with_project(
    project,
    {
      fs::link_create("../external", "shortcut")
      writeLines("keep", "../external/data.txt")
      writeLines(
        c(
          "directories:",
          "  cache: {path: scratch}",
          "  output: {path: shortcut/products}"
        ),
        "_projr.yml"
      )
      expect_error(.reproduce_check_dirs(), "outside")
      expect_identical(readLines("../external/data.txt"), "keep")
      expect_false(dir.exists("../external/products"))
    },
    quiet = TRUE,
    force = TRUE
  )
})
