# Write `_projr.yml` from YAML lines so key order is exactly as written.
.test_pipeline_config <- function(lines) {
  writeLines(
    c(
      "directories:",
      "  raw-data: {path: _raw_data}",
      "  output: {path: _output}",
      "  cache: {path: _tmp}",
      "  docs: {path: docs}",
      lines
    ),
    "_projr.yml"
  )
}

.test_pipeline_project <- function(env = parent.frame()) {
  project <- withr::local_tempdir(.local_envir = env)
  writeLines("0.0.1-1", file.path(project, "VERSION"))
  writeLines("directories: {}", file.path(project, "_projr.yml"))
  project
}

test_that("a pipeline combines with inline scripts and hooks by key position", {
  skip_if(.is_test_select())
  project <- .test_pipeline_project()
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    pipelines <- c(
      "pipelines:",
      "  analysis:",
      "    scripts: [prepare.R, analyse.R]",
      "    hooks:",
      "      pre: hooks/check-data.R",
      "      post: hooks/validate-results.R",
      "  sensitivity:",
      "    scripts: [sensitivity.R]"
    )
    # The example in #1063: scripts before pipeline, hooks after it.
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  scripts: initialise.R",
      "  pipeline: analysis",
      "  hooks:",
      "    pre: hooks/check-build.R",
      "    post: hooks/finish-build.R"
    ))
    expect_true(projr_yml_check())
    plan <- .pipeline_plan("build")
    expect_identical(plan$name, "analysis")
    expect_identical(plan$scripts, c("initialise.R", "prepare.R", "analyse.R"))
    expect_identical(plan$pre, c("hooks/check-data.R", "hooks/check-build.R"))
    expect_identical(
      plan$post,
      c("hooks/validate-results.R", "hooks/finish-build.R")
    )

    # Hooks before pipeline run first in each phase; scripts after append.
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  hooks:",
      "    pre: hooks/check-build.R",
      "    both: hooks/log.R",
      "  pipeline: analysis",
      "  scripts: [finalise.R]"
    ))
    plan <- .pipeline_plan("build")
    expect_identical(plan$scripts, c("prepare.R", "analyse.R", "finalise.R"))
    expect_identical(
      plan$pre,
      c("hooks/check-build.R", "hooks/log.R", "hooks/check-data.R")
    )
    expect_identical(plan$post, c("hooks/log.R", "hooks/validate-results.R"))

    # An explicit empty inline list adds nothing and keeps the pipeline.
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  scripts: []",
      "  pipeline: analysis"
    ))
    expect_identical(
      .pipeline_plan("build")$scripts,
      c("prepare.R", "analyse.R")
    )

    # An argument replacing the configured pipeline keeps its position.
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  pipeline: analysis",
      "  scripts: [finalise.R]"
    ))
    withr::with_envvar(c(PROJR_PIPELINE = "sensitivity"), {
      expect_identical(
        .pipeline_plan("build")$scripts,
        c("sensitivity.R", "finalise.R")
      )
    })

    # An argument-only selection runs after the inline entries.
    .test_pipeline_config(c(pipelines, "build:", "  scripts: [finalise.R]"))
    expect_null(.pipeline_plan("build"))
    withr::with_envvar(c(PROJR_PIPELINE = "sensitivity"), {
      expect_identical(
        .pipeline_plan("build")$scripts,
        c("finalise.R", "sensitivity.R")
      )
    })

    # Development builds use dev.pipeline, else build.pipeline, with dev.hooks.
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  pipeline: analysis",
      "dev:",
      "  hooks: {pre: hooks/dev.R}"
    ))
    plan <- .pipeline_plan("dev")
    expect_identical(plan$name, "analysis")
    expect_identical(plan$pre, c("hooks/dev.R", "hooks/check-data.R"))
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  pipeline: analysis",
      "dev:",
      "  pipeline: sensitivity"
    ))
    expect_identical(.pipeline_plan("dev")$scripts, "sensitivity.R")
  })
})

test_that("profiles select pipelines, set positions and replace pipeline scripts", {
  skip_if(.is_test_select())
  project <- .test_pipeline_project()
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    .test_pipeline_config(c(
      "pipelines:",
      "  analysis: {scripts: [prepare.R, analyse.R]}",
      "build:",
      "  scripts: [initialise.R]",
      "  pipeline: analysis"
    ))
    # The highest-precedence file that sets pipeline decides the position.
    writeLines(
      c("build:", "  pipeline: analysis", "  scripts: [late.R]"),
      "_projr-local.yml"
    )
    expect_identical(
      .pipeline_plan("build")$scripts,
      c("prepare.R", "analyse.R", "late.R")
    )
    unlink("_projr-local.yml")

    # A profile's pipeline scripts replace the default list rather than merge.
    writeLines(
      "pipelines: {analysis: {scripts: [quick.R]}}",
      "_projr-fast.yml"
    )
    withr::with_envvar(c(PROJR_PROFILE = "fast"), {
      expect_identical(
        .pipeline_plan("build")$scripts,
        c("initialise.R", "quick.R")
      )
    })
  })
})

test_that("pipeline configuration and selections are validated early", {
  skip_if(.is_test_select())
  project <- .test_pipeline_project()
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    .test_pipeline_config(c("build:", "  pipeline: missing"))
    expect_error(
      projr_yml_check(),
      "build.pipeline: missing matches no pipeline"
    )
    .test_pipeline_config(c(
      "pipelines:",
      "  analysis: {script: [prepare.R]}"
    ))
    expect_error(projr_yml_check(), "pipelines.analysis")
    .test_pipeline_config(c(
      "pipelines:",
      "  analysis: {hooks: {during: x.R}}"
    ))
    expect_error(projr_yml_check(), "pipelines.analysis.hooks")
    .test_pipeline_config(c("dev:", "  pipeline: [a, b]"))
    expect_error(projr_yml_check(), "dev.pipeline")
    .test_pipeline_config(c("pipelines:", "  analysis: {scripts: [a.R]}"))
    expect_error(.pipeline_check_name("other"), "matches no pipeline")
  })
})

test_that("builds run the pipeline's hooks and scripts in order", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    log <- file.path(project, "order.txt")
    step <- function(name) {
      sprintf("cat('%s\\n', file = '%s', append = TRUE)", name, log)
    }
    fs::dir_create("hooks")
    for (name in c("check-data", "check-build", "validate", "finish")) {
      writeLines(step(name), file.path("hooks", paste0(name, ".R")))
    }
    for (name in c("initialise", "prepare", "analyse", "sensitivity")) {
      writeLines(step(name), paste0(name, ".R"))
    }
    config <- .yml_get("default")
    config$build <- c(
      list(scripts = list("initialise.R"), pipeline = "analysis"),
      list(
        hooks = list(pre = "hooks/check-build.R", post = "hooks/finish.R")
      ),
      config$build[setdiff(names(config$build), c("scripts", "hooks"))]
    )
    config$pipelines <- list(
      analysis = list(
        scripts = list("prepare.R", "analyse.R"),
        hooks = list(pre = "hooks/check-data.R", post = "hooks/validate.R")
      ),
      sensitivity = list(scripts = list("sensitivity.R"))
    )
    .yml_set(config)

    # Development builds keep using dev.hooks, not build.hooks.
    projr_build_dev(project = project)
    expect_identical(
      readLines(log),
      c("check-data", "initialise", "prepare", "analyse", "validate")
    )

    # The argument switches the pipeline for one build.
    unlink(log)
    projr_build_dev(pipeline = "sensitivity", project = project)
    expect_identical(readLines(log), c("initialise", "sensitivity"))
    expect_identical(Sys.getenv("PROJR_PIPELINE"), "")
    expect_error(
      projr_build_dev(pipeline = "missing", project = project),
      "matches no pipeline"
    )

    # Production builds run build.hooks around the pipeline's and record it.
    unlink(log)
    projr_build_patch("pipeline", project = project)
    expect_identical(
      readLines(log),
      c(
        "check-data",
        "check-build",
        "initialise",
        "prepare",
        "analyse",
        "validate",
        "finish"
      )
    )
    expect_true(any(grepl("**Pipeline**", .buildlog_read(), fixed = TRUE)))
  })
})

test_that("several pipelines and globs run in order, and pipelines can be switched off", {
  skip_if(.is_test_select())
  project <- .test_pipeline_project()
  usethis::with_project(project, quiet = TRUE, force = TRUE, {
    pipelines <- c(
      "pipelines:",
      "  prepare: {scripts: [prepare.R], hooks: {pre: hooks/a.R}}",
      "  model-b: {scripts: [b.R], hooks: {pre: hooks/a.R, post: hooks/b.R}}",
      "  model-a: {scripts: [a.R]}",
      "  report: {scripts: [report.qmd]}"
    )
    # Entries run in the order given; a glob's matches in definition order.
    .test_pipeline_config(c(
      pipelines,
      "build:",
      "  scripts: [start.R]",
      "  pipeline: [prepare, model-*, report, prepare]"
    ))
    expect_true(projr_yml_check())
    plan <- .pipeline_plan("build")
    expect_identical(plan$name, c("prepare", "model-b", "model-a", "report"))
    expect_identical(
      plan$scripts,
      c("start.R", "prepare.R", "b.R", "a.R", "report.qmd")
    )
    expect_identical(plan$pre, "hooks/a.R")
    expect_identical(plan$post, "hooks/b.R")

    # The argument takes several entries and globs too.
    withr::with_envvar(c(PROJR_PIPELINE = "report,model-*"), {
      expect_identical(
        .pipeline_plan("build")$scripts,
        c("start.R", "report.qmd", "b.R", "a.R")
      )
    })
    expect_error(.pipeline_check_name(c("prepare", "nope-*")), "nope-\\*")

    # The scripts argument adds to the pipelines in the inline position.
    expect_identical(
      .pipeline_plan("build", scripts = "only.R")$scripts,
      c("only.R", "prepare.R", "b.R", "a.R", "report.qmd")
    )

    # pipeline: false (or []) in a higher layer, or the argument, switches the
    # pipelines off; inline scripts still run.
    writeLines(c("build:", "  pipeline: false"), "_projr-local.yml")
    expect_true(projr_yml_check())
    expect_null(.pipeline_plan("build"))
    expect_identical(.scripts_get_specified(NULL, "patch", NULL), "start.R")
    writeLines(c("build:", "  pipeline: []"), "_projr-local.yml")
    expect_null(.pipeline_plan("build"))
    unlink("_projr-local.yml")
    withr::with_envvar(c(PROJR_PIPELINE = .pipeline_none), {
      expect_null(.pipeline_plan("build"))
      expect_identical(
        .scripts_get_specified("only.R", "patch", NULL),
        "only.R"
      )
    })

    # Names cannot hold the argument's separators.
    .test_pipeline_config(c("pipelines:", "  'a,b': {scripts: [x.R]}"))
    expect_error(projr_yml_check(), "cannot contain")
  })
})
