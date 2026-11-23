test_that("development and production report the actions they run", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  withr::local_envvar(c(PROJR_OUTPUT_LEVEL = "std", PROJR_LOG_DETAILED = "true"))
  project <- withr::local_tempdir()
  usethis::with_project(project, {
    writeLines(c(
      "directories:",
      "  cache: {path: cache}",
      "  output: {path: results}",
      "build:",
      "  scripts: [analysis.R]",
      "  git: false",
      "  renv: false",
      "  hooks: {post: production.R}"
    ), "_projr.yml")
    writeLines("cat('analysis\\n', file = 'runs', append = TRUE)", "analysis.R")
    writeLines(c("warning('example hook warning')", "cat('post\\n', file = 'runs', append = TRUE)"),
               "production.R")

    output <- capture_messages(expect_warning(projr_build_patch("Example", project = project), "example hook warning"))
    expect_identical(readLines("runs"), c("analysis", "post"))
    expect_true(all(file.exists(c(".projr/BUILDLOG.md", "CHANGELOG.md"))))
    log <- list.files(".projr/logs/output", "\\.qmd$", recursive = TRUE, full.names = TRUE)
    expect_length(log, 1L)
    expect_equal(sum(grepl("**WARNING**: example hook warning", readLines(log), fixed = TRUE)), 1L)
    for (lines in list(output, readLines(log))) {
      expect_true(any(grepl("Running R script analysis.R", lines, fixed = TRUE)))
      expect_true(any(grepl("Recording build metadata", lines, fixed = TRUE)))
      expect_true(any(grepl("Running post-build hook production.R", lines, fixed = TRUE)))
      expect_false(any(grepl("Pushing to Git|Created Git commit|Snapshotting renv", lines)))
    }

    dev <- capture_messages(projr_build_dev(project = project))
    expect_identical(readLines("runs"), c("analysis", "post", "analysis"))
    expect_true(file.exists(log))
    dev_log <- list.files(".projr/logs/dev", "\\.qmd$", recursive = TRUE, full.names = TRUE)
    expect_length(dev_log, 1L)
    expect_true(any(grepl("Build completed successfully", dev, fixed = TRUE)))
    expect_true(any(grepl("[OK] SUCCESS", readLines(dev_log), fixed = TRUE)))
    for (lines in list(dev, readLines(dev_log))) {
      expect_true(any(grepl("Running R script analysis.R", lines, fixed = TRUE)))
      expect_false(any(grepl(paste(
        "Committing|Created Git commit|Pushing to Git|Preparing remote|Sending artifacts",
        "Recording build metadata|Updating (renv|roxygen|changelog|manifest|buildlog)",
        "Running (pre|post)-build hook|Setting development version",
        sep = "|"
      ), lines)))
    }

    empty <- capture_messages(projr_build_dev(scripts = character(), project = project))
    expect_true(any(grepl("No scripts or documents selected", empty, fixed = TRUE)))
    expect_false(any(grepl("Running R script", empty, fixed = TRUE)))
    expect_identical(readLines("runs"), c("analysis", "post", "analysis"))
    expect_true(file.exists(dev_log))
  }, force = TRUE, quiet = TRUE)
})

test_that("post-hook errors and interrupts keep their cause in the detailed log", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  withr::local_envvar(c(PROJR_OUTPUT_LEVEL = "std", PROJR_LOG_DETAILED = "true"))
  project <- withr::local_tempdir()
  usethis::with_project(project, {
    writeLines(c(
      "directories: {cache: {path: cache}}",
      "dev: {scripts: [], hooks: {post: hook.R}}"
    ), "_projr.yml")
    previous <- character()
    for (kind in c("error", "interrupt")) {
      if (kind == "error") {
        writeLines("stop(errorCondition('hook failed', class = 'hook_error'))", "hook.R")
      } else {
        writeLines("stop(structure(list(), class = c('interrupt', 'condition')))", "hook.R")
      }
      output <- capture_messages({
        failure <- tryCatch(projr_build_dev(project = project), error = identity, interrupt = identity)
      })
      expect_s3_class(failure, if (kind == "error") "hook_error" else "interrupt")
      logs <- list.files(".projr/logs/dev", "\\.qmd$", recursive = TRUE, full.names = TRUE)
      current <- setdiff(logs, previous)
      expect_length(current, 1L)
      detailed <- readLines(current)
      expect_true(any(grepl("Running post-build hook hook.R", output, fixed = TRUE)))
      expect_false(any(grepl("Build completed successfully", c(output, detailed), fixed = TRUE)))
      cause <- if (kind == "error") "hook failed" else "Build interrupted"
      evidence <- c("Development Build: Post-Build", "Running post-build hook hook.R", cause, "[X] FAILED")
      positions <- vapply(evidence, function(x) match(TRUE, grepl(x, detailed, fixed = TRUE)), integer(1))
      expect_false(anyNA(positions))
      expect_true(all(diff(positions) > 0))
      previous <- logs
    }
    expect_true(all(file.exists(previous)))
  }, force = TRUE, quiet = TRUE)
})

test_that("failure to write history leaves the original build error intact", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  withr::local_envvar(c(PROJR_OUTPUT_LEVEL = "std", PROJR_LOG_DETAILED = "true"))
  project <- withr::local_tempdir()
  usethis::with_project(project, {
    writeLines(c(
      "directories: {cache: {path: cache}}",
      "dev: {scripts: [analysis.R]}"
    ), "_projr.yml")
    local_mocked_bindings(.log_history_add = function(...) stop("history unavailable"))
    writeLines("stop('analysis failed')", "analysis.R")
    output <- capture_messages(expect_error(projr_build_dev(project = project), "analysis failed"))
    expect_true(any(grepl("Could not finish the build log: history unavailable", output, fixed = TRUE)))
    writeLines("invisible(TRUE)", "analysis.R")
    output <- capture_messages(expect_error(projr_build_dev(project = project), "history unavailable"))
    expect_false(any(grepl("Build completed successfully", output, fixed = TRUE)))
  }, force = TRUE, quiet = TRUE)
})

test_that("the first failed script selection leaves the build log ignored", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  usethis::with_project(project, {
    writeLines("directories: {cache: {path: cache}}", "_projr.yml")
    expect_error(projr_build_dev(scripts = "absent.R", project = "."), "absent.R")
    expect_true(file.exists(".projr/logs/dev/history/builds.md"))
    expect_true("/.projr/logs/**" %in% readLines(".gitignore"))
  }, quiet = TRUE, force = TRUE)
})
