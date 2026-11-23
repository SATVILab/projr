test_that(".yml_scripts functions work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: Direct list format for build scripts
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("script1.qmd", "script2.R")
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        as.character(.yml_scripts_get_build("default")),
        file.path(dir_test, c("script1.qmd", "script2.R"))
      )

      # Test 2: Dev scripts use top-level dev.scripts
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("full-analysis.qmd")
          ),
          dev = list(
            scripts = c("quick-test.qmd", "debug.R")
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        as.character(.yml_scripts_get_build("default")),
        file.path(dir_test, "full-analysis.qmd")
      )
      expect_identical(
        .yml_scripts_get_dev("default"),
        c("quick-test.qmd", "debug.R")
      )

      # Test 3: Fallback to build.scripts when dev.scripts not specified
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("analysis.qmd")
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        as.character(.yml_scripts_get_dev("default")),
        file.path(dir_test, "analysis.qmd")
      )

      # Test 4: NULL when build scripts not specified
      yaml::write_yaml(
        list(
          build = list()
        ),
        "_projr.yml"
      )
      expect_null(.yml_scripts_get_build("default"))
      expect_null(.yml_scripts_get_dev("default"))
    }
  )
})

test_that(".yml_hooks simple structure works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: Simple hooks structure with both, pre, and post
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              both = c("both1.R", "both2.R"),
              pre = "pre.R",
              post = c("post1.R", "post2.R")
            )
          )
        ),
        "_projr.yml"
      )

      # Check that hooks are read correctly
      pre_hooks <- .yml_hooks_get_stage("pre", "default")
      expect_true(!is.null(pre_hooks))
      expect_true("pre.R" %in% pre_hooks)
      expect_true("both1.R" %in% pre_hooks)
      expect_true("both2.R" %in% pre_hooks)

      post_hooks <- .yml_hooks_get_stage("post", "default")
      expect_true(!is.null(post_hooks))
      expect_true("post1.R" %in% post_hooks)
      expect_true("post2.R" %in% post_hooks)
      expect_true("both1.R" %in% post_hooks)
      expect_true("both2.R" %in% post_hooks)

      # Test 2: Only pre hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = c("pre1.R", "pre2.R")
            )
          )
        ),
        "_projr.yml"
      )

      pre_hooks <- .yml_hooks_get_stage("pre", "default")
      expect_identical(pre_hooks, c("pre1.R", "pre2.R"))

      post_hooks <- .yml_hooks_get_stage("post", "default")
      expect_null(post_hooks)

      # Test 3: Only post hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              post = "post.R"
            )
          )
        ),
        "_projr.yml"
      )

      post_hooks <- .yml_hooks_get_stage("post", "default")
      expect_identical(post_hooks, "post.R")

      # Test 4: Only both hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              both = c("both1.R", "both2.R")
            )
          )
        ),
        "_projr.yml"
      )

      pre_hooks <- .yml_hooks_get_stage("pre", "default")
      post_hooks <- .yml_hooks_get_stage("post", "default")
      expect_identical(pre_hooks, c("both1.R", "both2.R"))
      expect_identical(post_hooks, c("both1.R", "both2.R"))
    }
  )
})

test_that(".engine_get_from_scripts works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with .qmd files
  expect_identical(
    .engine_get_from_scripts(c("analysis.qmd", "report.qmd")),
    "quarto_document"
  )

  # Test with .Rmd files
  expect_identical(
    .engine_get_from_scripts(c("analysis.Rmd", "report.Rmd")),
    "rmd"
  )

  # Test with .R files
  expect_identical(
    .engine_get_from_scripts(c("script.R")),
    "r"
  )

  # Test with mixed files - qmd takes priority
  expect_identical(
    .engine_get_from_scripts(c("analysis.qmd", "report.Rmd")),
    "quarto_document"
  )

  # Test with NULL
  expect_identical(
    .engine_get(NULL),
    .engine_get_auto()
  )
})

test_that("dev.scripts is used exclusively for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: dev.scripts is used for dev builds
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("build1.qmd", "build2.qmd")
          ),
          dev = list(
            scripts = c("dev1.qmd", "dev2.R")
          )
        ),
        "_projr.yml"
      )

      # Dev builds should use dev.scripts
      expect_identical(
        .yml_scripts_get_dev("default"),
        c("dev1.qmd", "dev2.R")
      )

      # Production builds should use build.scripts
      expect_identical(
        as.character(.yml_scripts_get_build("default")),
        file.path(dir_test, c("build1.qmd", "build2.qmd"))
      )

      # Test 2: Fallback to build.scripts when dev.scripts is not specified
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("build1.qmd", "build2.qmd")
          )
        ),
        "_projr.yml"
      )

      # Without dev.scripts, dev builds fall back to build.scripts
      expect_identical(
        as.character(.yml_scripts_get_dev("default")),
        file.path(dir_test, c("build1.qmd", "build2.qmd"))
      )

      # Production builds still use build.scripts
      expect_identical(
        as.character(.yml_scripts_get_build("default")),
        file.path(dir_test, c("build1.qmd", "build2.qmd"))
      )
    }
  )
})

test_that("File existence checks work for scripts and hooks", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: Non-existent build script should error
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("nonexistent.qmd")
          )
        ),
        "_projr.yml"
      )

      expect_error(
        .scripts_get(NULL, "patch", "default"),
        "Script .*nonexistent[.]qmd.*does not exist"
      )

      # Test 2: Non-existent dev script should error
      yaml::write_yaml(
        list(
          dev = list(
            scripts = c("nonexistent-dev.R")
          )
        ),
        "_projr.yml"
      )

      expect_error(
        .scripts_get(NULL, NULL, "default"),
        "Script 'nonexistent-dev.R' does not exist"
      )

      # Test 3: Non-existent hook should error
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = "nonexistent-hook.R"
            )
          )
        ),
        "_projr.yml"
      )

      expect_error(
        .yml_hooks_check_exist("default"),
        "Hook 'nonexistent-hook.R' .* does not exist"
      )

      # Test 4: Existing files should not error
      # Create dummy files
      file.create("exists.qmd")
      file.create("exists-hook.R")

      yaml::write_yaml(
        list(
          build = list(
            scripts = c("exists.qmd"),
            hooks = list(
              pre = "exists-hook.R"
            )
          )
        ),
        "_projr.yml"
      )

      expect_silent(.yml_hooks_check_exist("default"))
    }
  )
})

test_that("dev.hooks works for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: dev.hooks are used for dev builds, build.hooks ignored
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = "build-pre.R",
              post = "build-post.R"
            )
          ),
          dev = list(
            hooks = list(
              pre = "dev-pre.R",
              both = "dev-both.R"
            )
          )
        ),
        "_projr.yml"
      )

      # Check that dev.hooks are accessible
      dev_hooks <- .yml_dev_get_hooks("default")
      expect_true(!is.null(dev_hooks))
      expect_true("pre" %in% names(dev_hooks))
      expect_true("both" %in% names(dev_hooks))

      # Test 2: When no dev.hooks, dev builds get no hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = "build-pre.R"
            )
          )
        ),
        "_projr.yml"
      )

      expect_null(.yml_dev_get_hooks("default"))
    }
  )
})

test_that("Issue scenario: build.scripts used as fallback for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Scenario from issue: only build.scripts is set, no dev.scripts
      # When scripts is NULL, dev build should use build.scripts
      yaml::write_yaml(
        list(
          build = list(
            scripts = "ExtraULW1S1ClusterIntro.qmd",
            git = TRUE
          )
        ),
        "_projr.yml"
      )

      # Dev build should use build.scripts as fallback
      expect_identical(
        as.character(.yml_scripts_get_dev("default")),
        file.path(dir_test, "ExtraULW1S1ClusterIntro.qmd")
      )

      # Production build should use build.scripts
      expect_identical(
        as.character(.yml_scripts_get_build("default")),
        file.path(dir_test, "ExtraULW1S1ClusterIntro.qmd")
      )

      # When dev.scripts is explicitly set, it takes precedence
      yaml::write_yaml(
        list(
          build = list(
            scripts = "ExtraULW1S1ClusterIntro.qmd",
            git = TRUE
          ),
          dev = list(
            scripts = "ExtraULW1S2ClusterHier.qmd"
          )
        ),
        "_projr.yml"
      )

      # Dev build should use dev.scripts (not build.scripts)
      expect_identical(
        .yml_scripts_get_dev("default"),
        "ExtraULW1S2ClusterHier.qmd"
      )

      # Production build should still use build.scripts
      expect_identical(
        as.character(.yml_scripts_get_build("default")),
        file.path(dir_test, "ExtraULW1S1ClusterIntro.qmd")
      )
    }
  )
})

test_that("repeated literal scripts run in order and project documents reject repeats", {
  skip_if(.is_test_select())
  project <- .test_setup_project(rm_engine = TRUE, git = FALSE)
  usethis::with_project(project, {
    writeLines('cat("a\\n", file = "order.txt", append = TRUE)', "a.R")
    writeLines('cat("b\\n", file = "order.txt", append = TRUE)', "b.R")
    selection <- c("[ab].R", "a.R", "b.R", "a.R")
    expect_identical(basename(.scripts_get_final(selection)), c("a.R", "b.R", "a.R"))
    projr_build_dev(scripts = c("a.R", "b.R", "a.R"))
    expect_identical(readLines("order.txt"), c("a", "b", "a"))

    writeLines(c(
      "---", "title: Repeat", "output: html_document", "---",
      "```{r}", 'cat("rmd\\n", file = "order.txt", append = TRUE)', "```"
    ), "report.Rmd")
    projr_build_dev(scripts = c("report.Rmd", "report.Rmd"))
    expect_identical(tail(readLines("order.txt"), 2L), c("rmd", "rmd"))
    expect_true(file.exists(file.path(projr_path_get_dir("docs", safe = TRUE), "report.html")))

    writeLines("rmd_files: [report.Rmd]", "_bookdown.yml")
    expect_error(.engines_get(.scripts_get_final(rep("report.Rmd", 2))), "Bookdown.*repeated renders.*report[.]Rmd")
    unlink("_bookdown.yml")
    writeLines(c("project:", "  type: website"), "_quarto.yml")
    file.create("chapter.qmd")
    expect_error(.engines_get(.scripts_get_final(rep("chapter.qmd", 2))), "Quarto.*repeated renders.*chapter[.]qmd")
  })
})
