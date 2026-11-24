test_that("development builds run with and without a branch restriction", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize project
      .init()

      # Get current branch
      current_branch <- .git_branch_get()
      expect_true(!is.null(current_branch))

      # Create a simple Rmd file to build
      writeLines(
        c("---", "title: Test", "---", "", "Test content"),
        "test-script.Rmd"
      )

      # Set up build scripts
      projr_yml_git_set(commit = FALSE, push = FALSE)
      yml <- .yml_get("default")
      yml[["build"]][["scripts"]] <- "test-script.Rmd"
      .yml_set(yml, "default")

      # Dev builds are not restricted (only output builds are)
      # So all these should work regardless of restrictions
      expect_no_error(projr_build_dev(scripts = "test-script.Rmd"))

      # Restrict to different branch - dev build still works
      projr_yml_restrictions_set(branch = "different-branch")
      expect_no_error(projr_build_dev(scripts = "test-script.Rmd"))

      # Remove restrictions
      projr_yml_restrictions_set(branch = TRUE)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("branch restriction check logic works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Get current branch
      current_branch <- .git_branch_get()
      expect_true(!is.null(current_branch))

      # Test 1: No restrictions - check should pass
      expect_silent(.build_check_branch_restriction())

      # Test 2: Restrict to current branch - check should pass
      projr_yml_restrictions_set(branch = current_branch)
      expect_silent(.build_check_branch_restriction())

      # Test 3: Restrict to different branch - check should fail
      projr_yml_restrictions_set(branch = "different-branch")
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted to specific branches"
      )

      # Test 4: Allow on multiple branches including current
      projr_yml_restrictions_set(branch = c("main", "dev", current_branch))
      expect_silent(.build_check_branch_restriction())

      # Test 5: Restrict all branches (FALSE/empty)
      projr_yml_restrictions_set(branch = FALSE)
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted on all branches"
      )

      # Test 6: Remove restrictions - check should pass again
      projr_yml_restrictions_set(branch = TRUE)
      expect_silent(.build_check_branch_restriction())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("production checks both one-off archives before publishing or running scripts", {
  skip_if(.is_test_select())
  .test_local_github_releases("archive")
  project <- withr::local_tempdir()
  usethis::with_project(
    project,
    {
      config <- list(
        directories = list(
          `raw-data` = list(path = "inputs"),
          cache = list(path = "_cache")
        ),
        remotes = list(local = list(reader = list(path = "reference"))),
        build = list(git = FALSE, scripts = "analysis.R")
      )
      dir.create("inputs")
      writeLines("original", "inputs/sample.txt")
      dir.create("reference")
      writeLines("Project: 99.0.0", "reference/VERSION")
      writeLines("file.create('ran-script')", "analysis.R")
      config$remotes$github <- list(
        fresh = list(),
        unavailable = list(repo = "fixture/missing")
      )
      config$build$restrictions <- list(
        not_behind_remote = list(github = c("fresh", "unavailable"))
      )
      yaml::write_yaml(config, "_projr.yml")
      error <- expect_error(projr_build_patch(project = project))
      expect_match(conditionMessage(error), "github\\.fresh.*empty")
      expect_match(
        conditionMessage(error),
        "github\\.unavailable.*Cannot read GitHub repository 'fixture/missing'"
      )
      expect_false(file.exists("VERSION"))
      expect_false(file.exists("ran-script"))
      config$build$restrictions <- NULL
      yaml::write_yaml(config, "_projr.yml")
      projr_version_set("0.0.0-1")

      # Both requested archives are initially empty. The registered reader is ahead.
      projr_build_patch(
        archive_local = TRUE,
        archive_github = TRUE,
        project = project
      )
      expect_identical(.restore_build_version(NULL), "v0.0.1")
      expect_true(file.exists("_archive/VERSION"))
      expect_true(
        "VERSION.zip" %in%
          .remote_ls_final_github_httr("fixture/project", "archive")
      )

      writeLines("Project: 0.8.0", "_archive/VERSION")
      .remote_write_version_file("github", c(tag = "archive"), "Project: 0.9.0")
      dir.create("partial")
      writeLines("unidentified payload", "partial/data.txt")
      config$remotes$local$incomplete <- list(path = "partial")
      config$build$local <- list(incomplete = list(content = "raw-data"))
      config$build$github <- list(fresh = list(content = "raw-data"))
      yaml::write_yaml(config, "_projr.yml")
      unlink("ran-script")
      before <- readLines("VERSION")
      error <- expect_error(
        projr_build_patch(
          archive_local = TRUE,
          archive_github = TRUE,
          project = project
        )
      )
      report <- conditionMessage(error)
      expect_match(report, "local\\.archive.*0\\.8\\.0")
      expect_match(report, "github\\.archive.*0\\.9\\.0")
      expect_match(report, "github\\.fresh.*empty")
      expect_match(report, "local\\.incomplete.*VERSION")
      expect_false(grepl("99.0.0", report, fixed = TRUE))
      expect_identical(readLines("VERSION"), before)
      expect_false(file.exists("ran-script"))
      expect_false(.remote_check_exists("github", "fresh"))

      # Disabling the guard explicitly allows publishing to an archive that is ahead.
      config$build$github <- NULL
      config$build$local <- NULL
      config$build$restrictions <- list(not_behind_remote = FALSE)
      yaml::write_yaml(config, "_projr.yml")
      projr_build_patch(archive_local = TRUE, project = project)
      expect_true(file.exists("ran-script"))
      expect_identical(.restore_build_version(NULL), "v0.0.2")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("an explicit read-only restriction distinguishes completed and proposed versions", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  usethis::with_project(
    project,
    {
      config <- list(
        directories = list(
          `raw-data` = list(path = "inputs"),
          cache = list(path = "_cache")
        ),
        remotes = list(
          local = list(
            baseline = list(path = "reference"),
            publisher = list(path = "published")
          )
        ),
        build = list(
          git = FALSE,
          scripts = "analysis.R",
          local = list(publisher = list(content = "raw-data")),
          restrictions = list(not_behind_remote = "baseline", rule = "base")
        )
      )
      yaml::write_yaml(config, "_projr.yml")
      dir.create("inputs")
      writeLines("data", "inputs/sample.txt")
      dir.create("reference")
      writeLines("Project: 1.10.0", "reference/VERSION")
      dir.create("published")
      writeLines("Project: 99.0.0", "published/VERSION")
      writeLines("file.create('ran-script')", "analysis.R")
      config$remotes$github <- list(baseline = list())
      yaml::write_yaml(config, "_projr.yml")
      expect_error(
        projr_build_patch(project = project),
        "[Aa]mbiguous.*baseline|baseline.*[Aa]mbiguous"
      )
      config$remotes$github <- NULL
      config$build$restrictions$not_behind_remote <- "misspelled"
      yaml::write_yaml(config, "_projr.yml")
      expect_error(projr_build_patch(project = project), "misspelled")
      config$build$restrictions$not_behind_remote <- "baseline"
      yaml::write_yaml(config, "_projr.yml")
      expect_error(projr_build_patch(project = project), "1\\.10\\.0")
      expect_false(file.exists("VERSION"))
      expect_false(file.exists("ran-script"))

      fs::dir_create(".projr")
      writeLines(
        c("# BUILDLOG", "", "### v1.9.0: Published"),
        ".projr/BUILDLOG.md"
      )
      projr_version_set("1.9.0-1")

      expect_error(projr_build_major(project = project), "1\\.10\\.0")
      expect_identical(projr_version_get(), "1.9.0-1")
      expect_false(file.exists("ran-script"))
      projr_build_dev(project = project)
      expect_true(file.remove("ran-script"))

      config$build$restrictions <- list(
        not_behind_remote = list(local = "baseline"),
        rule = "target_version_not_behind"
      )
      yaml::write_yaml(config, "_projr.yml")
      # The minor bump equals the remote; the major bump overtakes it.
      expect_error(projr_build_minor(project = project), "1\\.10\\.0")
      expect_false(file.exists("ran-script"))
      projr_build_major(project = project)
      expect_identical(.restore_build_version(NULL), "v2.0.0")
      expect_identical(readLines("reference/VERSION"), "Project: 1.10.0")
      expect_true("Project: 2.0.0" %in% readLines("published/VERSION"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
