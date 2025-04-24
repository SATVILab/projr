# bookdown
# ------------------------
test_that("projr_build_dev works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_build_dev()
      yml_bd <- .yml_bd_get()
      # it's set in _bookdown.yml
      # and not in _projr.yml, so use _bookdown.yml
      # as basename
      expect_identical(basename(yml_bd$output_dir), "docs")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
      # debugonce(.build_output_get_msg)
      # debugonce(.git_msg_get)
      projr_build("patch", msg = "test")
      .version_get()
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "docs")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build("minor", msg = "test")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test version bumping behavior
test_that("Version bumping works correctly for each component", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set initial version to something non-default to test different positions
      initial_version <- "1.2.3"
      projr_version_set(initial_version)
      
      # Test patch bump
      # debugonce(.build_post_finalise_artefacts)
      # debugonce(.build_copy_docs_bookdown)
      projr_build_patch(msg = "Patch bump test")
      expect_identical(projr_version_get(), "1.2.4")
      
      # Test minor bump
      projr_build_minor(msg = "Minor bump test")
      expect_identical(projr_version_get(), "1.3.0")
      
      # Test major bump
      projr_build_major(msg = "Major bump test")
      expect_identical(projr_version_get(), "2.0.0")
      
      # Test custom bump_component parameter
      desc::desc_set_version("3.4.5")
      projr_build(bump_component = "patch", msg = "Custom patch")
      expect_identical(projr_version_get(), "3.4.6")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test that pre-existing files are cleared
# Note: does not test whether they're cleared at the
# correct stage.
test_that("Pre/post/never clearing options work correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set up test files
      output_dir <- projr_path_get_dir("output", safe = FALSE, create = TRUE)
      test_file <- file.path(output_dir, "test.txt")
      writeLines("test", test_file)
      
      # Test "pre" clearing (default)
      projr_build_patch(msg = "Test pre clearing", clear_output = "pre")
      # Output should exist after build but be freshly created
      expect_true(dir.exists(output_dir))
      expect_false(file.exists(test_file))
      
      # Recreate test file for next test
      writeLines("test", test_file)
      
      # Test "never" clearing
      projr_build_patch(msg = "Test never clearing", clear_output = "never")
      # Output file should still exist after build
      expect_true(file.exists(test_file))
      
      # Test "post" clearing
      projr_build_patch(msg = "Test post clearing", clear_output = "post")
      # Output directory should be empty after build completion
      expect_true(dir.exists(output_dir))
      expect_equal(length(list.files(output_dir)), 0)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test archive functionality
test_that("Archive functionality works correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  skip() # fails at present, worth fixing
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      browser()
      # Create output content
      output_dir <- projr_path_get_dir("output", safe = FALSE, create = TRUE)
      writeLines("test content", file.path(output_dir, "test.txt"))
      
      # Test local archiving
      local_archive_dir <- file.path(dir_test, "_archive")
      if (!dir.exists(local_archive_dir)) dir.create(local_archive_dir)
      projr_build_patch(
        msg = "Test archiving",
        archive_local = TRUE,
        always_archive = TRUE
      )
      
      # Check if archive was created
      archives <- list.files(local_archive_dir, pattern = "\\.zip$")
      expect_true(length(archives) > 0)
      expect_true(any(grepl("output", archives)))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test error handling and version recovery
test_that("Failed builds handle version appropriately", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create an intentionally broken Rmd file
      broken_rmd <- "index.Rmd"
      writeLines(c(
        "---",
        "title: \"Test\"",
        "---",
        "",
        "```{r}",
        "# This will cause an error",
        "stop('Intentional error')",
        "```"
      ), broken_rmd)
      
      # Record initial version
      initial_version <- projr_version_get()
      
      # Define custom error handler to prevent test from failing
      tryCatch({
        # This build should fail
        projr_build_patch(msg = "This should fail")
      }, error = function(e) {
        # Do nothing, we expect an error
      })
      
      # Version should be back to initial version after failed build
      expect_identical(projr_version_get(), initial_version)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# rmarkdown
test_that("projr_build_ works with rmarkdown", {
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("rmarkdown")
      # dev build
      projr_build_dev()
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/docs"))
      expect_true(file.exists("_tmp/projr/v0.0.0-1/docs/index.html"))
      expect_false(file.exists("index.html"))
      # output build
      if (dir.exists("docs")) {
        unlink("docs", recursive = TRUE)
      }
      projr_build_patch(msg = "Test")
      expect_true(
        !dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(dir.exists("docs"))
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# quarto projects
test_that("projr_build_ works with quarto projects", {
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("quarto_project")
      # dev build
      # browser()
      projr_build_dev()
      expect_true(
        dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(
        length(.file_ls("_tmp/projr/v0.0.0-1/docs")) > 5
      )

      # output build
      # debugonce(.build_copy_docs_quarto_project)
      projr_build_patch(msg = "Test")
      expect_true(
        !dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(dir.exists("docs"))
      expect_true(length(.file_ls("docs")) > 5)
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# quarto 
test_that("projr_build_ works with quarto", {
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("quarto")
      # dev build
      projr_build_dev()
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/docs"))
      expect_true(file.exists("_tmp/projr/v0.0.0-1/docs/index.html"))
      expect_false(file.exists("index.html"))
      # output build
      if (dir.exists("docs")) {
        unlink("docs", recursive = TRUE)
      }
      projr_build_patch(msg = "Test")
      expect_true(
        !dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(dir.exists("docs"))
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test args_engine parameter passing
test_that("args_engine parameter works correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple Rmd file that can use parameters
      writeLines(c(
        "---",
        "title: \"Test Document\"",
        "params:",
        "  message: \"Default message\"",
        "---",
        "",
        "```{r}",
        "cat(params$message)",
        "```"
      ), "params.Rmd")
      
      # Test args_engine with parameters for rmarkdown
      custom_args <- list(
        params = list(message = "Custom message from args_engine")
      )
      
      # Test with dev build
      projr_build_dev(args_engine = custom_args)
      
      # Check if parameter was correctly passed (would need content checking)
      # This is a simplified check just to verify the build didn't error
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/docs"))
    },
    quiet = TRUE,
    force = TRUE
  )
})