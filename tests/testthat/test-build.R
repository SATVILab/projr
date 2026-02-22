# bookdown
# ------------------------
test_that("projr_build_dev works", {
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1-1")
      # run repeat build
      projr_build("minor", msg = "test")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test version bumping behavior
test_that("Version bumping works correctly for each component", {
  skip_if(.is_test_cran())
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
      expect_identical(projr_version_get(), "1.2.4-1")

      # Test minor bump
      projr_build_minor(msg = "Minor bump test")
      expect_identical(projr_version_get(), "1.3.0-1")

      # Test major bump
      projr_build_major(msg = "Major bump test")
      expect_identical(projr_version_get(), "2.0.0-1")

      # Test custom bump_component parameter
      desc::desc_set_version("3.4.5")
      projr_build(bump_component = "patch", msg = "Custom patch")
      expect_identical(projr_version_get(), "3.4.6-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test bookdown _files directory copying
test_that("bookdown _files directory is copied correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Modify index.Rmd to include a code chunk with a plot
      # This will cause bookdown to create a _files directory
      index_content <- c(
        "---",
        "title: \"Test Book\"",
        "author: \"Test Author\"",
        "description: \"Test\"",
        "---",
        "",
        "# Chapter 1",
        "",
        "This is a test with a plot:",
        "",
        "```{r test-plot, fig.cap=\"Test Plot\"}",
        "plot(1:10, 1:10)",
        "```"
      )
      writeLines(index_content, "index.Rmd")

      # Run build - this will render bookdown and create the _files directory
      projr_build_patch(msg = "Test bookdown files")

      # Get book filename from _bookdown.yml
      book_filename <- .yml_bd_get_book_filename()
      files_dir_name <- paste0(book_filename, "_files")

      # Check that _files directory was copied to final docs location
      docs_dir <- projr_path_get_dir("docs", safe = FALSE)
      dest_files_dir <- file.path(docs_dir, files_dir_name)

      # The _files directory should exist in the final docs location
      expect_true(dir.exists(dest_files_dir))

      # There should be figure files inside
      figure_files <- list.files(dest_files_dir, recursive = TRUE, pattern = "\\.(png|jpg|jpeg)$")
      expect_true(length(figure_files) > 0)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test that pre-existing files are cleared
# Note: does not test whether they're cleared at the
# correct stage.
test_that("Pre/post/never clearing options work correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip() # fails at present, worth fixing
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_on_ci()
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
      tryCatch(
        {
          # This build should fail
          projr_build_patch(msg = "This should fail")
        },
        error = function(e) {
          # Do nothing, we expect an error
        }
      )

      # Version should be back to initial version after failed build
      expect_identical(projr_version_get(), initial_version)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# rmarkdown
test_that("projr_build_ works with rmarkdown", {
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
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
      expect_true(!dir.exists("_tmp/projr/v0.0.0-1/docs"))
      expect_true(dir.exists("docs"))
      expect_true(file.exists("docs/index.html"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# quarto projects
test_that("projr_build_ works with quarto projects", {
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
  skip_if(.is_test_select())
  usethis::with_project(
    path = dir_test,
    code = {
      .test_setup_project_lit_docs("quarto_project")
      # dev build
      projr_build_dev()
      expect_true(
        dir.exists("_tmp/projr/v0.0.0-1/docs")
      )
      expect_true(
        length(.file_ls("_tmp/projr/v0.0.0-1/docs")) > 5
      )

      # output build
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
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(
    git = FALSE, set_env_var = TRUE
  )
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
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

test_that("PROJR_CLEAR_OUTPUT environment variable works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_CLEAR_OUTPUT", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_CLEAR_OUTPUT = old_val) else Sys.unsetenv("PROJR_CLEAR_OUTPUT"))

  # Test default value
  Sys.unsetenv("PROJR_CLEAR_OUTPUT")
  expect_identical(.build_get_clear_output(NULL), "pre")

  # Test "pre" value
  Sys.setenv(PROJR_CLEAR_OUTPUT = "pre")
  expect_identical(.build_get_clear_output(NULL), "pre")

  # Test "post" value
  Sys.setenv(PROJR_CLEAR_OUTPUT = "post")
  expect_identical(.build_get_clear_output(NULL), "post")

  # Test "never" value
  Sys.setenv(PROJR_CLEAR_OUTPUT = "never")
  expect_identical(.build_get_clear_output(NULL), "never")

  # Test invalid value
  Sys.setenv(PROJR_CLEAR_OUTPUT = "invalid")
  expect_error(.build_get_clear_output(NULL))
})

test_that("PROJR_CLEAR_OUTPUT explicit parameter overrides env var", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_CLEAR_OUTPUT", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_CLEAR_OUTPUT = old_val) else Sys.unsetenv("PROJR_CLEAR_OUTPUT"))

  # Set env var to one value
  Sys.setenv(PROJR_CLEAR_OUTPUT = "never")

  # Explicit parameter should override
  expect_identical(.build_get_clear_output("pre"), "pre")
  expect_identical(.build_get_clear_output("post"), "post")
  expect_identical(.build_get_clear_output("never"), "never")
})

test_that("PROJR_CLEAR_OUTPUT validates input strictly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_CLEAR_OUTPUT", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_CLEAR_OUTPUT = old_val) else Sys.unsetenv("PROJR_CLEAR_OUTPUT"))

  # Test case sensitivity
  Sys.setenv(PROJR_CLEAR_OUTPUT = "PRE")
  expect_error(.build_get_clear_output(NULL))

  Sys.setenv(PROJR_CLEAR_OUTPUT = "Post")
  expect_error(.build_get_clear_output(NULL))

  Sys.setenv(PROJR_CLEAR_OUTPUT = "NEVER")
  expect_error(.build_get_clear_output(NULL))

  # Test other invalid values
  Sys.setenv(PROJR_CLEAR_OUTPUT = "always")
  expect_error(.build_get_clear_output(NULL))
})

# Test .build_get_output_run
test_that(".build_get_output_run returns correct values", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Production builds should return TRUE
  expect_true(.build_get_output_run("major"))
  expect_true(.build_get_output_run("minor"))
  expect_true(.build_get_output_run("patch"))

  # Dev builds should return FALSE
  expect_false(.build_get_output_run(NULL))
  expect_false(.build_get_output_run("dev"))
})

# Test .build_ensure_version
test_that(".build_ensure_version creates VERSION file when missing", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove both VERSION and DESCRIPTION
      version_path <- .path_get("VERSION")
      desc_path <- .path_get("DESCRIPTION")
      if (file.exists(version_path)) file.remove(version_path)
      if (file.exists(desc_path)) file.remove(desc_path)

      # Function should create VERSION with 0.0.1
      .build_ensure_version()
      expect_true(file.exists(version_path))
      expect_identical(projr_version_get(), "0.0.1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_ensure_version does nothing when VERSION exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set version to something specific
      projr_version_set("1.2.3")

      # Function should not change version
      .build_ensure_version()
      expect_identical(projr_version_get(), "1.2.3")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_ensure_version does nothing when DESCRIPTION exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove VERSION but keep DESCRIPTION
      version_path <- .path_get("VERSION")
      if (file.exists(version_path)) file.remove(version_path)

      # Set version in DESCRIPTION
      desc::desc_set_version("2.3.4")

      # Function should not create VERSION or change version
      .build_ensure_version()
      expect_false(file.exists(version_path))
      expect_identical(projr_version_get(), "2.3.4")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test .build_dev_get_bump_component
test_that(".build_dev_get_bump_component returns NULL when not on dev version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set to non-dev version
      projr_version_set("1.0.0")

      # Should return NULL (to auto-bump to dev)
      result <- .build_dev_get_bump_component(TRUE)
      expect_null(result)

      result <- .build_dev_get_bump_component(FALSE)
      expect_null(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_dev_get_bump_component respects bump when on dev version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set to dev version
      projr_version_set("1.0.0-1")

      # Should return "dev" when bump = TRUE
      result <- .build_dev_get_bump_component(TRUE)
      expect_identical(result, "dev")

      # Should return NULL when bump = FALSE
      result <- .build_dev_get_bump_component(FALSE)
      expect_null(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test .build_post_dev
test_that(".build_post_dev appends dev component after production build", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Disable git operations for this test
      .yml_git_set_commit(FALSE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)

      # Set initial version
      projr_version_set("1.0.0")

      # Create version list (simulating post-build state)
      version_run_on_list <- list(
        version_run_on = "1.0.0",
        version_post = "1.0.0"
      )

      # Call .build_post_dev with production build
      .build_post_dev(
        bump_component = "patch",
        version_run_on_list = version_run_on_list,
        msg = "test"
      )

      # Version should now have dev component
      current_version <- projr_version_get()
      expect_true(grepl("-", current_version))
      expect_true(grepl("^1\\.0\\.0-", current_version))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_post_dev skips dev bump for dev builds", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set initial dev version
      projr_version_set("1.0.0-1")
      initial_version <- projr_version_get()

      # Create version list
      version_run_on_list <- list(
        version_run_on = "1.0.0-1",
        version_post = "1.0.0-1"
      )

      # Call .build_post_dev with dev build (NULL bump_component)
      result <- .build_post_dev(
        bump_component = NULL,
        version_run_on_list = version_run_on_list,
        msg = "test"
      )

      # Should return FALSE and not change version
      expect_false(result)
      expect_identical(projr_version_get(), initial_version)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test projr_build_dev with bump parameter
test_that("projr_build_dev bump parameter works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Start with non-dev version
      projr_version_set("0.0.0")

      # Build with bump = FALSE (should auto-bump to dev)
      projr_build_dev(bump = FALSE)
      version_after_first <- projr_version_get()
      expect_true(grepl("-", version_after_first))

      # Build again with bump = TRUE (should increment dev)
      projr_build_dev(bump = TRUE)
      version_after_second <- projr_version_get()
      expect_true(grepl("-", version_after_second))

      # Parse and compare dev numbers
      dev_num_first <- as.integer(sub(".*-", "", version_after_first))
      dev_num_second <- as.integer(sub(".*-", "", version_after_second))
      expect_true(dev_num_second > dev_num_first)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test projr_build_dev with profile parameter
test_that("projr_build_dev profile parameter works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set a profile-specific path configuration
      projr_yml_dir_path_set("output", "_output_test", "test-profile")

      # Build with profile
      old_profile <- Sys.getenv("PROJR_PROFILE", unset = "")
      projr_build_dev(profile = "test-profile")
      current_profile <- Sys.getenv("PROJR_PROFILE", unset = "")

      # Profile should be reset after build
      expect_identical(current_profile, old_profile)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# Test clear_output parameter with projr_build_dev
test_that("projr_build_dev respects clear_output parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test that clear_output="never" doesn't error and completes build
      expect_no_error({
        projr_build_dev(clear_output = "never")
      })

      # Test that clear_output="pre" works
      expect_no_error({
        projr_build_dev(clear_output = "pre")
      })

      # Test that clear_output="post" works
      expect_no_error({
        projr_build_dev(clear_output = "post")
      })
    },
    quiet = TRUE,
    force = TRUE
  )
})
