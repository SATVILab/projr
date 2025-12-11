test_that(".build_pre_script_run and .build_post_script_run work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup test scripts
      cat("x <- 1; saveRDS(x, 'pre.rds')", file = "pre-script.R")
      cat("y <- 2; saveRDS(y, 'post.rds')", file = "post-script.R")

      # Add pre-script
      .yml_script_add(
        path = "pre-script.R",
        title = "pre-test",
        stage = "pre"
      )

      # Add post-script
      .yml_script_add(
        path = "post-script.R",
        title = "post-test",
        stage = "post"
      )

      # Test .build_pre_script_run()
      .build_pre_script_run()
      expect_true(file.exists("pre.rds"))
      expect_false(file.exists("post.rds"))

      # Clean up
      unlink("pre.rds")

      # Test .build_post_script_run()
      .build_post_script_run()
      expect_false(file.exists("pre.rds"))
      expect_true(file.exists("post.rds"))

      # Clean up
      unlink("post.rds")
      unlink("pre-script.R")
      unlink("post-script.R")
    }
  )
})

test_that(".build_script_run works with empty configuration", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure no scripts are configured
      .yml_script_rm_all()

      # Should return FALSE and not error
      result <- .build_script_run(stage = "pre")
      expect_identical(result, FALSE)

      result <- .build_script_run(stage = "post")
      expect_identical(result, FALSE)
    }
  )
})

test_that(".script_run errors when file doesn't exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with non-existent file
      expect_error(
        .script_run("non-existent-script.R"),
        "does not exist"
      )
    }
  )
})

test_that(".script_run executes script in isolated environment", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test script that sets variables
      cat("test_var <- 'isolated'; test_num <- 42", file = "test-script.R")

      # Ensure variables don't exist in global env
      if (exists("test_var", inherits = FALSE)) rm(test_var)
      if (exists("test_num", inherits = FALSE)) rm(test_num)

      # Run script
      .script_run("test-script.R")

      # Variables should NOT be in global environment
      expect_false(exists("test_var", inherits = FALSE))
      expect_false(exists("test_num", inherits = FALSE))

      # Clean up
      unlink("test-script.R")
    }
  )
})

test_that(".build_script_run_title filters by stage correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test scripts
      cat("saveRDS('pre', 'pre.rds')", file = "pre-script.R")
      cat("saveRDS('post', 'post.rds')", file = "post-script.R")

      # Test with pre-stage script
      x_pre <- list(
        stage = "pre",
        path = "pre-script.R"
      )

      # Should run when stage matches
      .build_script_run_title(x_pre, stage = "pre")
      expect_true(file.exists("pre.rds"))
      unlink("pre.rds")

      # Should NOT run when stage doesn't match
      .build_script_run_title(x_pre, stage = "post")
      expect_false(file.exists("pre.rds"))

      # Test with post-stage script
      x_post <- list(
        stage = "post",
        path = "post-script.R"
      )

      # Should run when stage matches
      .build_script_run_title(x_post, stage = "post")
      expect_true(file.exists("post.rds"))
      unlink("post.rds")

      # Should NOT run when stage doesn't match
      .build_script_run_title(x_post, stage = "pre")
      expect_false(file.exists("post.rds"))

      # Clean up
      unlink("pre-script.R")
      unlink("post-script.R")
    }
  )
})

test_that(".build_script_run_title handles multiple paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create multiple test scripts
      cat("saveRDS('script1', 'script1.rds')", file = "script1.R")
      cat("saveRDS('script2', 'script2.rds')", file = "script2.R")
      cat("saveRDS('script3', 'script3.rds')", file = "script3.R")

      # Test with multiple paths
      x <- list(
        stage = "pre",
        path = c("script1.R", "script2.R", "script3.R")
      )

      .build_script_run_title(x, stage = "pre")

      # All scripts should have run
      expect_true(file.exists("script1.rds"))
      expect_true(file.exists("script2.rds"))
      expect_true(file.exists("script3.rds"))

      # Clean up
      unlink("script1.rds")
      unlink("script2.rds")
      unlink("script3.rds")
      unlink("script1.R")
      unlink("script2.R")
      unlink("script3.R")
    }
  )
})

test_that(".build_script_run handles multiple titles with different stages", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create test scripts
      cat("saveRDS('pre1', 'pre1.rds')", file = "pre1.R")
      cat("saveRDS('pre2', 'pre2.rds')", file = "pre2.R")
      cat("saveRDS('post1', 'post1.rds')", file = "post1.R")
      cat("saveRDS('post2', 'post2.rds')", file = "post2.R")

      # Add multiple titles with different stages
      .yml_script_add(
        path = "pre1.R",
        title = "pre-title-1",
        stage = "pre"
      )
      .yml_script_add(
        path = "pre2.R",
        title = "pre-title-2",
        stage = "pre"
      )
      .yml_script_add(
        path = "post1.R",
        title = "post-title-1",
        stage = "post"
      )
      .yml_script_add(
        path = "post2.R",
        title = "post-title-2",
        stage = "post"
      )

      # Run pre-stage scripts
      .build_script_run(stage = "pre")
      expect_true(file.exists("pre1.rds"))
      expect_true(file.exists("pre2.rds"))
      expect_false(file.exists("post1.rds"))
      expect_false(file.exists("post2.rds"))

      # Clean up pre files
      unlink("pre1.rds")
      unlink("pre2.rds")

      # Run post-stage scripts
      .build_script_run(stage = "post")
      expect_false(file.exists("pre1.rds"))
      expect_false(file.exists("pre2.rds"))
      expect_true(file.exists("post1.rds"))
      expect_true(file.exists("post2.rds"))

      # Clean up
      unlink("post1.rds")
      unlink("post2.rds")
      unlink("pre1.R")
      unlink("pre2.R")
      unlink("post1.R")
      unlink("post2.R")
    }
  )
})

test_that(".script_run returns invisible NULL", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple script
      cat("x <- 1", file = "simple.R")

      # Run and check return value
      result <- .script_run("simple.R")
      expect_null(result)

      # Clean up
      unlink("simple.R")
    }
  )
})

test_that(".build_script_run_title returns FALSE when stage doesn't match", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a script
      cat("x <- 1", file = "test.R")

      x <- list(
        stage = "pre",
        path = "test.R"
      )

      # Should return FALSE when stage doesn't match
      result <- .build_script_run_title(x, stage = "post")
      expect_identical(result, FALSE)

      # Clean up
      unlink("test.R")
    }
  )
})

test_that(".build_script_run processes scripts in order", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create scripts that append to a file to track execution order
      cat("cat('1\\n', file = 'order.txt', append = TRUE)", file = "script1.R")
      cat("cat('2\\n', file = 'order.txt', append = TRUE)", file = "script2.R")
      cat("cat('3\\n', file = 'order.txt', append = TRUE)", file = "script3.R")

      # Add scripts with multiple paths
      .yml_script_add(
        path = c("script1.R", "script2.R", "script3.R"),
        title = "ordered-scripts",
        stage = "pre"
      )

      # Remove existing order file if it exists
      if (file.exists("order.txt")) unlink("order.txt")

      # Run scripts
      .build_script_run(stage = "pre")

      # Check execution order
      expect_true(file.exists("order.txt"))
      order_content <- readLines("order.txt")
      expect_identical(order_content, c("1", "2", "3"))

      # Clean up
      unlink("order.txt")
      unlink("script1.R")
      unlink("script2.R")
      unlink("script3.R")
    }
  )
})
