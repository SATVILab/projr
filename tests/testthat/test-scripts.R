test_that(".yml_scripts functions work", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: Direct list format
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("script1.qmd", "script2.R")
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        .yml_scripts_get_build("default"),
        c("script1.qmd", "script2.R")
      )
      
      # Test 2: Dev key format (new structure without "build" sub-key)
      yaml::write_yaml(
        list(
          build = list(
            scripts = list(
              "full-analysis.qmd",
              dev = c("quick-test.qmd", "debug.R")
            )
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        .yml_scripts_get_build("default"),
        c("full-analysis.qmd")
      )
      expect_identical(
        .yml_scripts_get_dev("default"),
        c("quick-test.qmd", "debug.R")
      )
      
      # Test 3: Dev falls back to build when no dev key
      yaml::write_yaml(
        list(
          build = list(
            scripts = c("analysis.qmd")
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        .yml_scripts_get_dev("default"),
        c("analysis.qmd")
      )
      
      # Test 4: NULL when not specified
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

test_that(".engine_get_from_files works correctly", {
  skip_if(.is_test_select())
  
  # Test with .qmd files
  expect_identical(
    .engine_get_from_files(c("analysis.qmd", "report.qmd")),
    "quarto_document"
  )
  
  # Test with .Rmd files
  expect_identical(
    .engine_get_from_files(c("analysis.Rmd", "report.Rmd")),
    "rmd"
  )
  
  # Test with .R files
  expect_identical(
    .engine_get_from_files(c("script.R")),
    "rmd"
  )
  
  # Test with mixed files - qmd takes priority
  expect_identical(
    .engine_get_from_files(c("analysis.qmd", "report.Rmd")),
    "quarto_document"
  )
  
  # Test with NULL
  expect_identical(
    .engine_get_from_files(NULL),
    .engine_get()
  )
})

test_that("dev.scripts overrides build.scripts for dev builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Test 1: dev.scripts overrides build.scripts
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
      
      # Production builds should still use build.scripts
      expect_identical(
        .yml_scripts_get_build("default"),
        c("build1.qmd", "build2.qmd")
      )
      
      # Test 2: dev.scripts overrides build.scripts.dev
      yaml::write_yaml(
        list(
          build = list(
            scripts = list(
              "build1.qmd",
              dev = "build-dev.qmd"
            )
          ),
          dev = list(
            scripts = c("override-dev.qmd")
          )
        ),
        "_projr.yml"
      )
      
      # dev.scripts should win over build.scripts.dev
      expect_identical(
        .yml_scripts_get_dev("default"),
        c("override-dev.qmd")
      )
      
      # Test 3: Falls back to build.scripts.dev when no dev.scripts
      yaml::write_yaml(
        list(
          build = list(
            scripts = list(
              "build1.qmd",
              dev = c("fallback-dev.qmd", "fallback2.R")
            )
          )
        ),
        "_projr.yml"
      )
      
      expect_identical(
        .yml_scripts_get_dev("default"),
        c("fallback-dev.qmd", "fallback2.R")
      )
    }
  )
})

test_that("File existence checks work for scripts and hooks", {
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
        .yml_scripts_hooks_check_exist("default"),
        "Build script 'nonexistent.qmd' does not exist"
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
        .yml_scripts_hooks_check_exist("default"),
        "Dev script 'nonexistent-dev.R' does not exist"
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
        .yml_scripts_hooks_check_exist("default"),
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
      
      expect_silent(.yml_scripts_hooks_check_exist("default"))
    }
  )
})
