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
      
      # Test 2: Build sub-key format
      yaml::write_yaml(
        list(
          build = list(
            scripts = list(
              build = c("analysis.qmd", "report.Rmd"),
              pre = "setup.R",
              post = "cleanup.R"
            )
          )
        ),
        "_projr.yml"
      )
      expect_identical(
        .yml_scripts_get_build("default"),
        c("analysis.qmd", "report.Rmd")
      )
      expect_identical(
        .yml_scripts_get_hooks_pre("default"),
        "setup.R"
      )
      expect_identical(
        .yml_scripts_get_hooks_post("default"),
        "cleanup.R"
      )
      
      # Test 3: NULL when not specified
      yaml::write_yaml(
        list(
          build = list()
        ),
        "_projr.yml"
      )
      expect_null(.yml_scripts_get_build("default"))
    }
  )
})

test_that(".yml_hooks functions work", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Add a hook
      projr_yml_hooks_add(
        path = "test-hook.R",
        title = "test-hook",
        stage = "pre",
        profile = "default"
      )
      
      yml_hooks <- .yml_hooks_get("default")
      expect_true(!is.null(yml_hooks))
      expect_identical(yml_hooks[["test-hook"]][["stage"]], "pre")
      expect_identical(yml_hooks[["test-hook"]][["path"]], "test-hook.R")
      
      # Remove the hook
      projr_yml_hooks_rm(title = "test-hook", profile = "default")
      expect_null(.yml_hooks_get("default"))
    }
  )
})
