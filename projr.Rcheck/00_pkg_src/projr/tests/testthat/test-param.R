test_that("projr_yml_par_add adds parameters key", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test that parameters key doesn't exist initially
      yml_before <- .yml_get("default")
      expect_false("parameters" %in% names(yml_before))
      
      # Add parameters key
      result <- projr_yml_par_add("default")
      expect_true(result)
      
      # Verify parameters key was added
      yml_after <- .yml_get("default")
      expect_true("parameters" %in% names(yml_after))
      expect_identical(yml_after[["parameters"]], list())
      
      # Test that adding again returns FALSE (already exists)
      result2 <- projr_yml_par_add("default")
      expect_false(result2)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_par_add works with NULL profile", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with NULL profile (should default to "default")
      result <- projr_yml_par_add(NULL)
      expect_true(result)
      
      yml <- .yml_get("default")
      expect_true("parameters" %in% names(yml))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_par_add handles alternative param names", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test that it won't add if "param" already exists
      yml <- .yml_get("default")
      yml[["param"]] <- list(test = "value")
      .yml_set(yml, "default")
      
      result <- projr_yml_par_add("default")
      expect_false(result)
      
      yml_after <- .yml_get("default")
      expect_true("param" %in% names(yml_after))
      expect_false("parameters" %in% names(yml_after))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get returns NULL when no parameters exist", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # No parameters key exists
      result <- projr_par_get()
      expect_null(result)
      
      result2 <- projr_par_get("a", "b")
      expect_null(result2)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get returns entire parameters list when no path given", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add parameters with some values
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(
        a = list(b = "value_b", c = "value_c"),
        d = "value_d"
      )
      .yml_set(yml, "default")
      
      # Get entire parameters list
      result <- projr_par_get()
      expect_identical(result, yml[["parameters"]])
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get retrieves nested values correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add nested parameters
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(
        a = list(
          b = list(
            c = "value_c",
            d = 123
          ),
          e = "value_e"
        ),
        f = "value_f"
      )
      .yml_set(yml, "default")
      
      # Test single level
      result_f <- projr_par_get("f")
      expect_identical(result_f, "value_f")
      
      # Test two levels
      result_e <- projr_par_get("a", "e")
      expect_identical(result_e, "value_e")
      
      # Test three levels - string
      result_c <- projr_par_get("a", "b", "c")
      expect_identical(result_c, "value_c")
      
      # Test three levels - numeric
      result_d <- projr_par_get("a", "b", "d")
      expect_identical(result_d, 123)
      
      # Test intermediate list
      result_b <- projr_par_get("a", "b")
      expect_identical(result_b, list(c = "value_c", d = 123))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get returns NULL for nonexistent keys", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add parameters
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(a = list(b = "value_b"))
      .yml_set(yml, "default")
      
      # Test nonexistent top level
      result <- projr_par_get("nonexistent")
      expect_null(result)
      
      # Test nonexistent nested
      result2 <- projr_par_get("a", "nonexistent")
      expect_null(result2)
      
      # Test deeper nonexistent
      result3 <- projr_par_get("a", "b", "c")
      expect_null(result3)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get works with alternative parameter key names", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with "param" key
      yml <- .yml_get("default")
      yml[["param"]] <- list(a = "value_a")
      .yml_set(yml, "default")
      
      result <- projr_par_get("a")
      expect_identical(result, "value_a")
      
      # Test with "params" key
      yml[["params"]] <- list(b = "value_b")
      yml[["param"]] <- NULL
      .yml_set(yml, "default")
      
      result2 <- projr_par_get("b")
      expect_identical(result2, "value_b")
      
      # Test with "par" key
      yml[["par"]] <- list(c = "value_c")
      yml[["params"]] <- NULL
      .yml_set(yml, "default")
      
      result3 <- projr_par_get("c")
      expect_identical(result3, "value_c")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".par_get_list handles parameter key variations", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test that different parameter key names work correctly
      # and only one is present at a time (which is the normal case)
      
      # Test with "parameters"
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(a = "value_a")
      .yml_set(yml, "default")
      result <- projr_par_get("a")
      expect_identical(result, "value_a")
      
      # Test with "param" (overwriting previous)
      yml <- .yml_get("default")
      yml[["parameters"]] <- NULL
      yml[["param"]] <- list(b = "value_b")
      .yml_set(yml, "default")
      result2 <- projr_par_get("b")
      expect_identical(result2, "value_b")
      
      # Verify old key is gone
      result3 <- projr_par_get("a")
      expect_null(result3)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_param_get is an alias for projr_par_get", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add parameters
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(
        test = list(
          value = "test_value"
        )
      )
      .yml_set(yml, "default")
      
      # Both functions should return same result
      result1 <- projr_par_get("test", "value")
      result2 <- projr_param_get("test", "value")
      
      expect_identical(result1, result2)
      expect_identical(result1, "test_value")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get works with profile parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a custom profile with parameters
      projr_profile_create("custom")
      
      yml <- .yml_get("custom")
      yml[["parameters"]] <- list(custom_param = "custom_value")
      .yml_set(yml, "custom")
      
      # Get from custom profile
      result <- projr_par_get("custom_param", profile = "custom")
      expect_identical(result, "custom_value")
      
      # Verify default profile doesn't have this
      result_default <- projr_par_get("custom_param", profile = "default")
      expect_null(result_default)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_par_get handles various data types", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Add parameters with different data types
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(
        string = "text",
        number = 42,
        float = 3.14,
        logical = TRUE,
        vector = c(1, 2, 3),
        list_val = list(x = 1, y = 2),
        null_val = NULL
      )
      .yml_set(yml, "default")
      
      # Test each type
      expect_identical(projr_par_get("string"), "text")
      expect_identical(projr_par_get("number"), 42)
      expect_identical(projr_par_get("float"), 3.14)
      expect_identical(projr_par_get("logical"), TRUE)
      expect_identical(projr_par_get("vector"), c(1, 2, 3))
      expect_identical(projr_par_get("list_val"), list(x = 1, y = 2))
      expect_null(projr_par_get("null_val"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_par_add validates input", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with invalid profile type (should error due to .assert_chr)
      expect_error(projr_yml_par_add(123))
      expect_error(projr_yml_par_add(c("a", "b")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".par_get_option handles edge cases correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test internal function directly
      par_list <- list(
        a = list(
          b = list(
            c = "deep_value"
          )
        ),
        single = "simple_value"
      )
      
      # Test single level access
      result <- .par_get_option(par_list, "single")
      expect_identical(result, "simple_value")
      
      # Test deep nested access
      result2 <- .par_get_option(par_list, c("a", "b", "c"))
      expect_identical(result2, "deep_value")
      
      # Test nonexistent key returns NULL
      result3 <- .par_get_option(par_list, "nonexistent")
      expect_null(result3)
      
      # Test partial path to nonexistent
      result4 <- .par_get_option(par_list, c("a", "nonexistent"))
      expect_null(result4)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("param functions work in realistic scenario", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Realistic scenario: API configuration
      projr_yml_par_add()
      
      yml <- .yml_get("default")
      yml[["parameters"]] <- list(
        api = list(
          base_url = "https://api.example.com",
          version = "v1",
          endpoints = list(
            users = "/users",
            posts = "/posts"
          ),
          timeout = 30
        ),
        database = list(
          host = "localhost",
          port = 5432
        )
      )
      .yml_set(yml, "default")
      
      # Retrieve various parameters
      expect_identical(
        projr_par_get("api", "base_url"),
        "https://api.example.com"
      )
      expect_identical(
        projr_par_get("api", "endpoints", "users"),
        "/users"
      )
      expect_identical(
        projr_par_get("database", "port"),
        5432
      )
      
      # Get entire sections
      api_config <- projr_par_get("api")
      expect_true(is.list(api_config))
      expect_identical(api_config$version, "v1")
      expect_identical(api_config$timeout, 30)
    },
    force = TRUE,
    quiet = TRUE
  )
})
