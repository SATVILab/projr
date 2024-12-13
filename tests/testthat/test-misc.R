test_that(".projr_local_dir_create works", {
  skip_if(.is_test_select())
  # skips

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_false(.projr_run_output_check(bump_component = NULL))
      expect_false(.projr_run_output_check(bump_component = FALSE))
      expect_false(.projr_run_output_check(bump_component = "dev"))
      expect_true(.projr_run_output_check(bump_component = "patch"))
      expect_true(.projr_run_output_check(bump_component = "minor"))
      expect_true(.projr_run_output_check(bump_component = "major"))
      expect_false(.projr_run_output_check(output_run = FALSE))
      expect_true(.projr_run_output_check(output_run = TRUE))
      expect_true(.projr_run_output_check(
        output_run = TRUE, bump_component = FALSE
      ))
    }
  )
})

test_that("projr_use_data works", {
  skip_if(.is_test_select())
  # skips

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {

      # saving one object
      x <- "1"
      path_tmp <- projr_use_data(x)
      expect_true(fs::path_has_parent(path_tmp, "_tmp"))
      expect_true(file.exists(path_tmp))
      path_final <- projr_use_data(x, safe = FALSE)
      expect_true(fs::path_has_parent(path_final, "data/"))
      expect_true(file.exists("data/x.rda"))
      invisible(file.remove(path_tmp))
      invisible(file.remove(path_final))
      
      # saving multiple objects
      y <- "c"
      paths_tmp <- projr_use_data(x, y)
      expect_identical(length(paths_tmp), 2L)
      expect_true(all(file.exists(paths_tmp)))
      expect_true(all(fs::path_has_parent(path_tmp, "_tmp")))
      paths_final <- projr_use_data(x, y)
      expect_identical(length(paths_final), 2L)
      expect_true(all(file.exists(paths_final)))
      expect_true(all(fs::path_has_parent(paths_final, "_tmp")))

    }
  )
})

test_that(".projr_list_add_list works", {
  skip_if(.is_test_select())
  expect_true(
    "a" %in% names(
      .projr_list_add_list(
        x = "val", nm = "a", list_base = list(b = 2)
      )
    )
  )
})
