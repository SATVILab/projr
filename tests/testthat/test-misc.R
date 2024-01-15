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
      x <- "1"
      projr_use_data(x)
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
