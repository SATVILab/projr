test_that(".projr_restore_osf works", {
  # skips
  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      browser()
      expect_null(.projr_osf_get_path_dir_sub(
        label = "abc", path_append_label = NULL, path = NULL
      ))
      expect_null(.projr_osf_get_path_dir_sub(
        label = "abc", path_append_label = FALSE, path = NULL
      ))
      expect_identical(
        .projr_osf_get_path_dir_sub(
          label = "abc", path_append_label = TRUE, path = NULL
        ),
        "abc"
      )
      expect_identical(
        .projr_osf_get_path_dir_sub(
          label = "abc", path_append_label = TRUE, path = "def/ghi"
        ),
        "def/ghi/abc"
      )
      expect_identical(
        .projr_osf_get_path_dir_sub(
          label = "abc", path_append_label = FALSE, path = "def/ghi"
        ),
        "def/ghi"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
