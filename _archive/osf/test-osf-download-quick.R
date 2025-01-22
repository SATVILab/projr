test_that(".projr_osf_get_path_dir_sub works", {
  # skips
  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
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
          label = "abc", path_append_label = NULL, path = NULL
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

test_that(".projr_osf_complete_dnld_list works", {
  # skips
  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(
        .projr_osf_complete_dnld_list(NULL),
        list("conflict" = "overwrite", "strategy" = "download-all")
      )
      expect_identical(
        .projr_osf_complete_dnld_list(list("conflict" = "abc")),
        list("conflict" = "abc", "strategy" = "download-all")
      )
      expect_identical(
        .projr_osf_complete_dnld_list(
          list("conflict" = "abc", "strategy" = "def")
        ),
        list("conflict" = "abc", "strategy" = "def")
      )
      expect_identical(
        .projr_osf_complete_dnld_list(list("strategy" = "def")),
        list("conflict" = "overwrite", "strategy" = "def")
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
