test_that(".path_* functions work", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # filter
      # -------------------

      expect_equal(.path_filter_spec("a"), "a")
      expect_equal(.path_filter_spec("a/b", "a"), character())
      expect_equal(.path_filter_spec("a/b" |> c("c"), "a"), "c")

      # transform
      # -------------------
      # relative
      expect_equal(
        .path_force_rel("a/b" |> .path_force_abs()), "a/b"
      )
      # absolute
      expect_equal(
        "a/b" |> .path_force_abs(), fs::path_abs("a/b") |> as.character()
      )
      expect_equal(
        "a/b" |> .path_force_abs("c/d/e"),
        fs::path_abs("c/d/e/a/b") |> as.character()
      )
      expect_error(fs:path_abs("a") |> .path_force_abs("b"))
    }
  )
})

test_that(".file_* and .dir_* functions work", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # filter
      # -------------------
      dir_tmp <- .projr_test_setup_content_dir()
      expect_identical(.file_ls(dir_tmp), content_vec_test_file)
      expect_identical(.dir_ls(dir_tmp), content_vec_test_dir)
      expect_identical(
        file.path(dir_tmp, content_vec) |>
          .file_filter_dir() |>
          .path_force_rel(dir_tmp),
        content_vec_test_dir
      )
      expect_identical(
        file.path(dir_tmp, content_vec) |>
          .file_filter_dir_non() |>
          .path_force_rel(dir_tmp),
        content_vec_test_file
      )
      expect_identical(
        .file_ls(dir_tmp, full.names = TRUE) |>
          c("a") |>
          .file_filter_exists() |>
          .path_force_rel(dir_tmp),
        content_vec_test_file
      )
      expect_identical(.file_rm("a"), character())
      expect_identical(
        file.path(dir_tmp, "abc.txt") |>
          .file_rm_single() |>
          .path_force_rel(dir_tmp),
        "abc.txt"
      )
      expect_identical(
        file.path(dir_tmp, "abc.txt") |>
          .file_rm_single() |>
          .path_force_rel(dir_tmp),
        character()
      )
      expect_identical(
        c(
          file.path(dir_tmp, "abc.txt"),
          file.path(dir_tmp, "subdir1/def.txt"),
          file.path(dir_tmp, "subdir1/subdir2/ghi.txt")
        ) |>
          .file_rm() |>
          .path_force_rel(dir_tmp),
        c("subdir1/def.txt", "subdir1/subdir2/ghi.txt")
      )
      browser()

      dir_tmp <- .projr_test_setup_content_dir()
      debugonce(.dir_clear)
      expect_false(.dir_create_tmp_random() |> .dir_clear())
    }
  )
})
