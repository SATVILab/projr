test_that(".path_* functions work", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
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
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # filter
      # -------------------
      dir_tmp <- .test_setup_content_dir()
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
      # removing
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
      # clearing
      dir_tmp <- .test_setup_content_dir()
      expect_true(.dir_create_tmp_random() |> .dir_clear())
      expect_true(dir_tmp |> .dir_clear())
      dir_tmp <- .test_setup_content_dir()
      .dir_clear(dir_tmp)
      expect_identical(
        dir_tmp |> .dir_ls(),
        character()
      )
      # copying and moving
      # no exclusions
      dir_tmp <- .test_setup_content_dir()
      dir_tmp_2 <- .dir_create_tmp_random()
      file.create(file.path(dir_tmp_2, "f1")) |> invisible()
      .dir_copy_exact(dir_tmp, dir_tmp_2)
      expect_identical(
        dir_tmp |> .dir_ls(),
        dir_tmp_2 |> .dir_ls()
      )
      # exclusions
      dir_tmp <- .test_setup_content_dir()
      .dir_create(file.path(dir_tmp, "d1"))
      file.create(file.path(dir_tmp, "d1", "f2")) |> invisible()
      dir_tmp_2 <- .dir_create_tmp_random()
      file.create(file.path(dir_tmp_2, "f1")) |> invisible()
      .dir_copy_exact(dir_tmp, dir_tmp_2, dir_exc = "d1")
      expect_identical(
        dir_tmp |> .dir_ls() |> setdiff("d1"),
        dir_tmp_2 |> .dir_ls(),
      )
      expect_identical(
        dir_tmp |> .file_ls() |> setdiff("d1/f2"),
        dir_tmp_2 |> .file_ls(),
      )
      # removing initial, no exclusion
      dir_tmp <- .test_setup_content_dir()
      path_vec_dir <- dir_tmp |> .dir_ls()
      path_vec_file <- dir_tmp |> .file_ls()
      dir_tmp_2 <- .dir_create_tmp_random()
      .dir_move_exact(dir_tmp, dir_tmp_2)
      expect_identical(
        dir_tmp_2 |> .dir_ls(),
        path_vec_dir
      )
      expect_identical(
        dir_tmp_2 |> .file_ls(),
        path_vec_file
      )
      expect_identical(.file_ls(dir_tmp), character())
      expect_identical(.dir_ls(dir_tmp), character())
      # removing initial, with exclusion
      dir_tmp <- .test_setup_content_dir()
      path_vec_dir <- dir_tmp |> .dir_ls()
      path_vec_file <- dir_tmp |> .file_ls()

      .dir_create(file.path(dir_tmp, "d1"))
      file.create(file.path(dir_tmp, "d1", "f2")) |> invisible()
      path_vec_excess <- dir_tmp |> .dir_ls()
      dir_tmp_2 <- .dir_create_tmp_random()
      .dir_move_exact(dir_tmp, dir_tmp_2, dir_exc = "d1")
      expect_identical(dir_tmp_2 |> .dir_ls(), path_vec_dir)
      expect_identical(dir_tmp_2 |> .file_ls(), path_vec_file)
      expect_identical(.file_ls(dir_tmp), "d1/f2")
      expect_identical(.dir_ls(dir_tmp), "d1")

      # not removing initial, no exclusion
      dir_tmp <- .test_setup_content_dir()
      path_vec_dir <- dir_tmp |> .dir_ls()
      path_vec_file <- dir_tmp |> .file_ls()
      dir_tmp_2 <- .dir_create_tmp_random()
      file.create(file.path(dir_tmp_2, "f1")) |> invisible()
      .dir_move(dir_tmp, dir_tmp_2)
      expect_identical(
        dir_tmp_2 |> .dir_ls(),
        path_vec_dir
      )
      expect_identical(
        dir_tmp_2 |> .file_ls() |> sort(),
        path_vec_file |> c("f1") |> sort()
      )
      expect_identical(.file_ls(dir_tmp), character())
      expect_identical(.dir_ls(dir_tmp), character())
      # removing initial, with exclusion
      dir_tmp <- .test_setup_content_dir()
      path_vec_dir <- dir_tmp |> .dir_ls()
      path_vec_file <- dir_tmp |> .file_ls()

      .dir_create(file.path(dir_tmp, "d1"))
      file.create(file.path(dir_tmp, "d1", "f2")) |> invisible()
      path_vec_excess <- dir_tmp |> .dir_ls()
      dir_tmp_2 <- .dir_create_tmp_random()
      .dir_move(dir_tmp, dir_tmp_2, dir_exc = "d1")
      expect_identical(dir_tmp_2 |> .dir_ls(), path_vec_dir)
      expect_identical(dir_tmp_2 |> .file_ls(), path_vec_file)
      expect_identical(.file_ls(dir_tmp), "d1/f2")
      expect_identical(.dir_ls(dir_tmp), "d1")
    }
  )
})
