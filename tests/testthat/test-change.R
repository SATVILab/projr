test_that(".projr_change_get_manifest works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      .projr_test_setup_content("output", safe = FALSE)
      .projr_build_manifest_post(TRUE) |> invisible()

      # only one version
      # ----------------
      change_list <- .projr_change_get(
        label = "output", version_source = "manifest"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list[["removed"]]), 0L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 3L)
      change_list_data_raw <- .projr_change_get(
        label = "data-raw", version_source = "manifest"
      )
      expect_identical(length(change_list_data_raw), 4L)
      expect_identical(nrow(change_list_data_raw[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["removed"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["added"]]), 0L)

      # no change, two versions
      # -----------------------
      .projr_version_bump_major() |> invisible()
      .projr_build_manifest_post(TRUE) |> invisible()
      change_list <- .projr_change_get(
        label = "output", version_source = "manifest"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 3L)
      expect_identical(nrow(change_list[["removed"]]), 0L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 0L)
      change_list_data_raw <- .projr_change_get(
        label = "data-raw", version_source = "manifest"
      )
      expect_identical(length(change_list_data_raw), 4L)
      expect_identical(nrow(change_list_data_raw[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["removed"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["added"]]), 0L)

      # added category, three versions
      # ------------------------------
      .projr_version_bump_patch() |> invisible()
      .projr_test_setup_content("data-raw")
      .projr_build_manifest_pre(TRUE) |> invisible()
      .projr_build_manifest_post(TRUE) |> invisible()
      expect_true(
        all(c("data-raw", "output") %in% .projr_manifest_read(
          .projr_build_manifest_post_get_path(TRUE)
        )[["label"]])
      )
      change_list <- .projr_change_get(
        label = "output", version_source = "manifest"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 3L)
      expect_identical(nrow(change_list[["removed"]]), 0L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 0L)
      change_list_data_raw <- .projr_change_get(
        label = "data-raw", version_source = "manifest"
      )
      expect_identical(length(change_list_data_raw), 4L)
      expect_identical(nrow(change_list_data_raw[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["removed"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list_data_raw[["added"]]), 3L)
    }
  )
})

test_that(".projr_change_get_file works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      path_pre <- .projr_test_setup_content_dir()
      path_post <- .projr_test_dir_create_random()
      change_list <- .projr_change_get_file(
        type_pre = "local",
        remote_pre = path_pre,
        type_post = "local",
        remote_post = path_post
      )
      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list[["removed"]]), 4L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 0L)
    }
  )
})

test_that(".projr_change_get works for files", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      # nothing before or after
      change_list <- .projr_change_get(
        label = "output",
        output_run = FALSE,
        version_source = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )
      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list[["removed"]]), 0L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 0L)

      # add something
      .projr_version_bump_minor()
      .projr_test_setup_content("output", safe = FALSE)
      .projr_build_manifest_post(TRUE) |> invisible()
      change_list <- .projr_change_get(
        label = "output",
        output_run = TRUE,
        version_source = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )

      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list[["removed"]]), 0L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 3L)
      change_list <- .projr_change_get(
        label = "output",
        output_run = TRUE,
        version_source = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )

      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 0L)
      expect_identical(nrow(change_list[["removed"]]), 0L)
      expect_identical(nrow(change_list[["kept_changed"]]), 0L)
      expect_identical(nrow(change_list[["added"]]), 3L)

      # remove and change
      .projr_version_bump_major()
      .dir_copy(
        path_dir_from = "_output", path_dir_to = "_output2"
      )
      cat("abc", file = "_output/abc.txt")
      invisible(file.remove("_output/subdir1/def.txt"))
      change_list <- .projr_change_get(
        label = "output",
        output_run = TRUE,
        version_source = "file",
        type = "local",
        remote = "_output2"
      )

      expect_identical(length(change_list), 4L)
      expect_identical(nrow(change_list[["kept_unchanged"]]), 1L)
      expect_identical(nrow(change_list[["removed"]]), 1L)
      expect_identical(nrow(change_list[["kept_changed"]]), 1L)
      expect_identical(nrow(change_list[["added"]]), 0L)
    }
  )
})
