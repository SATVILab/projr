test_that(".change_get_manifest works", {
  skip("Test needs rework - .change_get_manifest not designed for project vs project comparison")
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_post(TRUE) |> invisible()

      # only one version
      # ----------------
      change_list <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "output"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 3L)
      change_list_raw_data <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "raw-data"
      )
      expect_identical(length(change_list_raw_data), 4L)
      expect_identical(length(change_list_raw_data[["fn_same"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_diff"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_source_extra"]]), 0L)

      # no change, two versions
      # -----------------------
      .version_bump_major() |> invisible()
      .build_manifest_post(TRUE) |> invisible()
      change_list <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "output"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 3L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
      change_list_raw_data <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "raw-data"
      )
      expect_identical(length(change_list_raw_data), 4L)
      expect_identical(length(change_list_raw_data[["fn_same"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_diff"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_source_extra"]]), 0L)

      # added category, three versions
      # ------------------------------
      .version_bump_patch() |> invisible()
      .test_content_setup_label("raw-data")
      .build_manifest_pre(TRUE) |> invisible()
      .build_manifest_post(TRUE) |> invisible()
      expect_true(
        all(c("raw-data", "output") %in% .manifest_read(
          .build_manifest_post_get_path(TRUE)
        )[["label"]])
      )
      change_list <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "output"
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 3L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
      change_list_raw_data <- .change_get_manifest(
        type_pre = "project",
        remote_pre = NULL,
        type_post = "project",
        remote_post = NULL,
        label = "raw-data"
      )
      expect_identical(length(change_list_raw_data), 4L)
      expect_identical(length(change_list_raw_data[["fn_same"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_diff"]]), 0L)
      expect_identical(length(change_list_raw_data[["fn_source_extra"]]), 3L)
    }
  )
})

test_that(".change_get_file works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      path_pre <- .test_content_setup_dir()
      path_post <- .test_dir_create_random()
      change_list <- .change_get_file(
        type_pre = "local",
        remote_pre = path_pre,
        type_post = "local",
        remote_post = path_post
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 4L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
    }
  )
})

test_that(".change_get works for files", {
  # skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_version_set("0.0.1")
      # nothing before or after
      change_list <- .change_get(
        label = "output",
        output_run = FALSE,
        inspect = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )
      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)

      # add something
      browser()
      .version_bump_minor()
      .test_content_setup_label("output", safe = FALSE)
      .build_manifest_post(TRUE) |> invisible()
      change_list <- .change_get(
        label = "output",
        output_run = TRUE,
        inspect = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )

      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 3L)
      change_list <- .change_get(
        label = "output",
        output_run = TRUE,
        inspect = "file",
        type = "local",
        remote = .dir_create_tmp_random()
      )

      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 0L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 0L)
      expect_identical(length(change_list[["fn_diff"]]), 0L)
      expect_identical(length(change_list[["fn_source_extra"]]), 3L)

      # remove and change
      .version_bump_major()
      .dir_copy(
        path_dir_from = "_output", path_dir_to = "_output2"
      )
      cat("abc", file = "_output/abc.txt")
      invisible(file.remove("_output/subdir1/def.txt"))
      change_list <- .change_get(
        label = "output",
        output_run = TRUE,
        inspect = "file",
        type = "local",
        remote = "_output2"
      )

      expect_identical(length(change_list), 4L)
      expect_identical(length(change_list[["fn_same"]]), 1L)
      expect_identical(length(change_list[["fn_dest_extra"]]), 1L)
      expect_identical(length(change_list[["fn_diff"]]), 1L)
      expect_identical(length(change_list[["fn_source_extra"]]), 0L)
    }
  )
})
