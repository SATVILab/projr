test_that(".projr_dest_send_get_plan works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # no version source, so just send everything
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "none",
          strategy = "upload-all"
        ),
        "add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "none",
          strategy = "sync-using-deletion"
        ),
        "delete_add_all"
      )
      # flat
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "upload-all",
          type = "github",
          structure = "latest"
        ),
        "add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "upload-all",
          type = "github",
          structure = "version"
        ),
        "add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "sync-using-deletion",
          type = "github",
          structure = "version"
        ),
        "delete_add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "sync-using-version",
          type = "github",
          structure = "version"
        ),
        "delete_add_all_if_change"
      )
      # hierarchical
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "upload-all",
          type = "osf",
          structure = "latest"
        ),
        "add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "upload-all",
          type = "osf",
          structure = "version"
        ),
        "add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "sync-using-deletion",
          type = "local",
          structure = "version"
        ),
        "delete_add_all"
      )
      expect_identical(
        .projr_dest_send_get_plan(
          version_source = "manifest",
          strategy = "sync-using-version",
          type = "osf",
          structure = "version"
        ),
        "delete_add_all_if_change"
      )
    }
  )
})

test_that(".projr_dest_send_get_plan_detail works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # just list whatever's there
      plan_list_detail_zero <- list("add" = character(), "rm" = character())
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "add_all",
        path_dir_local = .dir_create_tmp_random()
      )
      expect_identical(plan_list_detail, plan_list_detail_zero)
      dir_tmp <- .projr_test_setup_content_dir()
      expect_identical(
        .projr_dest_send_get_plan_detail(
          plan = "delete_add_all",
          path_dir_local = dir_tmp
        ),
        list("add" = .file_ls(dir_tmp), rm = character())
      )
      # add what's missing:
      # all missing
      dir_tmp_2 <- .dir_create_tmp_random()
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "add_missing",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2
      )
      plan_list_detail_full <- list(
        "add" = .file_ls(dir_tmp), "rm" = character()
      )
      expect_identical(plan_list_detail, plan_list_detail_full)
      # nothing missing
      .dir_copy(dir_tmp, dir_tmp_2)
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "add_missing",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2
      )
      expect_identical(plan_list_detail, plan_list_detail_zero)
      # only extra in remote
      .dir_clear(dir_tmp)
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "add_missing",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2
      )
      expect_identical(plan_list_detail, plan_list_detail_zero)

      # add all if any massing:
      # nothing missing
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "add_all_if_missing",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2
      )
      expect_identical(plan_list_detail, plan_list_detail_zero)
      # one file missing
      invisible(file.create(file.path(dir_tmp, "file_1.txt")))
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "add_all_if_missing",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2
      )
      expect_identical(
        plan_list_detail, list("add" = "file_1.txt", rm = character())
      )

      # only changes:
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "change",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2,
        version_source = "file"
      )
      expect_identical(
        plan_list_detail,
        list("add" = "file_1.txt", rm = .file_ls(dir_tmp_2))
      )
      # all if any change
      # only add missing
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "delete_add_all_if_change",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2,
        version_source = "file"
      )
      expect_identical(
        plan_list_detail,
        list("add" = "file_1.txt", rm = character())
      )
      # add even if present on remote
      .dir_copy(dir_tmp_2, dir_tmp)
      plan_list_detail <- .projr_dest_send_get_plan_detail(
        plan = "delete_add_all_if_change",
        path_dir_local = dir_tmp,
        type = "local",
        remote = dir_tmp_2,
        version_source = "file"
      )
      expect_identical(
        plan_list_detail,
        list("add" = .file_ls(dir_tmp), rm = character())
      )
    }
  )
})

test_that(".projr_plan_implement works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      plan_list_detail_zero <- list("add" = character(), "rm" = character())
      # do nothing
      dir_tmp <- .dir_create_tmp_random()
      dir_tmp_2 <- .dir_create_tmp_random()
      .projr_plan_implement(
        plan = "delete_add_all",
        plan_detail = plan_list_detail_zero,
        path_dir_local = dir_tmp,
        remote = dir_tmp_2,
        type = "local",
        structure = "latest"
      )
      expect_identical(.file_ls(dir_tmp_2) |> length(), 0L)
      expect_true(dir.exists(dir_tmp_2))
      # add content, but still do nothing
      .projr_test_setup_content_dir(dir_tmp)
      .projr_plan_implement(
        plan = "delete_add_all",
        plan_detail = plan_list_detail_zero,
        path_dir_local = dir_tmp,
        remote = dir_tmp_2,
        type = "local",
        structure = "latest"
      )
      expect_identical(.file_ls(dir_tmp_2) |> length(), 0L)
      expect_true(dir.exists(dir_tmp_2))
      # check that we're emptying remote if needed
      .projr_test_setup_content_dir(dir_tmp_2)
      expect_true(.file_ls(dir_tmp_2) |> length() > 0L)
      .projr_test_setup_content_dir(dir_tmp)
      .projr_plan_implement(
        plan = "delete_add_all",
        plan_detail = plan_list_detail_zero,
        path_dir_local = dir_tmp,
        remote = dir_tmp_2,
        type = "local",
        structure = "latest"
      )
      expect_identical(.file_ls(dir_tmp_2) |> length(), 4L)
      expect_true(dir.exists(dir_tmp_2))
      # check it's deleted when an empty versioned remote
      .dir_clear(dir_tmp_2)
      expect_true(.file_ls(dir_tmp_2) |> length() == 0L)
      .projr_test_setup_content_dir(dir_tmp)
      dir_tmp_2_version <- file.path(dir_tmp_2, "v0.0.1") |>
        .dir_create()
      .projr_plan_implement(
        plan = "delete_add_all",
        plan_detail = plan_list_detail_zero,
        path_dir_local = dir_tmp,
        remote = dir_tmp_2_version,
        type = "local",
        structure = "version"
      )
      expect_identical(.file_ls(dir_tmp_2) |> length(), 0L)
      expect_true(dir.exists(dir_tmp_2))
      expect_true(!dir.exists(dir_tmp_2_version))
      # check that files are copied across and deleted as required
      file.create(file.path(dir_tmp_2, "f1.txt")) |> invisible()
      file.create(file.path(dir_tmp_2, "f2.txt")) |> invisible()
      .projr_test_setup_content_dir(dir_tmp)
      .projr_plan_implement(
        plan = "add_all",
        plan_detail = list("add" = "abc.txt", rm = "f2.txt"),
        path_dir_local = dir_tmp,
        remote = dir_tmp_2,
        type = "local",
        structure = "latest",
        conflict = "overwrite"
      )
      expect_true(!file.exists(file.path(dir_tmp_2, "f2.txt")))
      expect_true(file.exists(file.path(dir_tmp_2, "abc.txt")))
      expect_true(file.exists(file.path(dir_tmp_2, "f1.txt")))
    }
  )
})
