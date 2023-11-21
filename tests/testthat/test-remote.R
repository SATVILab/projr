test_that(".projr_remote_create works", {
  skip_if_offline()
  skip_if(TRUE)
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      path_dir_tmp_random <- .projr_test_dir_create_random(create = FALSE)
      withr::defer(unlink(path_dir_tmp_random, recursive = TRUE))
      .projr_remote_create(type = "local", id = path_dir_tmp_random)
      expect_true(dir.exists(path_dir_tmp_random))
      expect_true(
        .projr_remote_check_exists(type = "local", id = path_dir_tmp_random)
      )
      unlink(path_dir_tmp_random, recursive = TRUE)
      expect_false(
        .projr_remote_check_exists(
          type = "local", id = path_dir_tmp_random
        )
      )

      # osf
      # --------------------------

      # project
      id_parent <- try(.projr_remote_create(
        type = "osf", name = "CreateParent"
      ))
      expect_true(.projr_remote_check_exists("osf", id_parent))
      .projr_osf_rm_node_id_defer(id_parent)

      # component
      id_comp <- try(.projr_remote_create(
        type = "osf", name = "CreateComp", id_parent = id_parent,
        category = "data"
      ))
      expect_true(.projr_remote_check_exists("osf", id_comp))
      .projr_osf_rm_node_id_defer(id_comp)

      # github
      # --------------------------
      tag_init <- .projr_test_random_string_get()
      tag <- .projr_remote_create("github", id = tag_init)
      expect_true(
        .projr_remote_check_exists("github", id = tag)
      )
    }
  )
})

test_that(".projr_remote_get works", {
  skip_if_offline()
  skip_if(TRUE)
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      expect_identical(
        .projr_remote_get("local", "a/b/c"),
        "a/b/c"
      )

      # osf
      # --------------------------
      id <- try(.projr_remote_create(
        type = "osf", name = "CreateParent"
      ))
      expect_identical(
        .projr_remote_get("osf", id)[["id"]],
        id
      )

      # github
      # --------------------------
      expect_identical(
        .projr_remote_get("github", "abc"),
        c("tag" = "abc")
      )
    }
  )
})

test_that(".projr_remote_get_final works", {
  skip_if_offline()
  skip_if(TRUE)
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      expect_identical(
        .projr_remote_get_final(
          "local",
          id = "a/b/c",
          path = NULL,
          path_append_label = FALSE,
          label = "data-raw",
          structure = "latest"
        ),
        "a/b/c"
      )
      expect_identical(
        .projr_remote_get_final(
          "local",
          id = "a/b/c",
          path_append_label = TRUE,
          label = "data-raw",
          structure = "version"
        ),
        "a/b/c/data-raw/v0.0.0-1"
      )
      # osf
      # --------------------------

      # no sub-directory
      id <- try(.projr_remote_create(
        type = "osf", name = "CreateParent"
      ))
      expect_error(
        .projr_remote_get_final(
          "osf",
          id = id, path_append_label = TRUE
        )[["id"]]
      )
      expect_identical(
        .projr_remote_get_final(
          "osf",
          id = id, path_append_label = FALSE,
          structure = "latest"
        )[["id"]],
        id
      )

      # sub-directory
      path_rel <- "a/data-raw/v0.0.0-1"
      osf_tbl <- osfr::osf_mkdir(.projr_remote_get("osf", id), path_rel)
      expect_identical(
        .projr_remote_get_final_osf(
          id = id,
          path = "a",
          path_append_label = TRUE,
          label = "data-raw",
          structure = "version"
        ),
        osf_tbl
      )

      # github
      # --------------------------
      expect_identical(
        .projr_remote_get_final("github", id = "kablumph", label = "data-raw"),
        c("tag" = "kablumph", fn = "data-raw.zip")
      )
    }
  )
})

test_that(".projr_remote_rm_final_if_empty works", {
  skip_if_offline()
  skip_if(TRUE)
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------

      id <- .projr_remote_get_final(
        "local",
        id = "a/b/c", path_append_label = FALSE, structure = "latest"
      )

      # latest structure
      expect_false(
        .projr_remote_rm_final_if_empty(
          "local",
          remote = id, structure = "latest"
        )
      )
      expect_true(dir.exists(id))

      # versioned structure
      id <- .projr_remote_get_final(
        "local",
        id = "a/b/c", path_append_label = FALSE, structure = "version"
      )

      # will remove now
      expect_true(
        .projr_remote_rm_final_if_empty(
          "local",
          remote = id, structure = "version"
        )
      )
      expect_false(dir.exists(id))

      # won't remove if there are contents
      id <- .projr_remote_get_final(
        "local",
        id = "a/b/c", path_append_label = FALSE, structure = "version"
      )
      file.create(file.path(id, "abc.txt"))

      # will remove now
      expect_false(
        .projr_remote_rm_final_if_empty(
          "local",
          remote = id, structure = "version"
        )
      )
      expect_true(dir.exists(id))


      # osf
      # --------------------------

      # create node
      id <- try(.projr_remote_create(
        type = "osf", name = "CreateNode"
      ))
      osf_tbl <- .projr_remote_get("osf", id)

      # when we pass the node rather than sub-dir
      expect_false(
        .projr_remote_rm_final_if_empty(
          "osf",
          remote = osf_tbl, structure = "version"
        )
      )

      # create the sub-directory
      osf_tbl_file <- .projr_remote_get_final(
        "osf",
        id = id, path_append_label = FALSE, structure = "version"
      )

      # remove it again
      expect_true(
        .projr_remote_rm_final_if_empty(
          "osf",
          remote = osf_tbl_file, structure = "version"
        )
      )
      expect_identical(osfr::osf_ls_files(osf_tbl) |> nrow(), 0L)

      # create the sub-directory, and upload to it
      osf_tbl_file <- .projr_remote_get_final(
        "osf",
        id = id, path_append_label = FALSE, structure = "version"
      )
      invisible(file.create("abc.txt"))
      osfr::osf_upload(x = osf_tbl_file, path = "abc.txt")

      # try to remove it, check that it isn't
      expect_false(
        .projr_remote_rm_final_if_empty(
          "osf",
          remote = osf_tbl_file, structure = "version"
        )
      )
      expect_identical(osfr::osf_ls_files(osf_tbl) |> nrow(), 1L)

      # github
      # --------------------------
      expect_false(
        .projr_remote_rm_final_if_empty("github", FALSE)
      )
    }
  )
})

test_that(".projr_remote_file_rm_all works", {
  skip_if_offline()
  skip_if(TRUE)
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      # does not exist
      path_dir_random <- file.path(tempdir(), "random_path_1234i3rknlasdfo")
      expect_false(
        .projr_remote_file_rm_all(
          "local",
          remote = path_dir_random
        )
      )

      # has content
      path_dir <- .projr_test_setup_content_dir()
      expect_true(
        .projr_remote_file_rm_all(
          "local",
          remote = path_dir
        )
      )
      expect_true(dir.exists(path_dir))
      expect_identical(list.files(path_dir), character())
      unlink(path_dir, recursive = TRUE)

      # osf
      # --------------------------

      # create node
      id <- try(.projr_remote_create(
        type = "osf", name = "CreateNode"
      ))
      osf_tbl <- .projr_remote_get("osf", id)

      # when empty
      expect_false(
        .projr_remote_file_rm_all(
          "osf",
          remote = osf_tbl
        )
      )

      # clear content
      osf_tbl_sub_a <- osfr::osf_mkdir(osf_tbl, path = "a")
      osf_tbl_sub_b <- osfr::osf_mkdir(osf_tbl, path = "a/b")
      path_tmp_file <- file.path(tempdir(), "abc.txt")
      file.create(path_tmp_file)
      osfr::osf_upload(x = osf_tbl, path = path_tmp_file)
      osfr::osf_upload(x = osf_tbl_sub_a, path = path_tmp_file)
      osfr::osf_upload(x = osf_tbl_sub_b, path = path_tmp_file)
      expect_true(
        .projr_remote_file_rm_all(
          "osf",
          remote = osf_tbl
        )
      )
      expect_true(nrow(osfr::osf_ls_files(osf_tbl)) == 0L)

      # github
      # --------------------------
      id <- .projr_remote_create("github", id = "abc")
      Sys.sleep(3)
      path_tmp_file <- file.path(tempdir(), "abc.txt")
      file.create(path_tmp_file)
      path_zip <- .projr_zip_file(
        fn_rel = basename(path_tmp_file),
        path_dir_fn_rel = dirname(path_tmp_file),
        fn_rel_zip = "abc.zip"
      )
      piggyback:::.pb_cache_clear()
      piggyback::pb_upload(file = path_zip, tag = id)
      content_tbl_pre_delete <- piggyback::pb_list(tag = id)
      expect_identical(nrow(content_tbl_pre_delete), 1L)
      expect_true(.projr_remote_file_rm_all("github", remote = id))
      piggyback:::.pb_cache_clear()
      content_tbl <- piggyback::pb_list(tag = id)
      expect_null(content_tbl)
    }
  )
})

test_that("adding, tallying and removing files from remotes works", {
  skip_if_offline()
  skip_if(FALSE)
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      # empty
      path_dir_random <- .projr_dir_tmp_random_get()
      expect_identical(
        .projr_remote_file_ls(
          "local",
          remote = path_dir_random
        ),
        character()
      )

      browser()

      # has content
      path_dir_source <- .projr_test_setup_content_dir()
      fn_vec_source <- .projr_remote_file_ls("local", path_dir_source)
      path_dir_dest <- .projr_dir_tmp_random_get()
      debugonce(.projr_remote_file_add_local)
      debugonce(.projr_dir_tree_copy)
      .projr_remote_file_add(
        "local",
        fn = fn_vec_source,
        path_dir_local = path_dir_source,
        remote = path_dir_dest
      )
      expect_identical(
        .projr_remote_file_ls("local", path_dir_dest),
        fn_vec_source
      )

      # remove some content
      # START HERE!!!
      expect_identical(
        .projr_remote_file_ls(
          "local",
          remote = path_dir_content
        ),
        list.files(path_dir, recursive = TRUE, all.files = TRUE)
      )
      unlink(path_dir, recursive = TRUE)

      # osf
      # --------------------------

      # create node
      browser()
      id <- try(.projr_remote_create(
        type = "osf", name = "CreateNode"
      ))
      osf_tbl <- .projr_remote_get("osf", id)

      # when empty
      expect_identical(
        .projr_remote_file_ls(
          "osf",
          remote = osf_tbl
        ),
        character()
      )

      # clear with content
      path_dir <- .projr_test_setup_content_dir()
      osfr::osf_upload(
        x = osf_tbl, path = path_dir, recurse = TRUE, conflicts = "overwrite"
      )
      osf_tbl_file <- osfr::osf_mkdir(
        x = osf_tbl, path = list.dirs(path_dir)[2] |> basename()
      )
      .projr_remote_file_ls_osf(osf_tbl)
      osfr::osf_ls_files(x = osf_tbl)
      osfr::osf_ls_files(osf_tbl_file)
      osfr::osf_upload(x = osf_tbl, path = path_tmp_file)
      osfr::osf_upload(x = osf_tbl_sub_a, path = path_tmp_file)
      osfr::osf_upload(x = osf_tbl_sub_b, path = path_tmp_file)
      expect_true(
        .projr_remote_file_rm_all(
          "osf",
          remote = osf_tbl
        )
      )
      expect_true(nrow(osfr::osf_ls_files(osf_tbl)) == 0L)

      # github
      # --------------------------
      id <- .projr_remote_create("github", id = "abc")
      Sys.sleep(3)
      path_tmp_file <- file.path(tempdir(), "abc.txt")
      file.create(path_tmp_file)
      path_zip <- .projr_zip_file(
        fn_rel = basename(path_tmp_file),
        path_dir_fn_rel = dirname(path_tmp_file),
        fn_rel_zip = "abc.zip"
      )
      piggyback:::.pb_cache_clear()
      piggyback::pb_upload(file = path_zip, tag = id)
      content_tbl_pre_delete <- piggyback::pb_list(tag = id)
      expect_identical(nrow(content_tbl_pre_delete), 1L)
      expect_true(.projr_remote_file_rm_all("github", remote = id))
      piggyback:::.pb_cache_clear()
      content_tbl <- piggyback::pb_list(tag = id)
      expect_null(content_tbl)
    }
  )
})
