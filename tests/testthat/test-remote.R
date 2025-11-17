# --------------------------
# creating remotes
# --------------------------
test_that(".remote_create works - local", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      path_dir_tmp_random <- .test_dir_create_random(create = FALSE)
      withr::defer(unlink(path_dir_tmp_random, recursive = TRUE))
      .remote_create(type = "local", id = path_dir_tmp_random)
      expect_true(dir.exists(path_dir_tmp_random))
      expect_true(
        .remote_check_exists(type = "local", id = path_dir_tmp_random)
      )
      unlink(path_dir_tmp_random, recursive = TRUE)
      expect_false(
        .remote_check_exists(
          type = "local", id = path_dir_tmp_random
        )
      )
    }
  )
})

test_that(".remote_create works - remote", {
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if(.is_test_lite())
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # osf
      # --------------------------

      if (suppressWarnings(nzchar(.auth_get_osf_pat()))) {

        # project
        id_parent <- .test_osf_create_project("ProjectParent")
        expect_true(.remote_check_exists("osf", id_parent))
        env <- environment()
        .osf_rm_node_id_defer(id_parent, env)

        # component
        id_comp <- try(.remote_create(
          type = "osf", name = "CreateComp", id_parent = id_parent,
          category = "data"
        ))
        expect_true(.remote_check_exists("osf", id_comp))
        .osf_rm_node_id_defer(id_comp, env)
      }

      # github
      # --------------------------
      tag_init <- .test_random_string_get()
      tag <- .remote_create("github", id = tag_init)
      start_time <- proc.time()[3]
      max_wait <- 600
      remote_exists <- .remote_check_exists("github", id = tag)
      carry_on <- !remote_exists && (proc.time()[3] - start_time < max_wait)
      while (carry_on) {
        Sys.sleep(10)
        remote_exists <- .remote_check_exists("github", id = tag)
        carry_on <- !remote_exists && (proc.time()[3] - start_time < max_wait)
      }
      expect_true(remote_exists)
    }
  )
})

# --------------------------
# getting remotes
# --------------------------
test_that(".remote_get works - local", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      expect_identical(
        .remote_get("local", "a/b/c"),
        "a/b/c"
      )
    }
  )
})

test_that(".remote_get works - remote", {
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # osf
      # --------------------------
      if (suppressWarnings(nzchar(.auth_get_osf_pat()))) {
        id <- .test_osf_create_project("ProjectParent")
        expect_identical(
          .remote_get("osf", id)[["id"]],
          id
        )
      }

      # github
      # --------------------------
      expect_identical(
        .remote_get("github", "abc"),
        c("tag" = "abc")
      )
    }
  )
})

# --------------------------
# get final remotes
# --------------------------

test_that(".remote_get_final works - local", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      expect_identical(
        .remote_get_final(
          "local",
          id = "a/b/c",
          path = NULL,
          path_append_label = FALSE,
          label = "raw-data",
          structure = "latest"
        ),
        "a/b/c"
      )
      expect_identical(
        .remote_get_final(
          "local",
          id = "a/b/c",
          label = "raw-data",
          structure = "archive",
          path_append_label = TRUE
        ),
        "a/b/c/raw-data/v0.0.0-1"
      )
    }
  )
})

test_that(".remote_get_final works", {
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # osf
      # --------------------------

      if (suppressWarnings(nzchar(.auth_get_osf_pat()))) {
        # no sub-directory
        id <- .test_osf_create_project("ProjectParent")
        expect_error(
          .remote_get_final(
            "osf",
            id = id, path_append_label = TRUE
          )[["id"]]
        )
        expect_identical(
          .remote_get_final(
            "osf",
            id = id, path_append_label = FALSE,
            structure = "latest"
          )[["id"]],
          id
        )

        # sub-directory
        path_rel <- "a/raw-data/v0.0.0-1"
        osf_tbl <- .osf_mkdir(.remote_get("osf", id), path_rel)
        expect_identical(
          .remote_get_final_osf(
            id = id,
            path = "a",
            path_append_label = TRUE,
            label = "raw-data",
            structure = "archive"
          ),
          osf_tbl
        )
      }


      # github
      # --------------------------
      expect_identical(
        .remote_get_final(
          "github",
          id = "kablumph", label = "raw-data", structure = "archive"
        ),
        c("tag" = "kablumph", fn = "raw-data-v0.0.0-1.zip")
      )
    }
  )
})

# --------------------------
# removing empty remotes
# --------------------------

test_that(".remote_rm_final_if_empty works - local", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------

      id <- .remote_get_final(
        "local",
        id = "a/b/c", path_append_label = FALSE, structure = "latest"
      )

      # latest structure
      expect_false(
        .remote_rm_final_if_empty(
          "local",
          remote = id, structure = "latest"
        )
      )
      expect_true(dir.exists(id))

      # versioned structure
      id <- .remote_get_final(
        "local",
        id = "a/b/c", path_append_label = FALSE, structure = "archive"
      )

      # will remove now
      expect_true(
        .remote_rm_final_if_empty(
          "local",
          remote = id, structure = "archive"
        )
      )
      expect_false(dir.exists(id))

      # won't remove if there are contents
      id <- .remote_get_final(
        "local",
        id = "a/b/c", path_append_label = FALSE, structure = "archive"
      )
      file.create(file.path(id, "abc.txt"))

      # will remove now
      expect_false(
        .remote_rm_final_if_empty(
          "local",
          remote = id, structure = "archive"
        )
      )
      expect_true(dir.exists(id))
    }
  )
})

test_that(".remote_rm_final_if_empty works - remote", {
  skip_if_offline()
  skip_if(.is_test_select())
  skip_on_cran()
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      if (suppressWarnings(nzchar(.auth_get_osf_pat()))) {
        # osf
        # --------------------------

        # create node
        id <- .test_osf_create_project("Project")
        osf_tbl <- .remote_get("osf", id)

        # when we pass the node rather than sub-dir
        expect_false(
          .remote_rm_final_if_empty(
            "osf",
            remote = osf_tbl, structure = "archive"
          )
        )

        # create the sub-directory
        osf_tbl_file <- .remote_get_final(
          "osf",
          id = id, path_append_label = FALSE, structure = "archive"
        )

        # remove it again
        expect_true(
          .remote_rm_final_if_empty(
            "osf",
            remote = osf_tbl_file, structure = "archive"
          )
        )
        is_zero <- (.osf_ls_files(osf_tbl) |> nrow()) == 0L
        n_sec <- 0
        start_time <- proc.time()[3]
        while (!is_zero && n_sec < 15) {
          Sys.sleep(3)
          is_zero <- (.osf_ls_files(osf_tbl) |> nrow()) == 0L
          n_sec <- proc.time()[3] - start_time
        }
        expect_true(is_zero)


        # create the sub-directory, and upload to it
        osf_tbl_file <- .remote_get_final(
          "osf",
          id = id, path_append_label = FALSE, structure = "archive"
        )
        invisible(file.create("abc.txt"))
        .osf_upload(x = osf_tbl_file, path = "abc.txt")

        # try to remove it, check that it isn't
        expect_false(
          .remote_rm_final_if_empty(
            "osf",
            remote = osf_tbl_file, structure = "archive"
          )
        )
        expect_identical(.osf_ls_files(osf_tbl) |> nrow(), 1L)
      }

      # github
      # --------------------------
      expect_false(
        .remote_rm_final_if_empty("github", FALSE)
      )
    }
  )
})

# --------------------------
# removing all contents of a remote
# --------------------------

test_that(".remote_file_rm_all works - local", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      # does not exist
      path_dir_random <- file.path(tempdir(), "random_path_1234i3rknlasdfo")
      expect_false(
        .remote_file_rm_all(
          "local",
          remote = path_dir_random
        )
      )

      # has content
      path_dir <- .test_setup_content_dir()
      expect_true(
        .remote_file_rm_all(
          "local",
          remote = path_dir
        )
      )
      expect_true(dir.exists(path_dir))
      expect_identical(list.files(path_dir), character())
      unlink(path_dir, recursive = TRUE)
    }
  )
})

test_that(".remote_file_rm_all works - remote", {
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {

      if (suppressWarnings(nzchar(.auth_get_osf_pat()))) {
        # osf
        # --------------------------

        # create node
        id <- .test_osf_create_project("Project")
        osf_tbl <- .remote_get("osf", id)

        # when empty
        expect_false(
          .remote_file_rm_all(
            "osf",
            remote = osf_tbl
          )
        )

        # clear content
        osf_tbl_sub_a <- .osf_mkdir(osf_tbl, path = "a")
        osf_tbl_sub_b <- .osf_mkdir(osf_tbl, path = "a/b")
        path_tmp_file <- file.path(tempdir(), "abc.txt")
        file.create(path_tmp_file)
        .osf_upload(
          x = osf_tbl, path = path_tmp_file, conflicts = "overwrite"
        )
        .osf_upload(
          x = osf_tbl_sub_a, path = path_tmp_file,
          conflicts = "overwrite"
        )
        .osf_upload(
          x = osf_tbl_sub_b, path = path_tmp_file,
          conflicts = "overwrite"
        )
        expect_true(
          .remote_file_rm_all(
            "osf",
            remote = osf_tbl
          )
        )
        expect_true(nrow(.osf_ls_files(osf_tbl)) == 0L)
      }

      # github
      # --------------------------
      piggyback:::.pb_cache_clear()
      id <- .remote_create("github", id = "abc")
      path_tmp_file <- file.path(tempdir(), "abc.txt")
      file.create(path_tmp_file)
      path_zip <- .zip_file(
        fn_rel = basename(path_tmp_file),
        path_dir_fn_rel = dirname(path_tmp_file),
        fn_rel_zip = "abc.zip"
      )
      .remote_file_add_github_zip_attempt(
        path_zip = path_zip,
        tag = id,
        output_level = "debug",
        log_file = NULL,
        max_time = 300
      )
      repo <- .pb_guess_repo()
      content_tbl_pre_delete <- piggyback::pb_list(
        repo = repo, tag = id
      )
      expect_identical(nrow(content_tbl_pre_delete), 1L)
      remote_github <- c("tag" = id, fn = basename(path_zip))
      .remote_file_rm_all(
        "github",
        remote = remote_github
      )
      content_tbl <- piggyback::pb_list(repo = repo, tag = id)
      expect_true(is.null(content_tbl) || nrow(content_tbl) == 0L)
    }
  )
})

# --------------------------
# editing files on remotes
# --------------------------

test_that("adding, tallying and removing files from remotes works - local", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # local
      # --------------------------
      # empty
      path_dir_random <- .dir_create_tmp_random()
      expect_identical(
        .remote_file_ls(
          "local",
          remote = path_dir_random
        ),
        character()
      )


      # has content
      path_dir_source <- .test_setup_content_dir()
      fn_vec_source <- .remote_file_ls("local", path_dir_source)
      path_dir_dest <- .dir_create_tmp_random()
      .remote_file_add(
        "local",
        fn = fn_vec_source,
        path_dir_local = path_dir_source,
        remote = path_dir_dest
      )
      expect_identical(
        .remote_file_ls("local", path_dir_dest),
        fn_vec_source
      )

      # remove some content
      fn_vec_dest_orig <- .remote_file_ls("local", path_dir_dest)
      fn_vec_rm <- c("abc.txt", "subdir1/def.txt")
      .remote_file_rm("local", fn = fn_vec_rm, remote = path_dir_dest)
      expect_identical(
        .remote_file_ls(
          "local",
          remote = path_dir_dest
        ),
        fn_vec_dest_orig |> setdiff(fn_vec_rm)
      )
      unlink(path_dir_dest, recursive = TRUE)
      unlink(path_dir_source, recursive = TRUE)
    }
  )
})

test_that("adding, tallying and removing files from remotes works - osf", {
  skip("OSF tests disabled - to be reviewed")
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # osf
      # --------------------------

      # create node
      id <- .test_osf_create_project("Project")
      osf_tbl <- .remote_get("osf", id)

      # when empty
      expect_identical(
        .remote_file_ls(
          "osf",
          remote = osf_tbl
        ),
        character()
      )

      # with content
      path_dir_source <- .test_setup_content_dir()
      fn_vec_source <- .remote_file_ls("local", path_dir_source)
      path_dir_dest <- .dir_create_tmp_random()
      .remote_file_add(
        "osf",
        fn = fn_vec_source,
        path_dir_local = path_dir_source,
        remote = osf_tbl
      )
      expect_identical(
        .remote_file_ls("osf", osf_tbl),
        fn_vec_source
      )

      # remove some content
      fn_vec_orig_osf <- .remote_file_ls("osf", osf_tbl)
      fn_vec_rm <- c("abc.txt", "subdir1/def.txt")
      expect_true(
        .remote_file_rm("osf", fn = fn_vec_rm, remote = osf_tbl)
      )
      expect_identical(
        .remote_file_ls(
          "osf",
          remote = osf_tbl
        ),
        fn_vec_orig_osf |> setdiff(fn_vec_rm)
      )
      unlink(path_dir_source, recursive = TRUE)
    }
  )
})

test_that("adding, tallying and removing files from remotes works - github", {
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # github
      # --------------------------
      # create release
      id <- .test_random_string_get()
      remote <- .remote_get_final(
        "github",
        id = id, label = "raw-data", structure = "latest"
      )
      .remote_create("github", remote[["tag"]])

      # when empty
      expect_identical(
        .remote_file_ls(
          "github",
          remote = remote
        ),
        character()
      )

      # with content
      path_dir_source <- .test_setup_content_dir()
      remote <- stats::setNames(.test_random_string_get(), "tag")
      remote <- remote |> c(c("fn" = "raw-data.zip"))
      fn_vec <- .remote_file_ls("local", path_dir_source)
      .remote_file_add(
        "github",
        fn = fn_vec, path_dir_local = path_dir_source, remote = remote
      )
      path_dir_save <- .dir_create_tmp_random()
      .remote_file_get_all(
        "github",
        remote = remote, path_dir_save_local = path_dir_save
      )
      expect_identical(
        .remote_file_ls("local", path_dir_save),
        fn_vec
      )

      # remove some content
      fn_vec_orig_github <- .remote_file_ls("github", remote)
      fn_vec_rm <- c("abc.txt", "subdir1/def.txt")
      expect_true(
        .remote_file_rm("github", fn = fn_vec_rm, remote = remote)
      )
      expect_identical(
        .remote_file_ls("github", remote),
        setdiff(fn_vec_orig_github, fn_vec_rm)
      )
    }
  )
})
