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
  skip_if(FALSE)
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
          remote_structure = "latest"
        ),
        character()
      )
      expect_identical(
        .projr_remote_get_final(
          "local",
          id = NULL,
          path = "a/b/c",
          path_append_label = TRUE,
          label = "data-raw",
          remote_structure = "version"
        ),
        "a/b/c/data-raw/v0.0.0-1"
      )
      # osf
      # --------------------------

      # no sub-directory
      browser()
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
          remote_structure = "latest"
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
          remote_structure = "version"
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
