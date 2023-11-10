test_that(".projr_remote_create works", {
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
      path_dir_tmp_random <- .projr_test_dir_create_random(create = FALSE)
      withr::defer(unlink(path_dir_tmp_random, recursive = TRUE))
      .projr_remote_create(
        remote_type = "local", remote_id = path_dir_tmp_random
      )
      expect_true(dir.exists(path_dir_tmp_random))
      expect_true(
        .projr_remote_check_exists(
          remote_type = "local", remote_id = path_dir_tmp_random
        )
      )
      unlink(path_dir_tmp_random, recursive = TRUE)
      expect_false(
        .projr_remote_check_exists(
          remote_type = "local", remote_id = path_dir_tmp_random
        )
      )

      # osf
      # --------------------------

      # project
      id_parent <- try(.projr_remote_create(
        remote_type = "osf", remote_id = "CreateParent"
      ))
      expect_true(.projr_remote_check_exists("osf", id_parent))
      .projr_osf_rm_node_id_defer(id_parent)

      # component
      debugonce(.projr_remote_create_osf)
      id_comp <- try(.projr_remote_create(
        remote_type = "osf", remote_id = "CreateComp", parent_id = id_parent,
        category = "data"
      ))
      expect_true(.projr_remote_check_exists("osf", id_comp))
      .projr_osf_rm_node_id_defer(id_comp)

      # github
      # --------------------------
      browser()
      tag_init <- .projr_test_random_string_get()

      tag <- .projr_remote_create_github(tag = tag_init)
    }
  )
})
