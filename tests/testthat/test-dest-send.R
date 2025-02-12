test_that("projr_yml_dest_add* functions work", {
  # skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive"
      )
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )
      # add two
      projr_yml_dest_add_local(
        title = "archive second",
        content = "output",
        path = "_archive/second"
      )
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          ),
          "archive second" = list(
            content = "output",
            path = "_archive/second"
          )
        )
      )
      # remove just one
      .projr_yml_dest_rm_title("archive second", "local", "default")
      expect_identical(
        .projr_yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )

      # ============
      # github
      # ============

      # test maniup
      .projr_yml_dest_rm_type_all("default")
      # add one
      projr_yml_dest_add_github(
        title = "archive",
        content = "raw-data"
      )
      expect_identical(
        .projr_yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
      # add two
      projr_yml_dest_add_github(
        title = "archive second",
        content = "output"
      )
      expect_identical(
        .projr_yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          ),
          "archive-second" = list(
            content = "output"
          )
        )
      )
      # remove just one
      .projr_yml_dest_rm_title("archive-second", "github", "default")
      expect_identical(
        .projr_yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
    }
  )
})

# --------------------------
# actually sending
# --------------------------

test_that("projr_dest_send works - local", {
  # skip_if(.is_test_select()
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      file.create(projr::projr_path_get("raw-data", "data.csv"))
      projr_init_git()
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr::projr_build_dev()
      # remove github remote
      yml_projr <- projr_yml_get()
      github_ind <- which(names(yml_projr[["build"]]) == "github")
      if (length(github_ind) > 0) {
        yml_projr[["build"]] <- yml_projr[["build"]][
          -github_ind
        ]
        .projr_yml_set(yml_projr)
      }
      projr::projr_build_patch()
      # add a local destination, that is never sent to
      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))


      # expect no upload
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))

      # now force upload
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))
      expect_true(file.exists("_archive/raw-data/v0.0.5/data.csv"))

      projr_yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest",
        send_cue = "always"
      )

      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        send_inspect = "file",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))
      expect_true(file.exists("_archive/raw-data/v0.0.6/data.csv"))

      # new upload, despite no change in project and cue: if-change
      # as remote was changed 
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        send_inspect = "file",
        overwrite = TRUE
      )
      file.create("_archive/raw-data/v0.0.6/extra.txt")
      projr::projr_build_patch()
      expect_true(file.exists("_archive/raw-data/v0.0.7/data.csv"))
      expect_true(!file.exists("_archive/raw-data/v0.0.7/extra.txt"))

      # inspect nothing, so always add
      projr_yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        send_inspect = "none",
        overwrite = TRUE
      )
      projr::projr_build_patch()
      expect_true(file.exists("_archive/raw-data/v0.0.8/data.csv"))


    }
  )
})
