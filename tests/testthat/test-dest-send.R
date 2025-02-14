test_that("projr_yml_dest_add* functions work", {
  # skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_dest_rm_type_all("default")
      # add one
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive"
      )
      expect_identical(
        .yml_dest_get_type("local", "default"),
        list(
          archive = list(
            content = "raw-data",
            path = "_archive"
          )
        )
      )
      # add two
     .yml_dest_add_local(
        title = "archive second",
        content = "output",
        path = "_archive/second"
      )
      expect_identical(
        .yml_dest_get_type("local", "default"),
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
      .yml_dest_rm_title("archive second", "local", "default")
      expect_identical(
        .yml_dest_get_type("local", "default"),
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
      .yml_dest_rm_type_all("default")
      # add one
     .yml_dest_add_github(
        title = "archive",
        content = "raw-data"
      )
      expect_identical(
        .yml_dest_get_type("github", "default"),
        list(
          archive = list(
            content = "raw-data"
          )
        )
      )
      # add two
     .yml_dest_add_github(
        title = "archive second",
        content = "output"
      )
      expect_identical(
        .yml_dest_get_type("github", "default"),
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
      .yml_dest_rm_title("archive-second", "github", "default")
      expect_identical(
        .yml_dest_get_type("github", "default"),
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
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      file.create(projr:.path_get("raw-data", "data.csv"))
     .init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      projr:.build_dev()
      # remove github remote
      yml_projr <-.yml_get()
      github_ind <- which(names(yml_projr[["build"]]) == "github")
      if (length(github_ind) > 0) {
        yml_projr[["build"]] <- yml_projr[["build"]][
          -github_ind
        ]
        .yml_set(yml_projr)
      }
      projr:.build_patch()
      # add a local destination, that is never sent to
     .yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      projr:.build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        overwrite = TRUE
      )
      projr:.build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))


      # expect no upload
      projr:.build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))

      # now force upload
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        overwrite = TRUE
      )

      projr:.build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))
      expect_true(file.exists("_archive/raw-data/v0.0.5/data.csv"))

     .yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest",
        send_cue = "always"
      )

     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "always",
        send_inspect = "file",
        overwrite = TRUE
      )
      projr:.build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      expect_true(file.exists("_archive/raw-data/v0.0.3/data.csv"))
      expect_true(!dir.exists("_archive/raw-data/v0.0.4"))
      expect_true(file.exists("_archive/raw-data/v0.0.6/data.csv"))

      # new upload, despite no change in project and cue: if-change
      # as remote was changed
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        send_inspect = "file",
        overwrite = TRUE
      )
      file.create("_archive/raw-data/v0.0.6/extra.txt")
      projr:.build_patch()
      expect_true(file.exists("_archive/raw-data/v0.0.7/data.csv"))
      expect_true(!file.exists("_archive/raw-data/v0.0.7/extra.txt"))

      # inspect nothing, so always add
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        send_inspect = "none",
        overwrite = TRUE
      )
      projr:.build_patch()
      expect_true(file.exists("_archive/raw-data/v0.0.8/data.csv"))

      # handle an empty directory
      unlink("_raw_data", recursive = TRUE)
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_cue = "if-change",
        overwrite = TRUE
      )
      projr:.build_patch()
      expect_true(dir.exists("_archive/raw-data/v0.0.9"))
      expect_true(.is_len_0(list.files("_archive/raw-data/v0.0.9")))

    }
  )
})


# --------------------------
# empty directory from the start
# --------------------------

test_that("projr_dest_send works - local - empty dirs", {
  # skip_if(.is_test_select()
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
     .init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      # remove github remote
      .yml_dest_rm_type_all("default")
      # add a local destination, that is never sent to
     .yml_dest_add_local(
        title = "latest",
        content = "raw-data",
        path = "_latest",
        structure = "latest"
      )
      projr:.build_patch()
      expect_true(dir.exists("_latest/raw-data"))
      expect_false(file.exists("_latest/raw-data/data.csv"))
      # add a file
      file.create("_raw_data/data.csv")
      projr:.build_patch()
      expect_true(file.exists("_latest/raw-data/data.csv"))
      # remove that one file
      file.remove("_raw_data/data.csv")
      projr:.build_patch()
      expect_true(dir.exists("_latest/raw-data"))
      expect_false(file.exists("_latest/raw-data/data.csv"))

      # -------------------
      # structure: archive
      # -------------------

      .yml_dest_rm_type_all("default")
     .yml_dest_add_local(
        title = "archive",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        overwrite = TRUE
      )
      projr:.build_patch()
      expect_true(dir.exists("_archive/raw-data/v0.0.4"))
      expect_false(file.exists("_archive/raw-data/v0.0.4/data.csv"))
      # add a file
      # browser()
      # debugonce(.dest_send_label)
      # browser()
      file.create("_raw_data/data.csv")
      projr:.build_patch() # problem isn't here,
      # it's that manifest.csv for v0.0.4 
      # says that data.csv is there when it isn't
      expect_true(file.exists("_archive/raw-data/v0.0.5/data.csv"))
      # remove that one file
      file.remove("_raw_data/data.csv")
      projr:.build_patch()
      expect_true(dir.exists("_archive/raw-data/v0.0.6"))
      expect_false(file.exists("_archive/raw-data/v0.0.6/data.csv"))
      # keep it removed
      projr:.build_patch()
      expect_false(dir.exists("_archive/raw-data/v0.0.7"))
    }
  )
})
