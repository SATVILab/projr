test_that("projr_build_output works - local - defaults", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive"
      )
      # handle nothing to send
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/data-raw")
      ))

      # handle something to upload
      # ---------------------

      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/data-raw/v0.1.2/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # expect no change
      # ----------------------
      projr_build_patch(msg = "I love zis data")
      expect_true(!dir.exists(
        file.path("_archive/data-raw", .projr_version_get_v())
      ))


      # add something
      # ----------------------
      file.create("_data_raw/add.txt")
      projr_build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/data-raw", .projr_version_get_v())
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", .projr_version_get_v(), "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "abc.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))

      # do nothing again
      # ----------------------
      projr_build_patch(msg = "I love zis data")
      expect_true(!dir.exists(
        file.path("_archive/data-raw", .projr_version_get_v())
      ))

      # remove something
      # ----------------------
      file.remove("_data_raw/add.txt")
      projr_build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/data-raw", .projr_version_get_v())
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", .projr_version_get_v(), "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# latest structure with file-based versioning
test_that("projr_build_output works - local - latest - file", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive",
        structure = "latest",
        send_version_source = "file"
      )
      # handle nothing to send
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/data-raw")
      ))

      # handle something to upload
      # ---------------------

      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/data-raw/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # expect no change
      # ----------------------
      # just check this runs without error
      projr_build_patch(msg = "I love zis data")

      # add something
      # ----------------------
      file.create("_data_raw/add.txt")
      projr_build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "abc.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))

      # do nothing again
      # ----------------------
      projr_build_patch(msg = "I love zis data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))

      # remove something
      # ----------------------
      file.remove("_data_raw/add.txt")
      projr_build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works - local - latest - <sync-approach>", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive",
        structure = "latest",
        send_sync_approach = "upload-all"
      )
      # handle nothing to send
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/data-raw")
      ))

      # handle something to upload
      # ---------------------

      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/data-raw/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # add something
      # ----------------------
      file.create("_data_raw/add.txt")
      projr_build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "abc.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))

      # remove something
      # ----------------------
      file.remove("_data_raw/add.txt")
      projr_build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # change to upload-missing
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "upload-missing",
        title = "Raw data", type = "local", profile = "default"
      )
      # just build without anything changing
      projr_build_patch(
        msg = "The more it changes, the more it stays the same"
      )

      # add something
      # ----------------------
      file.create("_data_raw/add2.txt")
      projr_build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "abc.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))

      # remove something
      # ----------------------
      file.remove("_data_raw/add2.txt")
      projr_build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-deletion
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "sync-using-deletion",
        title = "Raw data", type = "local", profile = "default"
      )
      projr_build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )

      projr_build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )
      file.create("_data_raw/add2.txt")
      file.remove("_data_raw/abc.txt")

      projr_build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(!file.exists(
        file.path(path_dir_sub_current |> dirname() |> dirname(), "abc.txt")
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works - local - latest - <sync-approach> - none", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive",
        structure = "latest",
        send_version_source = "none",
        send_sync_approach = "upload-all"
      )
      # handle nothing to send
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/data-raw")
      ))

      # handle something to upload
      # ---------------------

      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/data-raw/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # add something
      # ----------------------
      file.create("_data_raw/add.txt")
      projr_build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "abc.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))

      # remove something
      # ----------------------
      file.remove("_data_raw/add.txt")
      projr_build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # change to upload-missing
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "upload-missing",
        title = "Raw data", type = "local", profile = "default"
      )
      # just build without anything changing
      projr_build_patch(
        msg = "The more it changes, the more it stays the same"
      )

      # add something
      # ----------------------
      file.create("_data_raw/add2.txt")
      projr_build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "abc.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))

      # remove something
      # ----------------------
      file.remove("_data_raw/add2.txt")
      projr_build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-deletion
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "sync-using-deletion",
        title = "Raw data", type = "local", profile = "default"
      )
      projr_build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )

      projr_build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .projr_yml_dest_set_send_sync_approach(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )
      file.create("_data_raw/add2.txt")
      file.remove("_data_raw/abc.txt")

      projr_build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/data-raw/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/data-raw", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(!file.exists(
        file.path(path_dir_sub_current |> dirname() |> dirname(), "abc.txt")
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works - local - latest - none - <conflict>", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive",
        structure = "latest",
        send_version_source = "none",
        send_sync_approach = "upload-all",
        send_conflict = "error"
      )
      # handle nothing to send
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/data-raw")
      ))

      # handle something to upload
      # ---------------------
      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/data-raw/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # error when there is a conflict
      # ---------------------
      expect_error(projr_build_patch(msg = "Ze data"))

      # use skip
      # ----------------------
      .projr_yml_dest_set_send_conflict(
        "skip",
        title = "Raw data", type = "local", profile = "default"
      )
      .file_ls("_data_raw")
      writeLines("abc", "_data_raw/abc.txt")
      projr_build_patch(msg = "Changed but ignored")
      expect_identical(
        readLines("_archive/data-raw/abc.txt"), character()
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works - local - latest - none - <cue>", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive",
        structure = "latest",
        send_sync_approach = "sync-using-deletion",
        send_cue = "patch"
      )

      # cue: patch
      # ---------------------
      # patch build
      file.create("_data_raw/f1.txt")
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/data-raw/f1.txt"))

      # minor build
      file.create("_data_raw/f2.txt")
      projr_build_minor(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/data-raw/f1.txt"))
      expect_true(file.exists("_archive/data-raw/f2.txt"))

      # major build
      file.create("_data_raw/f3.txt")
      projr_build_major(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/data-raw/f1.txt"))
      expect_true(file.exists("_archive/data-raw/f3.txt"))

      # cue: minor
      # ---------------------

      .projr_yml_dest_set_send_cue(
        "minor",
        title = "Raw data", type = "local", profile = "default"
      )
      .dir_rm("_archive/data-raw")
      .dir_rm("_data_raw")
      dir.create("_data_raw")

      # patch build
      file.create("_data_raw/f1.txt")
      projr_build_patch(msg = "Vat are you vinking about")
      expect_false(file.exists("_archive/data-raw/f1.txt"))

      # minor build
      file.create("_data_raw/f2.txt")
      projr_build_minor(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/data-raw/f1.txt"))
      expect_true(file.exists("_archive/data-raw/f2.txt"))

      # major build
      file.create("_data_raw/f3.txt")
      projr_build_major(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/data-raw/f1.txt"))
      expect_true(file.exists("_archive/data-raw/f3.txt"))

      # cue: major
      # ---------------------

      .projr_yml_dest_set_send_cue(
        "major",
        title = "Raw data", type = "local", profile = "default"
      )
      .dir_rm("_archive/data-raw")
      .dir_rm("_data_raw")
      dir.create("_data_raw")

      # patch build
      file.create("_data_raw/f1.txt")
      projr_build_patch(msg = "Vat are you vinking about")
      expect_false(file.exists("_archive/data-raw/f1.txt"))

      # minor build
      file.create("_data_raw/f2.txt")
      projr_build_minor(msg = "Vat are you vinking about")
      expect_false(file.exists("_archive/data-raw/f1.txt"))
      expect_false(file.exists("_archive/data-raw/f2.txt"))

      # major build
      file.create("_data_raw/f3.txt")
      projr_build_major(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/data-raw/f1.txt"))
      expect_true(file.exists("_archive/data-raw/f3.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_build_output works - local - latest - none - don't append label", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
      projr_init()
      # remove remotes
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      expect_identical(projr_version_get(), "0.0.1")
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      expect_identical(projr_version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_local(
        title = "Raw data",
        content = "data-raw",
        path = "_archive",
        path_append_label = FALSE,
        structure = "latest",
        send_sync_approach = "sync-using-deletion"
      )
      # patch build
      file.create("_data_raw/f1.txt")
      projr_build_patch(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/f1.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})
