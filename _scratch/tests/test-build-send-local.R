test_that(.build_output works - local - defaults", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive"
      )
      # handle nothing to send
      # ---------------------
     .build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/raw-data")
      ))

      # handle something to upload
      # ---------------------

      .test_setup_content("raw-data")
     .build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/raw-data/v0.1.2/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # expect no change
      # ----------------------
     .build_patch(msg = "I love zis data")
      expect_true(!dir.exists(
        file.path("_archive/raw-data", .version_get_v())
      ))


      # add something
      # ----------------------
      file.create("_raw_data/add.txt")
     .build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/raw-data", .version_get_v())
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", .version_get_v(), "subdir1", "subdir2"
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
     .build_patch(msg = "I love zis data")
      expect_true(!dir.exists(
        file.path("_archive/raw-data", .version_get_v())
      ))

      # remove something
      # ----------------------
      file.remove("_raw_data/add.txt")
     .build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/raw-data", .version_get_v())
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", .version_get_v(), "subdir1", "subdir2"
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
test_that(.build_output works - local - latest - file", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init_full()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive",
        structure = "latest",
        send_inspect = "file"
      )
      # handle nothing to send
      # ---------------------
     .build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/raw-data")
      ))

      # handle something to upload
      # ---------------------

      .test_setup_content("raw-data")
     .build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/raw-data/subdir1/subdir2"
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
     .build_patch(msg = "I love zis data")

      # add something
      # ----------------------
      file.create("_raw_data/add.txt")
     .build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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
     .build_patch(msg = "I love zis data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))

      # remove something
      # ----------------------
      file.remove("_raw_data/add.txt")
     .build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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

test_that(.build_output works - local - latest - <strategy>", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive",
        structure = "latest",
        send_strategy = "upload-all"
      )
      # handle nothing to send
      # ---------------------
     .build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/raw-data")
      ))

      # handle something to upload
      # ---------------------

      .test_setup_content("raw-data")
     .build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/raw-data/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # add something
      # ----------------------
      file.create("_raw_data/add.txt")
     .build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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
      file.remove("_raw_data/add.txt")
     .build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # change to upload-missing
      # ----------------------
      .yml_dest_set_send_strategy(
        "upload-missing",
        title = "Raw data", type = "local", profile = "default"
      )
      # just build without anything changing
     .build_patch(
        msg = "The more it changes, the more it stays the same"
      )

      # add something
      # ----------------------
      file.create("_raw_data/add2.txt")
     .build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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
      file.remove("_raw_data/add2.txt")
     .build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-deletion
      # ----------------------
      .yml_dest_set_send_strategy(
        "sync-using-deletion",
        title = "Raw data", type = "local", profile = "default"
      )
     .build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .yml_dest_set_send_strategy(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )

     .build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .yml_dest_set_send_strategy(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )
      file.create("_raw_data/add2.txt")
      file.remove("_raw_data/abc.txt")

     .build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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

test_that(.build_output works - local - latest - <strategy> - none", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive",
        structure = "latest",
        send_inspect = "none",
        send_strategy = "upload-all"
      )
      # handle nothing to send
      # ---------------------
     .build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/raw-data")
      ))

      # handle something to upload
      # ---------------------

      .test_setup_content("raw-data")
     .build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/raw-data/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # add something
      # ----------------------
      file.create("_raw_data/add.txt")
     .build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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
      file.remove("_raw_data/add.txt")
     .build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # change to upload-missing
      # ----------------------
      .yml_dest_set_send_strategy(
        "upload-missing",
        title = "Raw data", type = "local", profile = "default"
      )
      # just build without anything changing
     .build_patch(
        msg = "The more it changes, the more it stays the same"
      )

      # add something
      # ----------------------
      file.create("_raw_data/add2.txt")
     .build_patch(msg = "More data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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
      file.remove("_raw_data/add2.txt")
     .build_patch(msg = "Less data")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-deletion
      # ----------------------
      .yml_dest_set_send_strategy(
        "sync-using-deletion",
        title = "Raw data", type = "local", profile = "default"
      )
     .build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .yml_dest_set_send_strategy(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )

     .build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
      )
      expect_true(!file.exists(
        file.path(dirname(dirname(path_dir_sub_current)), "add2.txt")
      ))
      expect_true(file.exists(
        file.path(path_dir_sub_current |> dirname(), "def.txt")
      ))

      # sync-using-version
      # ----------------------
      .yml_dest_set_send_strategy(
        "sync-using-version",
        title = "Raw data", type = "local", profile = "default"
      )
      file.create("_raw_data/add2.txt")
      file.remove("_raw_data/abc.txt")

     .build_patch(msg = "Synchronise")
      expect_true(dir.exists(
        file.path("_archive/raw-data/")
      ))
      path_dir_sub_current <- file.path(
        "_archive/raw-data", "subdir1", "subdir2"
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

test_that(.build_output works - local - latest - none - <conflict>", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive",
        structure = "latest",
        send_inspect = "none",
        send_strategy = "upload-all",
        send_conflict = "error"
      )
      # handle nothing to send
      # ---------------------
     .build_patch(msg = "Vat are you vinking about")
      expect_true(dir.exists(
        file.path(dir_test, "_archive/raw-data")
      ))

      # handle something to upload
      # ---------------------
      .test_setup_content("raw-data")
     .build_patch(msg = "Ze data")
      path_sub_dir_2 <- file.path(
        "_archive/raw-data/subdir1/subdir2"
      )
      expect_true(file.exists(
        file.path(path_sub_dir_2 |> dirname(), "def.txt")
      ))
      expect_true(file.exists(
        file.path(dirname(dirname(path_sub_dir_2)), "abc.txt")
      ))

      # error when there is a conflict
      # ---------------------
      expect_error.build_patch(msg = "Ze data"))

      # use skip
      # ----------------------
      .yml_dest_set_send_conflict(
        "skip",
        title = "Raw data", type = "local", profile = "default"
      )
      .file_ls("_raw_data")
      writeLines("abc", "_raw_data/abc.txt")
     .build_patch(msg = "Changed but ignored")
      expect_identical(
        readLines("_archive/raw-data/abc.txt"), character()
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(.build_output works - local - latest - none - <cue>", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive",
        structure = "latest",
        send_strategy = "sync-using-deletion",
        send_cue = "patch"
      )

      # cue: patch
      # ---------------------
      # patch build
      file.create("_raw_data/f1.txt")
     .build_patch(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/raw-data/f1.txt"))

      # minor build
      file.create("_raw_data/f2.txt")
     .build_minor(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/raw-data/f1.txt"))
      expect_true(file.exists("_archive/raw-data/f2.txt"))

      # major build
      file.create("_raw_data/f3.txt")
     .build_major(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/raw-data/f1.txt"))
      expect_true(file.exists("_archive/raw-data/f3.txt"))

      # cue: minor
      # ---------------------

      .yml_dest_set_send_cue(
        "minor",
        title = "Raw data", type = "local", profile = "default"
      )
      .dir_rm("_archive/raw-data")
      .dir_rm("_raw_data")
      dir.create("_raw_data")

      # patch build
      file.create("_raw_data/f1.txt")
     .build_patch(msg = "Vat are you vinking about")
      expect_false(file.exists("_archive/raw-data/f1.txt"))

      # minor build
      file.create("_raw_data/f2.txt")
     .build_minor(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/raw-data/f1.txt"))
      expect_true(file.exists("_archive/raw-data/f2.txt"))

      # major build
      file.create("_raw_data/f3.txt")
     .build_major(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/raw-data/f1.txt"))
      expect_true(file.exists("_archive/raw-data/f3.txt"))

      # cue: major
      # ---------------------

      .yml_dest_set_send_cue(
        "major",
        title = "Raw data", type = "local", profile = "default"
      )
      .dir_rm("_archive/raw-data")
      .dir_rm("_raw_data")
      dir.create("_raw_data")

      # patch build
      file.create("_raw_data/f1.txt")
     .build_patch(msg = "Vat are you vinking about")
      expect_false(file.exists("_archive/raw-data/f1.txt"))

      # minor build
      file.create("_raw_data/f2.txt")
     .build_minor(msg = "Vat are you vinking about")
      expect_false(file.exists("_archive/raw-data/f1.txt"))
      expect_false(file.exists("_archive/raw-data/f2.txt"))

      # major build
      file.create("_raw_data/f3.txt")
     .build_major(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/raw-data/f1.txt"))
      expect_true(file.exists("_archive/raw-data/f3.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(.build_output works - local - latest - none - don't append label", {
  skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # initialse `projr` project
     .init()
      # remove remotes
      .test_yml_unset_remote()
      .yml_git_set_commit(TRUE, TRUE, NULL)
      .yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .yml_git_set_push(FALSE, TRUE, NULL)
     .build_output("patch", msg = "test")
     .version_get()
      expect_identical.version_get(), "0.0.1")
      yml_bd <- .yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
     .build_output("minor", msg = "test")
      expect_identical.version_get(), "0.1.0")
      # no add that we're pushing to GitHub, but
      # raw-data and source are empty
     .yml_dest_add_local(
        title = "Raw data",
        content = "raw-data",
        path = "_archive",
        path_append_label = FALSE,
        structure = "latest",
        send_strategy = "sync-using-deletion"
      )
      # patch build
      file.create("_raw_data/f1.txt")
     .build_patch(msg = "Vat are you vinking about")
      expect_true(file.exists("_archive/f1.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})
