# Local Remote Tests
#
# This file tests local remote functionality only.
# All GitHub-specific tests have been moved to test-remote-github.R
# All OSF-specific tests have been moved to test-remote-osf.R
#
# These tests run in all modes (CRAN, LITE, full) and do not require
# any external credentials or network access.

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

# Remote creation tests for GitHub and OSF have been moved to:
# - test-remote-github.R (GitHub-specific tests)
# - test-remote-osf.R (OSF-specific tests)

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

# Remote get tests for GitHub and OSF have been moved to:
# - test-remote-github.R (GitHub-specific tests)
# - test-remote-osf.R (OSF-specific tests)

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

# Remote get_final tests for GitHub and OSF have been moved to:
# - test-remote-github.R (GitHub-specific tests)
# - test-remote-osf.R (OSF-specific tests)

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

# Remote rm_final_if_empty tests for GitHub and OSF have been moved to:
# - test-remote-github.R (GitHub-specific tests)
# - test-remote-osf.R (OSF-specific tests)

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

# Remote file_rm_all tests for GitHub and OSF have been moved to:
# - test-remote-github.R (GitHub-specific tests)
# - test-remote-osf.R (OSF-specific tests)

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


