test_that(".projr_local_dir_create works", {
  # skips
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_local_dir_create(c("a", "b", "b/c", "b/c/d", "e/f/g/h"))
      expect_true(dir.exists("b"))
      expect_true(dir.exists("e/f/g"))
      expect_true(!dir.exists("e/f/g/h"))
      expect_true(!dir.exists("a"))
    }
  )
})

test_that(".projr_local_conflict_manage works", {
  # skips
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      path_dir_source <- "dir_1/dir_2/dir_3"
      path_dir_dest <- "dir_dest"
      dir.create(path_dir_source, recursive = TRUE)
      dir.create(path_dir_dest, recursive = TRUE)
      file.create(file.path(path_dir_source, "b.txt"))
      file.create(file.path(dirname(path_dir_source), "a.txt"))
      file.create(file.path(path_dir_dest, "b.txt"))
      path_file_source <- file.path(path_dir_source, "b.txt")
      path_file_dest <- file.path(path_dir_dest, "b.txt")
      # conflict
      expect_error(.projr_local_conflict_manage(
        path_file_source = file.path(path_dir_source, "b.txt"),
        path_file_dest = file.path(path_dir_dest, "b.txt"),
        conflict = "error"
      ))
      # skip
      expect_identical(
        .projr_local_conflict_manage(
          path_file_source = c(
            file.path(path_dir_source, "b.txt"),
            file.path(dirname(path_dir_source), "a.txt")
          ),
          path_file_dest = c(
            file.path(path_dir_source, "b.txt"),
            file.path(dirname(path_dir_dest), "a.txt")
          ),
          conflict = "skip"
        ),
        list(
          "path_file_source" = file.path(dirname(path_dir_source), "a.txt"),
          "path_file_dest" = file.path(dirname(path_dir_dest), "a.txt")
        )
      )
      # overwrite
      expect_identical(
        .projr_local_conflict_manage(
          path_file_source = c(
            file.path(path_dir_source, "b.txt"),
            file.path(dirname(path_dir_source), "a.txt")
          ),
          path_file_dest = c(
            file.path(path_dir_source, "b.txt"),
            file.path(dirname(path_dir_dest), "a.txt")
          ),
          conflict = "overwrite"
        ),
        list(
          "path_file_source" = c(
            file.path(path_dir_source, "b.txt"),
            file.path(dirname(path_dir_source), "a.txt")
          ),
          "path_file_dest" = c(
            file.path(path_dir_source, "b.txt"),
            file.path(dirname(path_dir_dest), "a.txt")
          )
        )
      )
    }
  )
})


test_that(".projr_local_send_file works", {
  # skips
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_false(.projr_local_send_file(fn_rel = NULL))
      file.create("a.txt")
      expect_false(.projr_local_send_file(
        fn_rel = "a.txt",
        path_dir_local = "b",
        path_dir_local_dest = ".",
        conflict = "skip"
      ))
      dir.create("dir_1/dir_2", recursive = TRUE)
      file.create("dir_1/b.txt")
      file.create("dir_1/dir_2/c.txt")
      expect_true(.projr_local_send_file(
        fn_rel = c("a.txt", "dir_1/b.txt", "dir_1/dir_2/c.txt"),
        path_dir_local = ".",
        path_dir_local_dest = "dir_dest",
        conflict = "overwrite"
      ))
    }
  )
})
