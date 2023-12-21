test_that("projr_hash_dir works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # test hashing empty directory
      path_dir_empty <- file.path(tempdir(), "abc")
      .dir_rm(path_dir_empty)
      dir.create(path_dir_empty)
      hash_tbl <- .projr_hash_dir(path_dir_empty)
      expect_identical(nrow(hash_tbl), 0L)
      # test hashing empty directory with a sub-directory
      dir.create(file.path(path_dir_empty, "def"))
      hash_tbl <- .projr_hash_dir(path_dir_empty)
      expect_identical(nrow(hash_tbl), 0L)

      # test hashing non-empty directories
      path_dir <- .projr_test_setup_content_dir()
      hash_tbl <- .projr_hash_dir(path_dir)
      expect_identical(nrow(hash_tbl), 4L)
      expect_identical(length(unique(hash_tbl$hash)), 1L)
    }
  )
})
