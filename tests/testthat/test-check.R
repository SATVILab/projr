test_that(".projr_remote_create works", {
  skip_if_offline()
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # directories
      # -----------
      # single
      expect_error(.assert_dir_exists_single(c("a", "b")))
      expect_error(.assert_dir_exist_single("a"))
      dir.create("a")
      expect_true(.assert_dir_exists_single("a", TRUE))
      # multiple
      expect_error(.assert_dir_exists(c("a", "b")))
      .projr_dir_create("a")
      .projr_dir_create("b")
      expect_true(.assert_dir_exists("a", TRUE))

      # character
      # ---------
      # single
      a <- "abc"
      expect_true(.assert_string(a, FALSE))
      expect_true(.assert_string(a, TRUE))
      expect_error(.assert_string(letters))
      a <- NULL
      expect_true(.assert_string(a, FALSE))
      expect_error(.assert_string(a, TRUE))
      expect_error(.assert_string(a, TRUE), regexp = "a must be given")
      # multiple
      expect_true(.assert_string(a, FALSE))
      x <- letters
      expect_true(.assert_chr(x, TRUE))
      # number of characters
      expect_error(.assert_nchar(x, 2))
      expect_true(.assert_nchar(x, 1))
      expect_error(.assert_nchar_single(x, 1))
      expect_error(.assert_nchar_single(x[[1]], 2))
      expect_true(.assert_nchar_single(x[[1]], 1))

      # logical
      # -------
      # single
      expect_error(.assert_flag(letters[1]))
      expect_error(.assert_flag(logical(2)))
      expect_true(.assert_flag(logical(1)))
      expect_error(.assert_flag(character(0L)))
      # multiple
      expect_error(.assert_lgl(letters[1]))
      expect_true(.assert_lgl(logical(2)))
    }
  )
})
