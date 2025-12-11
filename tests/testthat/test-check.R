test_that(".cli_debug logging works in assertion functions", {
  skip_if(.is_test_select())

  # Setup a temporary test project directory with logging enabled
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize logging
      Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
      log_info <- .log_build_init(
        build_type = "dev", bump_component = "test",
        msg = "Testing CLI debug", output_level = "debug"
      )

      # Test that validation failures trigger .cli_debug calls
      # We can't directly verify the log content without reading the file,
      # but we can verify the assertions still work correctly

      # Test string validation
      expect_error(.assert_string(c("a", "b"), required = TRUE),
        regexp = "must be a non-empty string"
      )

      # Test flag validation
      expect_error(.assert_flag(c(TRUE, FALSE)),
        regexp = "must be a non-NA flag"
      )

      # Test numeric validation
      expect_error(.assert_number(c(1, 2)),
        regexp = "must be a non-NA number"
      )

      # Test class validation
      expect_error(.assert_class_exact(data.frame(x = 1), "matrix"),
        regexp = "must have exactly the following class"
      )

      # Test options validation
      expect_error(.assert_in("invalid", c("a", "b", "c"), required = TRUE),
        regexp = "must be one of"
      )

      # Test directory existence
      expect_error(.assert_dir_exists_single("nonexistent_dir"),
        regexp = "must exist"
      )

      # Finalize log
      if (!is.null(log_info)) {
        .log_build_finalize(success = TRUE, start_time = Sys.time())

        # Verify that a log file was created
        expect_true(file.exists(log_info$log_file))
      }
    }
  )
})

test_that("All assertion functions work as expected", {
  skip_if(.is_test_cran())
  skip_if_offline()
  skip_if(.is_test_select())

  # Setup a temporary test project directory
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Directories --------------------------------------------

      # single directory existence
      expect_error(.assert_dir_exists_single(c("a", "b")),
        regexp = "must have length one"
      )
      expect_error(.assert_dir_exist_single("a"),
        regexp = "could not find function"
      ) # checking typo intentionally
      # correct function name is `.assert_dir_exists_single`
      expect_error(.assert_dir_exists_single("a"),
        regexp = "must exist"
      )
      dir.create("a")
      expect_true(.assert_dir_exists_single("a", TRUE)) # now it should pass

      # multiple directories existence
      expect_error(.assert_dir_exists(c("a", "b")),
        regexp = "must exist"
      )
      .dir_create("b") # create directory b
      expect_true(.assert_dir_exists(c("a", "b"), TRUE))


      # Character Strings ---------------------------------------

      # single string
      a <- "abc"
      expect_true(.assert_string(a, FALSE)) # optional, given string
      expect_true(.assert_string(a, TRUE)) # required, given string
      expect_error(.assert_string(letters),
        regexp = "must be a non-empty string"
      )

      # NULL with required/optional
      a <- NULL
      expect_true(.assert_string(a, FALSE)) # optional: passes if not given
      expect_error(.assert_string(a, TRUE),
        regexp = "must be given"
      )

      # character vectors
      x <- letters
      expect_true(.assert_chr(x, TRUE)) # character vector with letters is fine

      # checking number of characters
      # `.assert_nchar()` tests that all elements have a certain length
      expect_error(.assert_nchar(x, 2),
        regexp = "must be non-empty"
      )
      expect_true(.assert_nchar(x, 1))
      expect_error(.assert_nchar_single(x[[1]], 2),
        regexp = "must be 2 characters long"
      )
      expect_true(.assert_nchar_single(x[[1]], 1)) # single char 'a'


      # Logical Flags --------------------------------------------

      # single logical (flag)
      expect_error(.assert_flag(letters[1]),
        regexp = "must be a non-NA flag"
      )
      expect_error(.assert_flag(logical(2)),
        regexp = "must be a non-NA flag"
      )
      expect_error(.assert_flag(character(0L)),
        regexp = "must be a non-NA flag"
      )
      expect_true(.assert_flag(TRUE)) # single TRUE is a valid flag
      expect_true(.assert_flag(FALSE)) # single FALSE is also valid

      # multiple logicals
      # `.assert_lgl()` requires non-NA logical vector
      expect_error(.assert_lgl(letters[1]),
        regexp = "must be a non-NA logical"
      )
      expect_true(.assert_lgl(c(TRUE, FALSE, TRUE)))


      # Numeric Checks -------------------------------------------

      # `.assert_num()` requires non-NA numeric vector with positive length
      expect_error(.assert_num(letters),
        regexp = "must be a non-NA numeric vector"
      )
      expect_error(.assert_num(numeric(0)),
        regexp = "must be a non-NA numeric vector"
      )
      expect_error(.assert_num(c(1, NA)),
        regexp = "must be a non-NA numeric vector"
      )
      expect_true(.assert_num(c(1, 2, 3)))

      # single number
      expect_error(.assert_number(c(1, 2)),
        regexp = "must be a non-NA number"
      )
      expect_error(.assert_number(NA_real_),
        regexp = "must be a non-NA number"
      )
      expect_true(.assert_number(42))


      # Class Checks ---------------------------------------------

      # `.assert_class_exact()` requires class to match exactly (after sorting)
      df <- data.frame(x = 1:3)
      expect_error(.assert_class_exact(df, "matrix"),
        regexp = "must have exactly the following class"
      )
      expect_true(.assert_class_exact(df, c("data.frame")))

      # `.assert_class_all()` requires object to inherit from all listed classes
      lm_obj <- lm(x ~ 1, data = df)
      expect_error(.assert_class_all(lm_obj, c("data.frame", "lm")),
        regexp = "must have all of the following class"
      )
      expect_true(.assert_class_all(lm_obj, c("lm")))

      # `.assert_class_any()` requires object to inherit from at least one listed class
      expect_error(.assert_class_any(lm_obj, c("data.frame", "matrix")),
        regexp = "must have at least one of the following class"
      )
      expect_true(.assert_class_any(lm_obj, c("lm", "list")))


      # Attribute Checks -----------------------------------------

      x_with_attr <- structure(1:3, my_attr = "value")
      # `.assert_attr_value()` checks exact value for given attribute
      expect_true(.assert_attr_value(x_with_attr, "my_attr", "value"))
      expect_error(.assert_attr_value(x_with_attr, "my_attr", "other_value"),
        regexp = "must have attribute my_attr with value"
      )

      # `.assert_attr_exact()` checks that the object has exactly these attributes
      expect_error(.assert_attr_exact(x_with_attr, "other_attr"),
        regexp = "must have exactly the following attribute"
      )
      expect_true(.assert_attr_exact(x_with_attr, "my_attr"))

      # `.assert_attr()` checks that an attribute is present
      expect_error(.assert_attr(x_with_attr, "missing_attr"),
        regexp = "must have attribute missing_attr"
      )
      expect_true(.assert_attr(x_with_attr, "my_attr"))


      # Options / Membership Checks ------------------------------

      # `.assert_in()` checks if all values are in a given set
      fruits <- c("apple", "banana", "cherry")
      expect_error(.assert_in("pear", fruits, required = TRUE),
        regexp = "must be one of apple, banana, cherry"
      )
      expect_true(.assert_in("apple", fruits, required = TRUE))
      expect_true(.assert_in(c("apple", "banana"), fruits))

      # `.assert_in_not()` checks if values are NOT in a given set
      expect_error(.assert_in_not("apple", fruits),
        regexp = "must not be one of"
      )
      expect_true(.assert_in_not("grape", fruits))

      # `.assert_in_single()` requires exactly one element in `x` that is in `opt`
      expect_error(.assert_in_single(c("apple", "banana"), fruits),
        regexp = "must have length one"
      )
      expect_true(.assert_in_single("apple", fruits))


      # Pattern Detection ----------------------------------------

      # `.assert_detect()` checks all elements match a pattern
      letters_subset <- c("apple", "application")
      expect_true(.assert_detect(letters_subset, "^app"))
      expect_error(.assert_detect(letters, "^app"),
        regexp = "must all match"
      )

      # `.assert_detect_any()` checks if any element matches a pattern
      expect_true(.assert_detect_any(letters, "^a"))
      expect_error(.assert_detect_any(letters, "^xyz"),
        regexp = "must contain at least one match"
      )

      # `.assert_detect_single()` checks single string matches pattern
      expect_error(.assert_detect_single(letters, "^a"),
        regexp = "must have length one"
      )
      expect_true(.assert_detect_single("apple", "^app"))
      expect_error(.assert_detect_single("banana", "^app"),
        regexp = "must match"
      )


      # Length Checks --------------------------------------------

      # `.assert_len_1()` checks length one
      expect_error(.assert_len_1(letters),
        regexp = "must have length one"
      )
      expect_true(.assert_len_1("apple"))

      # `.assert_len_pos()` checks positive length
      expect_error(.assert_len_pos(character(0)),
        regexp = "must have positive length"
      )
      expect_true(.assert_len_pos(letters))


      # Other General Checks -------------------------------------

      # `.assert_flag_min()` checks if it’s a logical vector of length one (flag), NA allowed
      # `.assert_flag()` is stricter (no NA allowed).
      expect_true(.assert_flag_min(TRUE))
      expect_true(.assert_flag_min(FALSE))
      expect_true(.assert_flag_min(NA)) # allowed by _min variant
      expect_error(.assert_flag_min("not logical"),
        regexp = "must be a flag"
      )

      # `.assert_num_min()`, `.assert_chr_min()` etc. check minimal conditions
      expect_true(.assert_num_min(numeric(0))) # Just checks is.numeric
      expect_error(.assert_num_min("not numeric"),
        regexp = "must be a numeric vector"
      )
      expect_true(.assert_chr_min(character(0))) # Just checks is.character
      expect_error(.assert_chr_min(1:5),
        regexp = "must be character"
      )

      # `.assert_string_min()` checks if length-one character (string), but can be empty
      expect_true(.assert_string_min("hello"))
      expect_true(.assert_string_min("")) # allowed since only checks length one character
      expect_error(.assert_string_min(c("a", "b")),
        regexp = "must be a string"
      )
    }
  )
})

test_that("Additional untested functions work correctly", {
  skip_if(.is_test_select())

  # Setup a temporary test project directory
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # .assert_chr_mid() - character vector with no NA, allows empty strings
      expect_true(.assert_chr_mid(c("", "a", "b")))
      expect_true(.assert_chr_mid(letters))
      expect_error(.assert_chr_mid(c("a", NA, "b")),
        regexp = "must be a non-empty character vector with no NA entries"
      )
      expect_error(.assert_chr_mid(character(0)),
        regexp = "must be a non-empty character vector with no NA entries"
      )
      expect_error(.assert_chr_mid(123),
        regexp = "must be a non-empty character vector with no NA entries"
      )

      # .assert_class_exact_unsorted() - exact class match without sorting
      df <- data.frame(x = 1:3)
      expect_true(.assert_class_exact_unsorted(df, "data.frame"))
      expect_error(.assert_class_exact_unsorted(df, c("tbl", "data.frame")),
        regexp = "must have exactly the following class"
      )

      # .assert_given_full() - checks given and not NA
      expect_true(.assert_given_full(1:5))
      expect_true(.assert_given_full("test"))
      expect_error(.assert_given_full(c(1, NA, 3)),
        regexp = "must be given"
      )
      x_null <- NULL
      expect_error(.assert_given_full(x_null),
        regexp = "must be given"
      )

      # .assert_has() - checks vector contains all specified values
      expect_true(.assert_has(letters, c("a", "b", "c")))
      expect_error(.assert_has(letters, c("a", "z", "xyz")),
        regexp = "must contain all the following value"
      )

      # .assert_has_not() - checks vector does not contain specified values
      expect_true(.assert_has_not(letters, c("A", "B", "123")))
      expect_error(.assert_has_not(letters, c("a", "xyz")),
        regexp = "must not contain any of the following value"
      )

      # .assert_in_single_not() - single value not in options
      expect_true(.assert_in_single_not("x", c("a", "b", "c")))
      expect_error(.assert_in_single_not("a", c("a", "b", "c")),
        regexp = "must not be one of"
      )
      expect_error(.assert_in_single_not(c("x", "y"), c("a", "b")),
        regexp = "must have length one"
      )

      # .assert_len() - checks exact length
      expect_true(.assert_len(letters, 26))
      expect_error(.assert_len(letters, 10),
        regexp = "must be length 10"
      )
      expect_error(.assert_len(character(0), 5),
        regexp = "must be length 5"
      )

      # .assert_lgl_min() - checks is logical, allows NA
      expect_true(.assert_lgl_min(c(TRUE, FALSE)))
      expect_true(.assert_lgl_min(c(TRUE, NA, FALSE)))
      expect_error(.assert_lgl_min("not logical"),
        regexp = "must be a logical vector"
      )

      # .assert_number_min() - checks single numeric, allows NA
      expect_true(.assert_number_min(42))
      expect_true(.assert_number_min(NA_real_))
      expect_error(.assert_number_min(c(1, 2)),
        regexp = "must be a number"
      )

      # .assert_nz() - checks non-empty with nzchar
      expect_true(.assert_nz(c("a", "b", "c")))
      expect_error(.assert_nz(c("a", "")),
        regexp = "must be non-empty"
      )
      expect_error(.assert_nz(character(0)),
        regexp = "must be non-empty"
      )

      # .assert_opt_single() - checks single value is in options
      expect_true(.assert_opt_single("a", c("a", "b", "c")))
      expect_error(.assert_opt_single("x", c("a", "b", "c")),
        regexp = "must be one of"
      )
      expect_error(.assert_opt_single(c("a", "b"), c("a", "b", "c")),
        regexp = "must have length one"
      )

      # .assert_path_not_file() - checks path is not an existing file
      dir.create("test_dir")
      expect_true(.assert_path_not_file("test_dir"))
      expect_true(.assert_path_not_file("nonexistent"))
      writeLines("test", "test_file.txt")
      expect_error(.assert_path_not_file("test_file.txt"),
        regexp = "must not be a pre-existing file"
      )

      # .assert_path_not_sub() - checks path is not subdirectory of another
      dir.create("parent_dir")
      dir.create("parent_dir/child")
      expect_error(.assert_path_not_sub("parent_dir/child", "parent_dir"),
        regexp = "must not be a subdirectory"
      )
      expect_true(.assert_path_not_sub("other_dir", "parent_dir"))

      # .assert_string_mid() - checks non-empty string, no NA
      expect_true(.assert_string_mid("hello"))
      expect_error(.assert_string_mid(""),
        regexp = "must be a non-empty string"
      )
      expect_error(.assert_string_mid(c("a", "b")),
        regexp = "must be a non-empty string"
      )

      # .chr_cap() - caps character vector elements
      expect_identical(.chr_cap(c("hello", "world"), 3), c("hel", "wor"))
      expect_identical(.chr_cap("test", 10), "test")
      expect_identical(.chr_cap(c("a", "bb", "ccc"), 2), c("a", "bb", "cc"))

      # .is_file_exists_description() - checks if DESCRIPTION exists
      # Returns TRUE if DESCRIPTION file exists in project
      desc_exists <- .is_file_exists_description()
      expect_true(is.logical(desc_exists))
      expect_true(length(desc_exists) == 1)

      # .is_given_full() - checks given and not NA
      expect_true(.is_given_full(1:5))
      expect_false(.is_given_full(c(1, NA, 3)))
      expect_false(.is_given_full(NULL))

      # .is_len_0() - checks length zero
      expect_true(.is_len_0(character(0)))
      expect_true(.is_len_0(numeric(0)))
      expect_false(.is_len_0(c(1, 2, 3)))

      # .is_path_not_file() - checks path is not an existing file
      expect_true(.is_path_not_file("nonexistent"))
      expect_true(.is_path_not_file("test_dir"))
      expect_false(.is_path_not_file("test_file.txt"))

      # .is_try_error() - checks if object is try-error
      result_ok <- try(1 + 1, silent = TRUE)
      result_err <- try(stop("error"), silent = TRUE)
      expect_false(.is_try_error(result_ok))
      expect_true(.is_try_error(result_err))

      # .path_get_proj() - gets project path
      proj_path <- .path_get_proj()
      expect_true(is.character(proj_path))
      expect_true(length(proj_path) == 1)
      expect_true(nzchar(proj_path))

      # .string_cap() - caps single string
      expect_identical(.string_cap("hello world", 5), "hello")
      expect_identical(.string_cap("test", 10), "test")
      expect_identical(.string_cap("abcdefghij", 5), "abcde")
    }
  )
})

test_that("Partially tested functions have full coverage", {
  skip_if(.is_test_select())

  # Setup a temporary test project directory
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # .assert_chr() - additional edge cases
      expect_error(.assert_chr(c("", "a")),
        regexp = "must be a non-empty character vector"
      )
      expect_error(.assert_chr(c("a", NA)),
        regexp = "must be a non-empty character vector"
      )
      x_chr_null <- NULL
      # .assert_chr returns FALSE when given NULL and required=FALSE
      expect_false(.assert_chr(x_chr_null, required = FALSE))

      # .assert_given() - edge cases with missing argument
      expect_error(.assert_given(NULL),
        regexp = "must be given"
      )

      # .assert_nm_get() - test with invalid nm parameter
      expect_error(.assert_nm_get(123, nm = 123),
        regexp = "`nm` must be a string"
      )

      # .is_given() - comprehensive test
      x_missing <- NULL
      expect_false(.is_given()) # test with missing argument
      expect_true(.is_given(x_missing)) # NULL is "given"

      # .assert_len_1() - NULL handling
      x_null <- NULL
      expect_true(.assert_len_1(x_null, required = FALSE))

      # Test _min variants more thoroughly
      expect_true(.assert_chr_min(character(0)))
      expect_true(.assert_chr_min(c("", NA)))

      # Test directory functions with required = FALSE (NULL is ok)
      expect_true(.assert_dir_exists_single(NULL, required = FALSE))
      expect_true(.assert_dir_exists(NULL, required = FALSE))

      # Test various functions with optional (required = FALSE, using NULL)
      # When required=FALSE and value is NULL, validation is skipped
      expect_true(.assert_flag(NULL, required = FALSE))
      expect_true(.assert_flag_min(NULL, required = FALSE))
      expect_true(.assert_lgl(NULL, required = FALSE))
      # These return FALSE when NULL and required=FALSE
      expect_false(.assert_nchar(NULL, 5, required = FALSE))
      expect_false(.assert_nchar_single(NULL, 5, required = FALSE))
      expect_true(.assert_num(NULL, required = FALSE))
      expect_true(.assert_num_min(NULL, required = FALSE))
      expect_true(.assert_number(NULL, required = FALSE))
      expect_true(.assert_string_min(NULL, required = FALSE))

      # Test class functions with required = FALSE (using NULL)
      expect_true(.assert_class_all(NULL, "character", required = FALSE))
      expect_true(.assert_class_any(NULL, "character", required = FALSE))
      expect_true(.assert_class_exact(NULL, "character", required = FALSE))

      # Test attribute functions with required = FALSE
      expect_true(.assert_attr(NULL, "test", required = FALSE))

      # Test detect functions with required = FALSE
      expect_true(.assert_detect(NULL, "pattern", required = FALSE))
      expect_true(.assert_detect_any(NULL, "pattern", required = FALSE))
      expect_true(.assert_detect_single(NULL, "pattern", required = FALSE))

      # Test membership functions with required = FALSE
      expect_true(.assert_in(NULL, c("a", "b"), required = FALSE))
      expect_true(.assert_in_not(NULL, c("a", "b"), required = FALSE))
      expect_true(.assert_in_single(NULL, c("a", "b"), required = FALSE))
    }
  )
})

test_that("Edge cases for remaining functions with partial coverage", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # .assert_path_not_sub() - additional edge case
      dir.create("parent")
      dir.create("parent/child", recursive = TRUE)
      # Test with NULL when not required
      expect_true(.assert_path_not_sub(NULL, "parent", required = FALSE))

      # .assert_opt_single() - additional edge case with NULL
      expect_true(.assert_opt_single(NULL, c("a", "b"), required = FALSE))

      # .assert_len() - additional edge case with NULL
      expect_true(.assert_len(NULL, 5, required = FALSE))

      # .assert_attr() - test when object doesn't have attribute
      x_no_attr <- 1:5
      expect_error(.assert_attr(x_no_attr, "missing_attr", required = TRUE),
        regexp = "must have attribute missing_attr"
      )

      # .assert_chr_mid() - test with NULL when not required
      expect_true(.assert_chr_mid(NULL, required = FALSE))

      # .assert_chr_min() - test edge case
      expect_true(.assert_chr_min(NULL, required = FALSE))

      # .assert_len_pos() - test with NULL when not required
      expect_true(.assert_len_pos(NULL, required = FALSE))

      # .assert_lgl_min() - test with NULL when not required
      expect_true(.assert_lgl_min(NULL, required = FALSE))

      # .assert_number_min() - test with NULL when not required
      expect_true(.assert_number_min(NULL, required = FALSE))

      # .assert_nz() - test with NULL when not required
      expect_false(.assert_nz(NULL, required = FALSE))

      # .assert_path_not_file() - test with NULL when not required
      expect_true(.assert_path_not_file(NULL, required = FALSE))

      # .assert_string_mid() - test with NULL when not required
      expect_true(.assert_string_mid(NULL, required = FALSE))

      # .assert_attr_exact() - additional edge cases
      x_with_attr <- structure(1:3, attr1 = "val1", attr2 = "val2")
      expect_error(.assert_attr_exact(x_with_attr, c("attr1", "attr3")),
        regexp = "must have exactly the following attribute"
      )
      expect_true(.assert_attr_exact(x_with_attr, c("attr1", "attr2")))

      # .assert_class_exact_unsorted() - test with NULL when not required
      expect_true(.assert_class_exact_unsorted(NULL, "character", required = FALSE))

      # .assert_has() - additional edge cases
      expect_true(.assert_has(NULL, "value", required = FALSE))

      # .assert_has_not() - additional edge cases
      expect_true(.assert_has_not(NULL, "value", required = FALSE))

      # .assert_in_single_not() - additional edge cases
      expect_true(.assert_in_single_not(NULL, c("a", "b"), required = FALSE))

      # .assert_attr_value() - additional edge cases
      x_attr <- structure(1:3, test_attr = "expected")
      expect_true(.assert_attr_value(x_attr, "test_attr", "expected"))
      expect_true(.assert_attr_value(NULL, "test_attr", "val", required = FALSE))

      # .assert_path_not_sub() - test with multiple parent paths
      dir.create("parent2")
      expect_true(.assert_path_not_sub("other_path", c("parent", "parent2")))
      # Test case where path IS a subdirectory of one of multiple parents
      dir.create("parent2/child2", recursive = TRUE)
      expect_error(.assert_path_not_sub("parent2/child2", c("parent", "parent2")),
        regexp = "must not be a subdirectory"
      )

      # .assert_attr_exact() - test when attributes don't match (different order)
      x_multi_attr <- structure(1:3, z_attr = "z", a_attr = "a")
      expect_true(.assert_attr_exact(x_multi_attr, c("a_attr", "z_attr")))

      # .assert_len() - test with explicit nm parameter
      expect_error(.assert_len(1:3, 5, required = TRUE, nm = "custom_name"),
        regexp = "custom_name must be length 5"
      )

      # .assert_attr() - test when object has the attribute
      x_with_attr <- structure(1:5, my_attr = "value")
      expect_true(.assert_attr(x_with_attr, "my_attr", required = TRUE))
      # Test with required=TRUE and object without attribute
      expect_error(.assert_attr(1:5, "missing_attr", required = TRUE),
        regexp = "must have attribute missing_attr"
      )
    }
  )
})
