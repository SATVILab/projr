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
