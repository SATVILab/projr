test_that(".init_prompt_ind returns answer_auto in test mode", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with answer_auto = "test_value"
  result <- .init_prompt_ind(
    .var = "TEST_VAR",
    nm_item_long = "test item",
    option_default = NULL,
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_translate = NULL,
    answer_auto = "test_value"
  )

  expect_identical(result, "test_value")
})

test_that(".init_prompt_ind returns NULL for answer_auto = NULL", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  result <- .init_prompt_ind(
    .var = "TEST_VAR",
    nm_item_long = "test item",
    option_default = NULL,
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_translate = NULL,
    answer_auto = NULL
  )

  expect_null(result)
})

test_that(".init_prompt_ind returns various answer types", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with numeric answer
  result_num <- .init_prompt_ind(
    .var = "TEST_VAR",
    nm_item_long = "test item",
    answer_auto = 42
  )
  expect_identical(result_num, 42)

  # Test with character answer
  result_chr <- .init_prompt_ind(
    .var = "TEST_VAR",
    nm_item_long = "test item",
    answer_auto = "custom_answer"
  )
  expect_identical(result_chr, "custom_answer")

  # Test with logical answer
  result_log <- .init_prompt_ind(
    .var = "TEST_VAR",
    nm_item_long = "test item",
    answer_auto = TRUE
  )
  expect_identical(result_log, TRUE)
})

test_that(".init_prompt_ind_translate returns nm_item when answer_translate is NULL", {
  skip_if(.is_test_select())

  result <- .init_prompt_ind_translate("test_value", NULL)
  expect_identical(result, "test_value")

  result2 <- .init_prompt_ind_translate("another_value", NULL)
  expect_identical(result2, "another_value")
})

test_that(".init_prompt_ind_translate translates using answer_translate list", {
  skip_if(.is_test_select())

  answer_translate <- list(
    "Yes" = "y",
    "No" = "n",
    "Maybe" = "m"
  )

  result_yes <- .init_prompt_ind_translate("Yes", answer_translate)
  expect_identical(result_yes, "y")

  result_no <- .init_prompt_ind_translate("No", answer_translate)
  expect_identical(result_no, "n")

  result_maybe <- .init_prompt_ind_translate("Maybe", answer_translate)
  expect_identical(result_maybe, "m")
})

test_that(".init_prompt_ind_translate handles NULL nm_item", {
  skip_if(.is_test_select())

  answer_translate <- list("Yes" = "y")

  # NULL with answer_translate should error (attempt to select less than one element)
  expect_error(
    .init_prompt_ind_translate(NULL, answer_translate),
    "attempt to select less than one element"
  )

  # NULL with NULL answer_translate should return NULL
  result <- .init_prompt_ind_translate(NULL, NULL)
  expect_null(result)
})

test_that(".init_prompt_ind_translate handles missing keys", {
  skip_if(.is_test_select())

  answer_translate <- list("Yes" = "y")

  # Item not in translate list should return NULL (list access behavior)
  result <- .init_prompt_ind_translate("NotInList", answer_translate)
  expect_null(result)
})

test_that(".init_prompt_yn returns answer_auto in test mode", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with default answer_auto = 2 (No)
  result_default <- .init_prompt_yn("Test question?")
  expect_identical(result_default, 2)

  # Test with custom answer_auto = 1 (Yes)
  result_yes <- .init_prompt_yn("Test question?", answer_auto = 1)
  expect_identical(result_yes, 1)

  # Test with custom answer_auto = 2 (No)
  result_no <- .init_prompt_yn("Test question?", answer_auto = 2)
  expect_identical(result_no, 2)
})

test_that(".init_prompt_yn handles various answer_auto values", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with numeric values
  result1 <- .init_prompt_yn("Question?", answer_auto = 1)
  expect_identical(result1, 1)

  result2 <- .init_prompt_yn("Question?", answer_auto = 2)
  expect_identical(result2, 2)

  # Test with non-standard values
  result3 <- .init_prompt_yn("Question?", answer_auto = 0)
  expect_identical(result3, 0)

  result4 <- .init_prompt_yn("Question?", answer_auto = 999)
  expect_identical(result4, 999)
})

test_that(".init_prompt_ind with allow_specify_other variations", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with allow_specify_other = TRUE
  result1 <- .init_prompt_ind(
    nm_item_long = "test item",
    allow_specify_other = TRUE,
    allow_complete_later = FALSE,
    answer_auto = "value1"
  )
  expect_identical(result1, "value1")

  # Test with allow_specify_other = FALSE
  result2 <- .init_prompt_ind(
    nm_item_long = "test item",
    allow_specify_other = FALSE,
    allow_complete_later = FALSE,
    answer_auto = "value2"
  )
  expect_identical(result2, "value2")
})

test_that(".init_prompt_ind with allow_complete_later variations", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with allow_complete_later = TRUE
  result1 <- .init_prompt_ind(
    nm_item_long = "test item",
    allow_specify_other = FALSE,
    allow_complete_later = TRUE,
    answer_auto = "value1"
  )
  expect_identical(result1, "value1")

  # Test with allow_complete_later = FALSE
  result2 <- .init_prompt_ind(
    nm_item_long = "test item",
    allow_specify_other = FALSE,
    allow_complete_later = FALSE,
    answer_auto = "value2"
  )
  expect_identical(result2, "value2")
})

test_that(".init_prompt_ind with option_default", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with option_default provided
  result <- .init_prompt_ind(
    nm_item_long = "test item",
    option_default = "default_value",
    answer_auto = "auto_answer"
  )
  expect_identical(result, "auto_answer")

  # Test with NULL option_default
  result2 <- .init_prompt_ind(
    nm_item_long = "test item",
    option_default = NULL,
    answer_auto = "auto_answer"
  )
  expect_identical(result2, "auto_answer")
})

test_that(".init_prompt_ind with answer_translate", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  answer_translate <- list(
    "Option1" = "opt1",
    "Option2" = "opt2"
  )

  # Test that answer_auto is returned directly in test mode
  # (translation is bypassed)
  result <- .init_prompt_ind(
    nm_item_long = "test item",
    answer_translate = answer_translate,
    answer_auto = "Option1"
  )
  expect_identical(result, "Option1")
})

test_that(".init_prompt_ind with .var set via environment variable", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with environment variable set
  Sys.setenv("PROJR_MY_VAR" = "env_value")
  withr::defer(Sys.unsetenv("PROJR_MY_VAR"))

  result <- .init_prompt_ind(
    .var = "MY_VAR",
    nm_item_long = "test item",
    answer_auto = "auto_value"
  )
  expect_identical(result, "auto_value")

  # Test without environment variable
  result2 <- .init_prompt_ind(
    .var = "NONEXISTENT_VAR",
    nm_item_long = "test item",
    answer_auto = "auto_value"
  )
  expect_identical(result2, "auto_value")
})

test_that(".init_prompt_ind_translate with complex answer_translate", {
  skip_if(.is_test_select())

  # Test with various types of values
  answer_translate <- list(
    "string" = "translated_string",
    "number" = 42,
    "logical" = TRUE,
    "null" = NULL
  )

  expect_identical(.init_prompt_ind_translate("string", answer_translate), "translated_string")
  expect_identical(.init_prompt_ind_translate("number", answer_translate), 42)
  expect_identical(.init_prompt_ind_translate("logical", answer_translate), TRUE)
  expect_null(.init_prompt_ind_translate("null", answer_translate))
})

test_that(".init_prompt_ind_translate with empty answer_translate", {
  skip_if(.is_test_select())

  # Empty list
  answer_translate <- list()
  result <- .init_prompt_ind_translate("any_value", answer_translate)
  expect_null(result)

  # NULL translate list
  result2 <- .init_prompt_ind_translate("any_value", NULL)
  expect_identical(result2, "any_value")
})

test_that(".init_prompt_yn with different question strings", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with simple question
  result1 <- .init_prompt_yn("Continue?", answer_auto = 1)
  expect_identical(result1, 1)

  # Test with long question
  result2 <- .init_prompt_yn(
    "Do you want to proceed with this very long question that has many words?",
    answer_auto = 2
  )
  expect_identical(result2, 2)

  # Test with empty question
  result3 <- .init_prompt_yn("", answer_auto = 1)
  expect_identical(result3, 1)
})

test_that(".init_prompt_ind with NULL .var", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with .var = NULL
  result <- .init_prompt_ind(
    .var = NULL,
    nm_item_long = "test item",
    answer_auto = "test_answer"
  )
  expect_identical(result, "test_answer")
})

test_that(".init_prompt_ind edge cases", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with empty string answer
  result1 <- .init_prompt_ind(
    nm_item_long = "test item",
    answer_auto = ""
  )
  expect_identical(result1, "")

  # Test with NA answer
  result2 <- .init_prompt_ind(
    nm_item_long = "test item",
    answer_auto = NA
  )
  expect_identical(result2, NA)

  # Test with character vector
  result3 <- .init_prompt_ind(
    nm_item_long = "test item",
    answer_auto = c("value1", "value2")
  )
  expect_identical(result3, c("value1", "value2"))
})

test_that(".init_prompt_ind_translate preserves data types", {
  skip_if(.is_test_select())

  # Test that translation preserves various types
  answer_translate <- list(
    "int" = 1L,
    "numeric" = 1.5,
    "char" = "text",
    "logical" = FALSE,
    "vec" = c("a", "b"),
    "list" = list(x = 1, y = 2)
  )

  expect_identical(.init_prompt_ind_translate("int", answer_translate), 1L)
  expect_identical(.init_prompt_ind_translate("numeric", answer_translate), 1.5)
  expect_identical(.init_prompt_ind_translate("char", answer_translate), "text")
  expect_identical(.init_prompt_ind_translate("logical", answer_translate), FALSE)
  expect_identical(.init_prompt_ind_translate("vec", answer_translate), c("a", "b"))
  expect_identical(.init_prompt_ind_translate("list", answer_translate), list(x = 1, y = 2))
})

test_that(".init_prompt_yn with edge case answer_auto values", {
  skip_if(.is_test_select())
  .test_set()
  withr::defer(.test_unset())

  # Test with NA
  result1 <- .init_prompt_yn("Question?", answer_auto = NA)
  expect_identical(result1, NA)

  # Test with character
  result2 <- .init_prompt_yn("Question?", answer_auto = "yes")
  expect_identical(result2, "yes")

  # Test with NULL
  result3 <- .init_prompt_yn("Question?", answer_auto = NULL)
  expect_null(result3)

  # Test with logical
  result4 <- .init_prompt_yn("Question?", answer_auto = TRUE)
  expect_identical(result4, TRUE)
})
