# Tests for internal helper functions in R/ignore.R
# These functions parse and manipulate .gitignore and .Rbuildignore files

# ==============================================================================
# .ignore_path_read() tests
# ==============================================================================

test_that(".ignore_path_read returns file contents when file exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a test file with content
      test_content <- c("line1", "line2", "line3")
      writeLines(test_content, ".gitignore")

      result <- .ignore_path_read(".gitignore")

      expect_identical(result, test_content)
      expect_type(result, "character")
      expect_length(result, 3)
    }
  )
})

test_that(".ignore_path_read returns empty character vector for non-existent file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure file doesn't exist
      if (file.exists("nonexistent.txt")) unlink("nonexistent.txt")

      result <- .ignore_path_read("nonexistent.txt")

      expect_identical(result, character(0))
      expect_length(result, 0)
    }
  )
})

test_that(".ignore_path_read suppresses warnings", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a file without final newline (may cause warning)
      cat("no newline", file = ".gitignore")

      # Should not produce warnings
      expect_no_warning(result <- .ignore_path_read(".gitignore"))
      expect_type(result, "character")
    }
  )
})

# ==============================================================================
# .ignore_get_list_empty() tests
# ==============================================================================

test_that(".ignore_get_list_empty returns correct empty list structure", {
  skip_if(.is_test_select())

  result <- .ignore_get_list_empty()

  expect_type(result, "list")
  expect_named(result, c("start", "content", "end"))
  expect_identical(result$start, character(0))
  expect_identical(result$content, character(0))
  expect_identical(result$end, character(0))
})

# ==============================================================================
# .ignore_path_get_check() tests
# ==============================================================================

test_that(".ignore_path_get_check errors when multiple start markers", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "content",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      )

      expect_error(
        .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore"),
        "Multiple projr sections found"
      )
    }
  )
})

test_that(".ignore_path_get_check errors when multiple end markers", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section",
        "content",
        "# End of projr section"
      )

      expect_error(
        .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore"),
        "Multiple projr sections found"
      )
    }
  )
})

test_that(".ignore_path_get_check errors when only start marker present", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "content"
      )

      expect_error(
        .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore"),
        "Found start of projr section but not end"
      )
    }
  )
})

test_that(".ignore_path_get_check errors when only end marker present", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "content",
        "# End of projr section"
      )

      expect_error(
        .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore"),
        "Found end of projr section but not start"
      )
    }
  )
})

test_that(".ignore_path_get_check errors when start marker after end marker", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# End of projr section",
        "content",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())"
      )

      expect_error(
        .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore"),
        "Start of projr section found after end"
      )
    }
  )
})

test_that(".ignore_path_get_check succeeds with both markers in correct order", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "content",
        "# End of projr section"
      )

      expect_no_error(
        result <- .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore")
      )
      expect_true(result)
    }
  )
})

test_that(".ignore_path_get_check succeeds when no markers present", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c("manual content", "more content")

      expect_no_error(
        result <- .ignore_path_get_check(match_str_top, match_str_bottom, file_vec, ".gitignore")
      )
      expect_true(result)
    }
  )
})

# ==============================================================================
# .ignore_path_get_ind() tests
# ==============================================================================

test_that(".ignore_path_get_ind finds both markers", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "manual content",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "auto content",
        "# End of projr section",
        "more manual"
      )

      result <- .ignore_path_get_ind(file_vec, ".gitignore")

      expect_named(result, c("top", "bot"))
      expect_identical(result[["top"]], 2L)
      expect_identical(result[["bot"]], 4L)
    }
  )
})

test_that(".ignore_path_get_ind returns NA when no markers present", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c("manual content", "more content")

      result <- .ignore_path_get_ind(file_vec, ".gitignore")

      expect_named(result, c("top", "bot"))
      expect_true(is.na(result[["top"]]))
      expect_true(is.na(result[["bot"]]))
    }
  )
})

test_that(".ignore_path_get_ind handles markers at start and end of file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "content",
        "# End of projr section"
      )

      result <- .ignore_path_get_ind(file_vec, ".gitignore")

      expect_identical(result[["top"]], 1L)
      expect_identical(result[["bot"]], 3L)
    }
  )
})

# ==============================================================================
# .ignore_path_get_startend() tests
# ==============================================================================

test_that(".ignore_path_get_startend creates new markers when none exist", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c("existing content", "more content")

      result <- .ignore_path_get_startend(NA_integer_, NA_integer_, file_vec)

      expect_type(result, "list")
      expect_named(result, c("start", "end"))

      # Start should include existing content plus start marker
      expect_true(any(grepl("existing content", result$start)))
      expect_true(any(grepl("Start of projr section", result$start)))

      # End should be the end marker
      expect_identical(result$end, "# End of projr section")
    }
  )
})

test_that(".ignore_path_get_startend extracts content with existing markers", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "before1",
        "before2",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "auto content",
        "# End of projr section",
        "after1",
        "after2"
      )

      result <- .ignore_path_get_startend(3L, 5L, file_vec)

      # Start should be lines 1-3 (including start marker)
      expect_length(result$start, 3)
      expect_identical(result$start[1], "before1")
      expect_identical(result$start[2], "before2")
      expect_true(grepl("Start of projr section", result$start[3]))

      # End should be lines 5-7 (from end marker to end)
      expect_length(result$end, 3)
      expect_true(grepl("End of projr section", result$end[1]))
      expect_identical(result$end[2], "after1")
      expect_identical(result$end[3], "after2")
    }
  )
})

test_that(".ignore_path_get_startend handles markers at file boundaries", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Markers at very start and end
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "content",
        "# End of projr section"
      )

      result <- .ignore_path_get_startend(1L, 3L, file_vec)

      expect_length(result$start, 1)
      expect_length(result$end, 1)
    }
  )
})

# ==============================================================================
# .ignore_path_get_content() tests
# ==============================================================================

test_that(".ignore_path_get_content extracts content between markers", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "before",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "auto1",
        "auto2",
        "auto3",
        "# End of projr section",
        "after"
      )

      result <- .ignore_path_get_content(2L, 6L, file_vec)

      expect_identical(result, c("auto1", "auto2", "auto3"))
      expect_length(result, 3)
    }
  )
})

test_that(".ignore_path_get_content returns empty when no markers", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c("content1", "content2")

      result <- .ignore_path_get_content(NA_integer_, NA_integer_, file_vec)

      expect_identical(result, character(0))
      expect_length(result, 0)
    }
  )
})

test_that(".ignore_path_get_content returns empty when markers are adjacent", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "before",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section",
        "after"
      )

      result <- .ignore_path_get_content(2L, 3L, file_vec)

      expect_identical(result, character(0))
      expect_length(result, 0)
    }
  )
})

test_that(".ignore_path_get_content handles single line of content", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      file_vec <- c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "single_line",
        "# End of projr section"
      )

      result <- .ignore_path_get_content(1L, 3L, file_vec)

      expect_identical(result, "single_line")
      expect_length(result, 1)
    }
  )
})

# ==============================================================================
# .ignore_path_get_list() tests
# ==============================================================================

test_that(".ignore_path_get_list parses file with projr section correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file with projr section
      file_content <- c(
        "manual1",
        "manual2",
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "auto1",
        "auto2",
        "# End of projr section",
        "manual3"
      )
      writeLines(file_content, ".gitignore")

      result <- .ignore_path_get_list(".gitignore", ignore = FALSE, override = FALSE)

      expect_type(result, "list")
      expect_named(result, c("start", "content", "end"))

      # Start should include manual content and start marker
      expect_true(any(grepl("manual1", result$start)))
      expect_true(any(grepl("Start of projr section", result$start)))

      # Content should be the auto content
      expect_true("auto1" %in% result$content)
      expect_true("auto2" %in% result$content)

      # End should include end marker and remaining manual content
      expect_true(any(grepl("End of projr section", result$end)))
      expect_true("manual3" %in% result$end)
    }
  )
})

test_that(".ignore_path_get_list returns empty list for non-existent file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      if (file.exists("nonexistent.txt")) unlink("nonexistent.txt")

      result <- .ignore_path_get_list("nonexistent.txt", ignore = FALSE, override = FALSE)

      expect_type(result, "list")
      expect_named(result, c("start", "content", "end"))
      expect_identical(result$start, character(0))
      expect_identical(result$content, character(0))
      expect_identical(result$end, character(0))
    }
  )
})

test_that(".ignore_path_get_list parses file without projr section", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file without projr section
      file_content <- c("manual1", "manual2", "manual3")
      writeLines(file_content, ".gitignore")

      result <- .ignore_path_get_list(".gitignore", ignore = FALSE, override = FALSE)

      expect_type(result, "list")
      expect_named(result, c("start", "content", "end"))

      # When no projr section exists, start should include all existing content plus new marker
      expect_true(any(grepl("manual1", result$start)))
      expect_true(any(grepl("Start of projr section", result$start)))

      # Content should be empty (no existing projr section)
      expect_identical(result$content, character(0))

      # End should be the end marker
      expect_identical(result$end, "# End of projr section")
    }
  )
})

test_that(".ignore_path_get_list handles empty file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create empty file
      writeLines("", ".gitignore")

      result <- .ignore_path_get_list(".gitignore", ignore = FALSE, override = FALSE)

      expect_type(result, "list")
      expect_named(result, c("start", "content", "end"))

      # Should create new markers for empty file
      expect_true(any(grepl("Start of projr section", result$start)))
      expect_identical(result$end, "# End of projr section")
    }
  )
})

# ==============================================================================
# .ignore_path_write() tests
# ==============================================================================

test_that(".ignore_path_write writes content to file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      test_content <- c("line1", "line2", "line3")

      result <- .ignore_path_write(test_content, ".gitignore")

      expect_true(file.exists(".gitignore"))
      written_content <- readLines(".gitignore")
      # .newline_append adds empty string at end
      expect_identical(written_content, c(test_content, ""))
      expect_identical(result, ".gitignore")
    }
  )
})

test_that(".ignore_path_write appends newline at end of file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      test_content <- c("line1", "line2")

      .ignore_path_write(test_content, ".gitignore")

      # Read raw content to check for trailing newline
      raw_content <- readChar(".gitignore", file.info(".gitignore")$size)

      # File should end with newline
      expect_true(grepl("\n$", raw_content))
    }
  )
})

test_that(".ignore_path_write creates file if it doesn't exist", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      if (file.exists("newfile.txt")) unlink("newfile.txt")

      expect_false(file.exists("newfile.txt"))

      .ignore_path_write(c("content"), "newfile.txt")

      expect_true(file.exists("newfile.txt"))
      # .newline_append adds empty string at end
      expect_identical(readLines("newfile.txt"), c("content", ""))
    }
  )
})

test_that(".ignore_path_write overwrites existing file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create file with initial content
      writeLines(c("old1", "old2"), ".gitignore")

      # Overwrite with new content
      .ignore_path_write(c("new1", "new2", "new3"), ".gitignore")

      result <- readLines(".gitignore")
      # .newline_append adds empty string at end
      expect_identical(result, c("new1", "new2", "new3", ""))
      expect_false(any(grepl("old", result)))
    }
  )
})

test_that(".ignore_path_write handles empty character vector", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .ignore_path_write(character(0), ".gitignore")

      expect_true(file.exists(".gitignore"))
      result <- readLines(".gitignore")
      # .newline_append adds empty string even for empty input
      expect_identical(result, "")
    }
  )
})
