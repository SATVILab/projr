# Tests for R/yml-quarto.R
# =============================================================================

# Phase 1: Core Functions
# -----------------------------------------------------------------------------

test_that(".yml_quarto_get returns empty list when file doesn't exist", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure no _quarto.yml exists
      if (file.exists("_quarto.yml")) {
        file.remove("_quarto.yml")
      }

      result <- .yml_quarto_get()
      expect_identical(result, list())
    }
  )
})

test_that(".yml_quarto_get returns parsed YAML when file exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a simple _quarto.yml
      yml_content <- list(
        project = list(type = "website", `output-dir` = "_site"),
        website = list(title = "Test Site")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get()
      expect_true(is.list(result))
      expect_equal(result$project$type, "website")
      expect_equal(result$project$`output-dir`, "_site")
      expect_equal(result$website$title, "Test Site")
    }
  )
})

test_that(".yml_quarto_set writes YAML file correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure no _quarto.yml exists
      if (file.exists("_quarto.yml")) {
        file.remove("_quarto.yml")
      }

      yml_data <- list(
        project = list(type = "website"),
        website = list(title = "My Site")
      )

      result <- .yml_quarto_set(yml_data)
      expect_true(result)
      expect_true(file.exists("_quarto.yml"))

      # Read back and verify
      yml_read <- yaml::read_yaml("_quarto.yml")
      expect_equal(yml_read$project$type, "website")
      expect_equal(yml_read$website$title, "My Site")
    }
  )
})

test_that(".yml_quarto_set wraps strings in lists for project.render", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        project = list(render = "index.qmd")
      )

      .yml_quarto_set(yml_data)

      # Check the YAML text itself to verify it was written as an array
      yml_text <- readLines("_quarto.yml", warn = FALSE)
      # Should have array syntax: "  - index.qmd" or "  render:" followed by "  - index.qmd"
      render_line_idx <- grep("render:", yml_text)
      expect_true(length(render_line_idx) > 0)
      # Check if next line has array syntax
      if (length(render_line_idx) > 0) {
        next_line <- yml_text[render_line_idx[1] + 1]
        expect_true(grepl("^\\s*-", next_line))
      }
    }
  )
})

test_that(".yml_quarto_set wraps strings in lists for website keys", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        website = list(
          `other-links` = "link1",
          `code-links` = "link2",
          `repo-actions` = "action1",
          `reader-mode` = "mode1"
        )
      )

      .yml_quarto_set(yml_data)

      # Check the YAML text to verify array syntax
      yml_text <- readLines("_quarto.yml", warn = FALSE)
      # Each key should be followed by a line with array syntax (-)
      expect_true(any(grepl("other-links:", yml_text)))
      expect_true(any(grepl("code-links:", yml_text)))
      expect_true(any(grepl("repo-actions:", yml_text)))
      expect_true(any(grepl("reader-mode:", yml_text)))

      # Check that values are written as arrays (with - prefix)
      # Count lines with array syntax in the website section
      website_start <- grep("^website:", yml_text)[1]
      # Find next top-level key or end
      next_section <- grep("^[a-z]", yml_text)
      next_section <- next_section[next_section > website_start]
      website_end <- if (length(next_section) > 0) next_section[1] - 1 else length(yml_text)
      website_lines <- yml_text[website_start:website_end]
      array_lines <- grep("^\\s*-", website_lines)
      # Should have at least 4 array items
      expect_true(length(array_lines) >= 4)
    }
  )
})

test_that(".yml_quarto_set wraps strings in lists for navbar keys", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        website = list(
          navbar = list(
            right = "item1",
            left = "item2",
            tools = "tool1",
            pinned = "pin1"
          )
        )
      )

      .yml_quarto_set(yml_data)

      # Check the YAML text for array syntax
      yml_text <- readLines("_quarto.yml", warn = FALSE)
      expect_true(any(grepl("navbar:", yml_text)))
      expect_true(any(grepl("right:", yml_text)))
      expect_true(any(grepl("left:", yml_text)))
      expect_true(any(grepl("tools:", yml_text)))
      expect_true(any(grepl("pinned:", yml_text)))

      # Should have array syntax (4 array items for the navbar keys)
      navbar_start <- grep("navbar:", yml_text)[1]
      next_section <- grep("^[a-z]|^  [a-z]", yml_text)
      next_section <- next_section[next_section > navbar_start]
      # Find end of navbar section (next sibling key at same level)
      navbar_end <- if (length(next_section) > 0) {
        # Find the next line at same indent level as navbar
        same_level <- grep("^  [a-z]", yml_text)
        same_level <- same_level[same_level > navbar_start]
        if (length(same_level) > 0) same_level[1] - 1 else length(yml_text)
      } else {
        length(yml_text)
      }
      navbar_lines <- yml_text[navbar_start:navbar_end]
      array_lines <- grep("^\\s*-", navbar_lines)
      expect_true(length(array_lines) >= 4)
    }
  )
})

test_that(".yml_quarto_set wraps strings in lists for sidebar keys", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        website = list(
          sidebar = "sidebar1"
        )
      )

      .yml_quarto_set(yml_data)

      # Check the YAML text for array syntax
      yml_text <- readLines("_quarto.yml", warn = FALSE)
      sidebar_line_idx <- grep("sidebar:", yml_text)
      expect_true(length(sidebar_line_idx) > 0)
      # Check if next line has array syntax
      if (length(sidebar_line_idx) > 0) {
        next_line <- yml_text[sidebar_line_idx[1] + 1]
        expect_true(grepl("^\\s*-", next_line))
      }
    }
  )
})

test_that(".yml_quarto_set wraps strings in lists for book keys", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        book = list(
          chapters = "chapter1.qmd",
          appendices = "appendix1.qmd"
        )
      )

      .yml_quarto_set(yml_data)

      # Check the YAML text for array syntax
      yml_text <- readLines("_quarto.yml", warn = FALSE)
      expect_true(any(grepl("chapters:", yml_text)))
      expect_true(any(grepl("appendices:", yml_text)))

      # Should have array syntax for both
      book_start <- grep("^book:", yml_text)[1]
      next_section <- grep("^[a-z]", yml_text)
      next_section <- next_section[next_section > book_start]
      book_end <- if (length(next_section) > 0) next_section[1] - 1 else length(yml_text)
      book_lines <- yml_text[book_start:book_end]
      array_lines <- grep("^\\s*-", book_lines)
      expect_true(length(array_lines) >= 2)
    }
  )
})

test_that(".yml_quarto_set handles logical values correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        project = list(
          type = "website",
          preview = TRUE
        ),
        website = list(
          search = FALSE
        )
      )

      .yml_quarto_set(yml_data)

      # Read the file as text to check lowercase true/false
      yml_text <- readLines("_quarto.yml", warn = FALSE)
      expect_true(any(grepl("true", yml_text)))
      expect_true(any(grepl("false", yml_text)))
      expect_false(any(grepl("TRUE", yml_text)))
      expect_false(any(grepl("FALSE", yml_text)))
    }
  )
})

test_that(".yml_quarto_set appends newline to file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(project = list(type = "website"))

      .yml_quarto_set(yml_data)

      # Read file with readBin to check for trailing newline
      file_content <- readBin("_quarto.yml", "raw", n = file.info("_quarto.yml")$size)
      # Last character should be newline (0x0a)
      expect_equal(file_content[length(file_content)], as.raw(0x0a))
    }
  )
})

# Phase 2: Output Directory Functions
# -----------------------------------------------------------------------------

test_that(".yml_quarto_get_output_dir returns NULL when not set", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml without output-dir
      yml_content <- list(
        project = list(type = "website")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_output_dir()
      expect_null(result)
    }
  )
})

test_that(".yml_quarto_get_output_dir returns value when set", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml with output-dir
      yml_content <- list(
        project = list(
          type = "website",
          `output-dir` = "_site"
        )
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_output_dir()
      expect_equal(result, "_site")
    }
  )
})

test_that(".yml_quarto_set_output_dir creates project section when absent", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml without project section
      writeLines("website:\n  title: Test", "_quarto.yml")

      .yml_quarto_set_output_dir("_output")

      yml_lines <- readLines("_quarto.yml", warn = FALSE)
      expect_true(any(grepl("^project:", yml_lines)))
      expect_true(any(grepl("output-dir: _output", yml_lines)))
    }
  )
})

test_that(".yml_quarto_set_output_dir updates existing output-dir", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml with existing output-dir
      writeLines(
        c(
          "project:",
          "  type: website",
          "  output-dir: _site"
        ),
        "_quarto.yml"
      )

      .yml_quarto_set_output_dir("_output")

      yml_lines <- readLines("_quarto.yml", warn = FALSE)
      expect_true(any(grepl("output-dir: _output", yml_lines)))
      expect_false(any(grepl("output-dir: _site", yml_lines)))
    }
  )
})

test_that(".yml_quarto_set_output_dir adds output-dir when project exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml with project but no output-dir
      writeLines(
        c(
          "project:",
          "  type: website"
        ),
        "_quarto.yml"
      )

      .yml_quarto_set_output_dir("_output")

      yml_lines <- readLines("_quarto.yml", warn = FALSE)
      expect_true(any(grepl("output-dir: _output", yml_lines)))
    }
  )
})

# Phase 3: Project Functions
# -----------------------------------------------------------------------------

test_that(".yml_quarto_get_project_type returns NULL when not set", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml without project type
      yml_content <- list(
        project = list(`output-dir` = "_site")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_project_type()
      expect_null(result)
    }
  )
})

test_that(".yml_quarto_get_project_type returns type when set", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test website type
      yml_content <- list(
        project = list(type = "website")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_project_type()
      expect_equal(result, "website")

      # Test book type
      yml_content <- list(
        project = list(type = "book")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_project_type()
      expect_equal(result, "book")
    }
  )
})

test_that(".yml_quarto_get_project returns NULL when no project section", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml without project section
      yml_content <- list(
        website = list(title = "Test")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_project()
      expect_null(result)
    }
  )
})

test_that(".yml_quarto_get_project returns project configuration", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_content <- list(
        project = list(
          type = "website",
          `output-dir` = "_site"
        )
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_project()
      expect_true(is.list(result))
      expect_equal(result$type, "website")
      expect_equal(result$`output-dir`, "_site")
    }
  )
})

test_that(".yml_quarto_get_nm returns NULL for non-existent key", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_content <- list(
        project = list(type = "website")
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      result <- .yml_quarto_get_nm("nonexistent")
      expect_null(result)
    }
  )
})

test_that(".yml_quarto_get_nm returns value for existing key", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_content <- list(
        project = list(type = "website"),
        website = list(title = "My Site"),
        format = list(html = list(toc = TRUE))
      )
      yaml::write_yaml(yml_content, "_quarto.yml")

      # Test different top-level keys
      result_project <- .yml_quarto_get_nm("project")
      expect_true(is.list(result_project))
      expect_equal(result_project$type, "website")

      result_website <- .yml_quarto_get_nm("website")
      expect_true(is.list(result_website))
      expect_equal(result_website$title, "My Site")

      result_format <- .yml_quarto_get_nm("format")
      expect_true(is.list(result_format))
      expect_true(is.list(result_format$html))
    }
  )
})

# Phase 4: Helper Functions
# -----------------------------------------------------------------------------

test_that(".wrap_in_list_if_string does not change lists", {
  skip_if(.is_test_select())

  lst <- list(
    project = list(
      render = list("file1.qmd", "file2.qmd")
    )
  )

  result <- .wrap_in_list_if_string(lst, c("project", "render"))
  expect_true(is.list(result$project$render))
  expect_equal(length(result$project$render), 2)
  expect_equal(result$project$render[[1]], "file1.qmd")
})

test_that(".wrap_in_list_if_string wraps strings in list", {
  skip_if(.is_test_select())

  lst <- list(
    project = list(
      render = "file1.qmd"
    )
  )

  result <- .wrap_in_list_if_string(lst, c("project", "render"))
  expect_true(is.list(result$project$render))
  expect_equal(length(result$project$render), 1)
  expect_equal(result$project$render[[1]], "file1.qmd")
})

test_that(".wrap_in_list_if_string handles NULL values", {
  skip_if(.is_test_select())

  lst <- list(
    project = list(
      render = NULL
    )
  )

  # Should not error, just return unchanged
  result <- .wrap_in_list_if_string(lst, c("project", "render"))
  expect_null(result$project$render)
})

test_that(".wrap_in_list_if_string handles non-existent paths", {
  skip_if(.is_test_select())

  lst <- list(
    project = list(type = "website")
  )

  # Should not error when path doesn't exist
  result <- .wrap_in_list_if_string(lst, c("project", "render"))
  expect_true(is.list(result$project))
  expect_null(result$project$render)
})

test_that(".get_nested_value returns NULL for non-existent paths", {
  skip_if(.is_test_select())

  lst <- list(
    a = list(
      b = list(c = "value")
    )
  )

  # Non-existent key at first level
  result <- .get_nested_value(lst, c("x"))
  expect_null(result)

  # Non-existent key at second level
  result <- .get_nested_value(lst, c("a", "x"))
  expect_null(result)

  # Non-existent key at third level
  result <- .get_nested_value(lst, c("a", "b", "x"))
  expect_null(result)
})

test_that(".get_nested_value returns value for valid paths", {
  skip_if(.is_test_select())

  lst <- list(
    a = list(
      b = list(
        c = "value"
      )
    )
  )

  # Single level
  result <- .get_nested_value(lst, c("a"))
  expect_true(is.list(result))
  expect_true("b" %in% names(result))

  # Two levels
  result <- .get_nested_value(lst, c("a", "b"))
  expect_true(is.list(result))
  expect_equal(result$c, "value")

  # Three levels
  result <- .get_nested_value(lst, c("a", "b", "c"))
  expect_equal(result, "value")
})

test_that(".get_nested_value handles non-list intermediate values", {
  skip_if(.is_test_select())

  lst <- list(
    a = "not_a_list"
  )

  # Should return NULL when intermediate value is not a list
  result <- .get_nested_value(lst, c("a", "b"))
  expect_null(result)
})

test_that(".set_nested_value sets value at single-level path", {
  skip_if(.is_test_select())

  lst <- list(a = 1)

  result <- .set_nested_value(lst, c("b"), "new_value")
  expect_equal(result$a, 1)
  expect_equal(result$b, "new_value")

  # Overwrite existing
  result <- .set_nested_value(lst, c("a"), "updated")
  expect_equal(result$a, "updated")
})

test_that(".set_nested_value sets value at multi-level path", {
  skip_if(.is_test_select())

  lst <- list(
    a = list(
      b = list(c = "old")
    )
  )

  # Update existing nested value
  result <- .set_nested_value(lst, c("a", "b", "c"), "new")
  expect_equal(result$a$b$c, "new")

  # Add new nested value
  result <- .set_nested_value(lst, c("a", "b", "d"), "added")
  expect_equal(result$a$b$d, "added")
})

test_that(".set_nested_value creates intermediate lists as needed", {
  skip_if(.is_test_select())

  lst <- list()

  # Create nested structure from empty list
  result <- .set_nested_value(lst, c("a", "b", "c"), "value")
  expect_equal(result$a$b$c, "value")

  # Partial structure exists
  lst2 <- list(a = list())
  result2 <- .set_nested_value(lst2, c("a", "b", "c"), "value")
  expect_equal(result2$a$b$c, "value")
})

# Phase 5: Internal Functions (set_output_dir helpers)
# -----------------------------------------------------------------------------

test_that(".yml_quarto_get_project_block_indices finds project block", {
  skip_if(.is_test_select())

  yml_lines <- c(
    "project:",
    "  type: website",
    "  output-dir: _site",
    "website:",
    "  title: Test"
  )

  indices <- .yml_quarto_get_project_block_indices(yml_lines, 1)
  expect_equal(indices$start, 1)
  expect_equal(indices$end, 3)
})

test_that(".yml_quarto_get_project_block_indices handles project at end", {
  skip_if(.is_test_select())

  yml_lines <- c(
    "website:",
    "  title: Test",
    "project:",
    "  type: website"
  )

  indices <- .yml_quarto_get_project_block_indices(yml_lines, 3)
  expect_equal(indices$start, 3)
  expect_equal(indices$end, 4)
})

test_that(".yml_quarto_update_project_block replaces existing output-dir", {
  skip_if(.is_test_select())

  project_block <- c(
    "project:",
    "  type: website",
    "  output-dir: _site"
  )

  result <- .yml_quarto_update_project_block(project_block, "_output")
  expect_true(any(grepl("output-dir: _output", result)))
  expect_false(any(grepl("output-dir: _site", result)))
})

test_that(".yml_quarto_update_project_block adds output-dir when missing", {
  skip_if(.is_test_select())

  project_block <- c(
    "project:",
    "  type: website"
  )

  result <- .yml_quarto_update_project_block(project_block, "_output")
  expect_true(any(grepl("output-dir: _output", result)))
  expect_equal(length(result), 3)
})

test_that(".yml_quarto_rebuild_yaml rebuilds correctly", {
  skip_if(.is_test_select())

  yml_lines <- c(
    "format:",
    "  html: default",
    "project:",
    "  type: website",
    "  output-dir: _site",
    "website:",
    "  title: Test"
  )

  updated_block <- c(
    "project:",
    "  type: website",
    "  output-dir: _output"
  )

  block_indices <- list(start = 3, end = 5)

  result <- .yml_quarto_rebuild_yaml(yml_lines, updated_block, block_indices)

  # Should have: 2 lines before + 3 updated + 2 lines after = 7 lines
  expect_equal(length(result), 7)
  expect_equal(result[1:2], yml_lines[1:2])
  expect_equal(result[3:5], updated_block)
  expect_equal(result[6:7], yml_lines[6:7])
})

test_that(".yml_quarto_rebuild_yaml handles project at start", {
  skip_if(.is_test_select())

  yml_lines <- c(
    "project:",
    "  type: website",
    "website:",
    "  title: Test"
  )

  updated_block <- c(
    "project:",
    "  type: book"
  )

  block_indices <- list(start = 1, end = 2)

  result <- .yml_quarto_rebuild_yaml(yml_lines, updated_block, block_indices)

  expect_equal(result[1:2], updated_block)
  expect_equal(result[3:4], yml_lines[3:4])
})

test_that(".yml_quarto_rebuild_yaml handles project at end", {
  skip_if(.is_test_select())

  yml_lines <- c(
    "website:",
    "  title: Test",
    "project:",
    "  type: website"
  )

  updated_block <- c(
    "project:",
    "  type: book"
  )

  block_indices <- list(start = 3, end = 4)

  result <- .yml_quarto_rebuild_yaml(yml_lines, updated_block, block_indices)

  expect_equal(result[1:2], yml_lines[1:2])
  expect_equal(result[3:4], updated_block)
})

# Phase 6: Integration Tests
# -----------------------------------------------------------------------------

test_that("complete workflow: create, modify, read Quarto YAML", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # 1. Create initial _quarto.yml
      initial_yml <- list(
        project = list(
          type = "website",
          `output-dir` = "_site"
        ),
        website = list(
          title = "Initial Title",
          navbar = list(
            left = list("index.qmd")
          )
        )
      )

      .yml_quarto_set(initial_yml)
      expect_true(file.exists("_quarto.yml"))

      # 2. Read and verify
      read_yml <- .yml_quarto_get()
      expect_equal(read_yml$project$type, "website")
      expect_equal(read_yml$website$title, "Initial Title")

      # 3. Modify output directory
      .yml_quarto_set_output_dir("_output")
      expect_equal(.yml_quarto_get_output_dir(), "_output")

      # 4. Modify with .yml_quarto_set
      modified_yml <- .yml_quarto_get()
      modified_yml$website$title <- "Updated Title"
      .yml_quarto_set(modified_yml)

      # 5. Verify modifications
      final_yml <- .yml_quarto_get()
      expect_equal(final_yml$project$`output-dir`, "_output")
      expect_equal(final_yml$website$title, "Updated Title")
    }
  )
})

test_that("yml_quarto handles complex nested structures", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      complex_yml <- list(
        project = list(
          type = "website",
          render = list("*.qmd"),
          `output-dir` = "_site"
        ),
        website = list(
          title = "Complex Site",
          navbar = list(
            left = list(
              list(text = "Home", href = "index.qmd"),
              list(text = "About", href = "about.qmd")
            ),
            right = list(
              list(icon = "github", href = "https://github.com")
            ),
            tools = list(
              list(icon = "twitter", href = "https://twitter.com")
            )
          ),
          sidebar = list(
            list(
              title = "Section 1",
              contents = list("page1.qmd", "page2.qmd")
            )
          )
        ),
        format = list(
          html = list(
            theme = "cosmo",
            toc = TRUE,
            `code-fold` = TRUE
          )
        )
      )

      .yml_quarto_set(complex_yml)
      read_back <- .yml_quarto_get()

      expect_equal(read_back$project$type, "website")
      expect_true(is.list(read_back$website$navbar$left))
      expect_true(is.list(read_back$website$sidebar))
      expect_equal(read_back$format$html$theme, "cosmo")
    }
  )
})

test_that("yml_quarto handles edge case: empty _quarto.yml", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create empty _quarto.yml
      writeLines("", "_quarto.yml")

      result <- .yml_quarto_get()
      # yaml::read_yaml returns NULL for empty files
      expect_true(is.null(result) || length(result) == 0)
    }
  )
})

test_that("yml_quarto preserves non-wrapped values", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_data <- list(
        project = list(
          type = "website",
          `output-dir` = "_site"
        ),
        format = list(
          html = list(
            theme = "cosmo",
            toc = TRUE
          )
        )
      )

      .yml_quarto_set(yml_data)
      read_back <- .yml_quarto_get()

      # These should NOT be wrapped in lists
      expect_equal(read_back$project$type, "website")
      expect_equal(read_back$project$`output-dir`, "_site")
      expect_equal(read_back$format$html$theme, "cosmo")
      expect_true(read_back$format$html$toc)
    }
  )
})
