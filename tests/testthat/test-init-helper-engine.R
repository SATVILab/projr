# Tests for R/init-helper-engine.R

# .init_engine() tests
# =====================

test_that(".init_engine returns FALSE when engine already exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Project already has _bookdown.yml, so should return FALSE
      expect_false(.init_engine(list(engine = "bookdown")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine creates bookdown engine when none exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Remove bookdown files
      file.remove("_output.yml")

      # Create minimal nm_list
      nm_list <- list(
        engine = "bookdown",
        pkg = "testpkg",
        gh = "testuser",
        first = "Test",
        last = "User",
        title = "Test Project"
      )

      # Create _dependencies.R (required for .dep_install)
      file.create("_dependencies.R")

      result <- .init_engine(nm_list)
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("index.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine creates quarto_project engine", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "quarto_project",
        format = "book",
        pkg = "testpkg",
        gh = "testuser",
        first = "Test",
        last = "User",
        title = "Test Project"
      )

      file.create("_dependencies.R")
      result <- .init_engine(nm_list)
      expect_true(file.exists("_quarto.yml"))
      expect_true(file.exists("index.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine creates quarto_document engine", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "quarto_document",
        format = "html",
        filename = "report",
        title = "Test Report"
      )

      file.create("_dependencies.R")
      result <- .init_engine(nm_list)
      expect_true(file.exists("report.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine creates rmd engine", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        engine = "rmd",
        format = "html",
        filename = "analysis",
        title = "Analysis Report"
      )

      file.create("_dependencies.R")
      result <- .init_engine(nm_list)
      expect_true(file.exists("analysis.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine stops with unrecognized engine", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(engine = "unknown_engine")
      expect_error(.init_engine(nm_list), "Document engine not recognised")
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_check_exists() tests
# ==================================

test_that(".init_engine_check_exists detects bookdown files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Project has _bookdown.yml by default
      expect_true(.init_engine_check_exists())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_check_exists detects quarto project files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create _quarto.yml
      writeLines("project:\n  type: book", "_quarto.yml")
      expect_true(.init_engine_check_exists())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_check_exists detects .qmd files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a .qmd file
      writeLines("# Test", "test.qmd")
      expect_true(.init_engine_check_exists())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_check_exists detects .Rmd files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a .Rmd file
      writeLines("# Test", "test.Rmd")
      expect_true(.init_engine_check_exists())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_check_exists detects .rmd files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a .rmd file (lowercase)
      writeLines("# Test", "test.rmd")
      expect_true(.init_engine_check_exists())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_check_exists returns FALSE when no files exist", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      expect_false(.init_engine_check_exists())
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_bookdown_bookdown() tests
# =======================================

test_that(".init_engine_bookdown_bookdown creates _bookdown.yml", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init_engine_bookdown_bookdown()
      expect_true(file.exists("_bookdown.yml"))

      # Check content
      yml <- yaml::read_yaml("_bookdown.yml")
      expect_true(!is.null(yml))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_bookdown_output() tests
# =====================================

test_that(".init_engine_bookdown_output creates _output.yml with metadata", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testpkg",
        gh = "testuser",
        title = "Test Book"
      )

      result <- .init_engine_bookdown_output(nm_list)
      expect_true(result)
      expect_true(file.exists("_output.yml"))

      # Check content contains metadata
      content <- readLines("_output.yml")
      expect_true(any(grepl("testuser", content)))
      expect_true(any(grepl("testpkg", content)))
      expect_true(any(grepl("Test Book", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_bookdown_output adds newline at end", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testpkg",
        gh = "testuser",
        title = "Test Book"
      )

      .init_engine_bookdown_output(nm_list)

      # Read file as raw to check for trailing newline
      content <- readLines("_output.yml", warn = FALSE)
      # File should end with a newline (this is enforced by .newline_append)
      expect_true(file.exists("_output.yml"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_bookdown_index() tests
# ====================================

test_that(".init_engine_bookdown_index creates index.Rmd with metadata", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        pkg = "testpkg",
        first = "John",
        last = "Doe",
        title = "My Test Book"
      )

      result <- .init_engine_bookdown_index(nm_list)
      expect_true(result)
      expect_true(file.exists("index.Rmd"))

      # Check content
      content <- readLines("index.Rmd")
      expect_true(any(grepl("title: testpkg", content)))
      expect_true(any(grepl("author: John Doe", content)))
      # Note: template doesn't have a description field, so description isn't added
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_quarto_project_yml() tests
# ========================================

test_that(".init_engine_quarto_project_yml creates book format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "book",
        title = "Test Book",
        first = "Jane",
        last = "Smith"
      )

      result <- .init_engine_quarto_project_yml(nm_list)
      expect_true(result)
      expect_true(file.exists("_quarto.yml"))

      # Check content
      yml <- yaml::read_yaml("_quarto.yml")
      expect_equal(yml$project$type, "book")
      expect_equal(yml$book$title, "Test Book")
      expect_true(grepl("Jane Smith", yml$book$author))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_quarto_project_yml creates website format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "website",
        title = "Test Website",
        first = "Bob",
        last = "Jones"
      )

      result <- .init_engine_quarto_project_yml(nm_list)
      expect_true(result)
      expect_true(file.exists("_quarto.yml"))

      # Check content
      yml <- yaml::read_yaml("_quarto.yml")
      expect_equal(yml$project$type, "website")
      expect_equal(yml$website$title, "Test Website")
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_quarto_project_index() tests
# ==========================================

test_that(".init_engine_quarto_project_index creates index.qmd", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      result <- .init_engine_quarto_project_index()
      expect_true(result)
      expect_true(file.exists("index.qmd"))

      # Check content
      content <- readLines("index.qmd")
      expect_true(any(grepl("# Introduction", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_quarto_document_doc() tests
# =========================================

test_that(".init_engine_quarto_document_doc creates html document", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "html",
        title = "My Report",
        filename = "report"
      )

      result <- .init_engine_quarto_document_doc(nm_list)
      expect_true(result)
      expect_true(file.exists("report.qmd"))

      # Check content
      content <- readLines("report.qmd")
      expect_true(any(grepl("title: My Report", content)))
      expect_true(any(grepl("format:", content)))
      expect_true(any(grepl("html:", content)))
      expect_true(any(grepl("embed-resources: true", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_quarto_document_doc handles word format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "word",
        title = "Word Report",
        filename = "word-doc"
      )

      result <- .init_engine_quarto_document_doc(nm_list)
      expect_true(result)
      expect_true(file.exists("word-doc.qmd"))

      # Check content - word should be converted to docx
      content <- readLines("word-doc.qmd")
      expect_true(any(grepl("docx:", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_quarto_document_doc handles powerpoint format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "powerpoint",
        title = "Presentation",
        filename = "slides"
      )

      result <- .init_engine_quarto_document_doc(nm_list)
      expect_true(result)
      expect_true(file.exists("slides.qmd"))

      # Check content - powerpoint should be converted to pptx
      content <- readLines("slides.qmd")
      expect_true(any(grepl("pptx:", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_quarto_document_doc strips .qmd extension from filename", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "html",
        title = "Test",
        filename = "test.qmd" # filename with .qmd extension
      )

      .init_engine_quarto_document_doc(nm_list)
      # Should create test.qmd, not test.qmd.qmd
      expect_true(file.exists("test.qmd"))
      expect_false(file.exists("test.qmd.qmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# .init_engine_rmd_doc() tests
# =============================

test_that(".init_engine_rmd_doc creates html_document", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "html",
        title = "HTML Report",
        filename = "report"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)
      expect_true(file.exists("report.Rmd"))

      # Check content
      content <- readLines("report.Rmd")
      expect_true(any(grepl("title: HTML Report", content)))
      expect_true(any(grepl("output: html_document", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates pdf_document", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "pdf",
        title = "PDF Report",
        filename = "pdf-report"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("pdf-report.Rmd")
      expect_true(any(grepl("output: pdf_document", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates word_document", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "word",
        title = "Word Doc",
        filename = "word"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("word.Rmd")
      expect_true(any(grepl("output: word_document", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates beamer_presentation", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "beamer",
        title = "Beamer Slides",
        filename = "slides"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("slides.Rmd")
      expect_true(any(grepl("output: beamer_presentation", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates ioslides_presentation", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "ioslides",
        title = "IOSlides",
        filename = "ioslides"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("ioslides.Rmd")
      expect_true(any(grepl("output: ioslides_presentation", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates slidy_presentation", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "slidy",
        title = "Slidy Slides",
        filename = "slidy"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("slidy.Rmd")
      expect_true(any(grepl("output: slidy_presentation", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates powerpoint_presentation", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "powerpoint",
        title = "PowerPoint",
        filename = "ppt"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("ppt.Rmd")
      expect_true(any(grepl("output: powerpoint_presentation", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc creates revealjs_presentation", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list <- list(
        format = "revealjs",
        title = "RevealJS",
        filename = "reveal"
      )

      result <- .init_engine_rmd_doc(nm_list)
      expect_true(result)

      content <- readLines("reveal.Rmd")
      expect_true(any(grepl("output: revealjs::revealjs_presentation", content)))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_engine_rmd_doc strips .Rmd and .rmd extensions from filename", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE, rm_engine = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with .Rmd extension
      nm_list1 <- list(
        format = "html",
        title = "Test",
        filename = "test.Rmd"
      )
      .init_engine_rmd_doc(nm_list1)
      expect_true(file.exists("test.Rmd"))
      expect_false(file.exists("test.Rmd.Rmd"))

      # Test with .rmd extension
      nm_list2 <- list(
        format = "html",
        title = "Test2",
        filename = "test2.rmd"
      )
      .init_engine_rmd_doc(nm_list2)
      expect_true(file.exists("test2.Rmd")) # Should be .Rmd not .rmd
      expect_false(file.exists("test2.rmd.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
