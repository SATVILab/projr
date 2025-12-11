# Tests for R/ignore-auto.R
# Focus on increasing test coverage for internal helper functions

# ==============================================================================
# Core entry point tests
# ==============================================================================

test_that(".ignore_auto works with archive_local parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with archive_local = FALSE (default)
      result <- .ignore_auto(archive_local = FALSE)
      expect_true(result)

      # Check that .gitignore and .Rbuildignore were created
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))

      # Test with archive_local = TRUE
      result <- .ignore_auto(archive_local = TRUE)
      expect_true(result)
    }
  )
})

# ==============================================================================
# Quarto-specific functions
# ==============================================================================

test_that(".ignore_auto_quarto_rbuild ignores _extensions directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create _extensions directory
      dir.create("_extensions")

      # Run function
      result <- .ignore_auto_quarto_rbuild()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("extensions", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_quarto_rbuild returns FALSE when no directories exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove any quarto directories if they exist
      if (dir.exists("_extensions")) unlink("_extensions", recursive = TRUE)
      if (dir.exists("index_files")) unlink("index_files", recursive = TRUE)
      if (dir.exists(".quarto")) unlink(".quarto", recursive = TRUE)

      # Run function
      result <- .ignore_auto_quarto_rbuild()
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_quarto_rbuild handles index_files directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create index_files directory
      dir.create("index_files")

      result <- .ignore_auto_quarto_rbuild()
      expect_true(result)

      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("index_files", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_quarto_rbuild handles .quarto directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .quarto directory
      dir.create(".quarto")

      result <- .ignore_auto_quarto_rbuild()
      expect_true(result)

      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("quarto", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_quarto_git ignores index_files and .quarto", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create directories
      dir.create("index_files")
      dir.create(".quarto")

      result <- .ignore_auto_quarto_git()
      expect_true(result)

      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("index_files", gitignore)))
      expect_true(any(grepl("\\.quarto", gitignore)))
    }
  )
})

test_that(".ignore_auto_quarto_git returns FALSE when no directories exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove directories
      if (dir.exists("index_files")) unlink("index_files", recursive = TRUE)
      if (dir.exists(".quarto")) unlink(".quarto", recursive = TRUE)

      result <- .ignore_auto_quarto_git()
      expect_false(result)
    }
  )
})

# ==============================================================================
# Directory detection functions
# ==============================================================================

test_that(".ignore_auto_devcontainer ignores .devcontainer directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .devcontainer directory
      dir.create(".devcontainer")

      result <- .ignore_auto_devcontainer()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("devcontainer", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_devcontainer returns FALSE when directory does not exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove directory if it exists
      if (dir.exists(".devcontainer")) unlink(".devcontainer", recursive = TRUE)

      result <- .ignore_auto_devcontainer()
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_github ignores .github directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .github directory
      dir.create(".github")

      result <- .ignore_auto_github()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("github", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_github returns FALSE when directory does not exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove directory if it exists
      if (dir.exists(".github")) unlink(".github", recursive = TRUE)

      result <- .ignore_auto_github()
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_vscode ignores .vscode directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .vscode directory
      dir.create(".vscode")

      # Function returns invisibly, need to capture it properly
      .ignore_auto_vscode()

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("vscode", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_vscode ignores .code-workspace files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .code-workspace file
      file.create("project.code-workspace")

      result <- .ignore_auto_vscode()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("code-workspace", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_vscode handles .code-workspace files without .vscode dir", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove .vscode directory
      if (dir.exists(".vscode")) unlink(".vscode", recursive = TRUE)

      # Create .code-workspace file
      file.create("test.code-workspace")

      # Should process without error
      .ignore_auto_vscode()

      # Check that .code-workspace pattern is in rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("code-workspace", rbuildignore, ignore.case = TRUE)))
    }
  )
})

# ==============================================================================
# Config file pattern functions
# ==============================================================================

test_that(".ignore_auto_yml ignores _projr.yml variants", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create various projr yml files
      file.create("_projr.yml")
      file.create("_projr-custom.yml")
      file.create("_quarto.yml")
      file.create("_bookdown.yml")

      # Function returns NULL/invisible, just run it
      .ignore_auto_yml()

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("projr", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_yml ignores _projr-local.yml in git", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create _projr-local.yml
      file.create("_projr-local.yml")

      result <- .ignore_auto_yml()
      expect_true(result)

      # Check .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_projr-local.yml", gitignore)))
    }
  )
})

test_that(".ignore_auto_env ignores environment files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create environment files
      file.create("_environment")
      file.create("_environment-dev")
      file.create("_environment.required")
      file.create("_environment.local")

      result <- .ignore_auto_env()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("environment", rbuildignore, ignore.case = TRUE)))

      # Check that _environment.local is in .gitignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_environment.local", gitignore)))
    }
  )
})

# ==============================================================================
# Build source functions
# ==============================================================================

test_that(".ignore_auto_build_source ignores .qmd files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .qmd files
      file.create("document.qmd")
      file.create("analysis.qmd")

      result <- .ignore_auto_build_source()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("document", rbuildignore, ignore.case = TRUE)))
    }
  )
})

test_that(".ignore_auto_build_source ignores .Rmd files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .Rmd files
      file.create("document.Rmd")
      file.create("analysis.rmd")

      result <- .ignore_auto_build_source()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("document", rbuildignore, ignore.case = TRUE)))
    }
  )
})

# ==============================================================================
# Extension filtering
# ==============================================================================

test_that(".ignore_auto_ext_rbuild ignores common file extensions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create files with various extensions
      file.create("document.pdf")
      file.create("data.csv")
      file.create("output.html")
      file.create("script.R")

      result <- .ignore_auto_ext_rbuild()
      expect_true(result)

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      # These should be in the ignore patterns
      expect_true(length(rbuildignore) > 0)
    }
  )
})

test_that(".ignore_auto_ext_rbuild does not ignore README.md", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create README.md and other .md files
      file.create("README.md")
      file.create("notes.md")

      result <- .ignore_auto_ext_rbuild()
      expect_true(result)

      # README.md should be excluded from ignore patterns
      rbuildignore <- readLines(".Rbuildignore")
      # README.md should not be in the list (it's explicitly excluded)
    }
  )
})

test_that(".ignore_auto_ext_git ignores build artifacts but preserves important files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create various files
      file.create("build.log")
      file.create("output.pdf")
      file.create("CHANGELOG.md")
      file.create("manifest.csv")
      file.create("LICENSE.md")

      result <- .ignore_auto_ext_git()
      expect_true(result)

      # Check .gitignore
      gitignore <- readLines(".gitignore")
      # build.log should be ignored
      expect_true(any(grepl("build.log", gitignore)))
      # Important files should not be ignored (they're excluded)
    }
  )
})

# ==============================================================================
# TeX file handling
# ==============================================================================

test_that(".ignore_auto_build_tex_bookdown ignores bookdown tex files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set engine to bookdown
      yml_content <- "
build:
  engine: bookdown
"
      writeLines(yml_content, "_projr.yml")

      # Create _bookdown.yml with book_filename
      bd_content <- "book_filename: 'mybook'"
      writeLines(bd_content, "_bookdown.yml")

      # Create the tex file
      file.create("mybook.tex")

      result <- .ignore_auto_build_tex_bookdown()
      expect_true(result)

      # Check that tex file is ignored
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("mybook.tex", gitignore)))
    }
  )
})

test_that(".ignore_auto_build_tex_bookdown uses _main.tex as default", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set engine to bookdown without specifying book_filename
      yml_content <- "
build:
  engine: bookdown
"
      writeLines(yml_content, "_projr.yml")

      # Create _bookdown.yml without book_filename (will default to _main)
      writeLines("", "_bookdown.yml")

      # Create default tex file
      file.create("_main.tex")

      result <- .ignore_auto_build_tex_bookdown()
      expect_true(result)

      # Check that _main.tex is ignored
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_main.tex", gitignore)))
    }
  )
})

test_that(".ignore_auto_build_tex_bookdown returns FALSE for non-bookdown engine", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set engine to something other than bookdown
      yml_content <- "
build:
  engine: quarto_document
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_auto_build_tex_bookdown()
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_build_tex_quarto ignores index.tex for quarto_project", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Need to set up _quarto.yml for quarto_project detection
      quarto_yml <- "
project:
  type: book
"
      writeLines(quarto_yml, "_quarto.yml")

      # Create index.tex
      file.create("index.tex")

      result <- .ignore_auto_build_tex_quarto()
      expect_true(result)

      # Check that index.tex is ignored
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("index.tex", gitignore)))
    }
  )
})

test_that(".ignore_auto_build_tex_quarto returns FALSE without index.tex", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set engine to quarto_project but don't create index.tex
      yml_content <- "
build:
  engine: quarto_project
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_auto_build_tex_quarto()
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_build_tex_rqmd ignores corresponding .tex files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .qmd and .Rmd files
      file.create("document.qmd")
      file.create("report.Rmd")

      # Create corresponding .tex files
      file.create("document.tex")
      file.create("report.tex")

      result <- .ignore_auto_build_tex_rqmd()
      expect_true(result)

      # Check that .tex files are ignored
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("document.tex", gitignore)))
      expect_true(any(grepl("report.tex", gitignore)))
    }
  )
})

test_that(".ignore_auto_build_tex_rqmd returns FALSE without source files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # No .qmd or .Rmd files
      result <- .ignore_auto_build_tex_rqmd()
      expect_false(result)
    }
  )
})

# ==============================================================================
# Build content directory handling
# ==============================================================================

test_that(".ignore_auto_build_content_dir handles quarto_document engine", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set engine to quarto_document
      yml_content <- "
build:
  engine: quarto_document
"
      writeLines(yml_content, "_projr.yml")

      # Create a .qmd file
      file.create("analysis.qmd")

      result <- .ignore_auto_build_content_dir()
      expect_null(result)
    }
  )
})

test_that(".ignore_auto_build_content_dir returns NULL for bookdown", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Set engine to bookdown
      yml_content <- "
build:
  engine: bookdown
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_auto_build_content_dir()
      expect_null(result)
    }
  )
})

# ==============================================================================
# Local destination handling
# ==============================================================================

test_that(".ignore_auto_dest_local handles archive_local parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal YAML without local archive destination
      yml_content <- "
directories:
  output:
    path: _output
"
      writeLines(yml_content, "_projr.yml")

      # Test with archive_local = TRUE - should add _archive
      .ignore_auto_dest_local(archive_local = TRUE)

      # Check that _archive is ignored
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_archive", gitignore)))
    }
  )
})

test_that(".ignore_auto_dest_local handles local destinations from YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create YAML with local destination
      yml_content <- "
build:
  local:
    my-dest:
      path: _local_dest
      ignore: git
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_auto_dest_local(archive_local = FALSE)
      expect_true(result)

      # Check that custom path is ignored in git
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_local_dest", gitignore)))
    }
  )
})

test_that(".ignore_auto_dest_local_title handles different ignore settings", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test ignore = "git"
      yml_dest <- list(path = "_git_only", ignore = "git")
      .ignore_auto_dest_local_title(yml_dest)

      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_git_only", gitignore)))

      # Test ignore = "rbuild"
      yml_dest2 <- list(path = "_rbuild_only", ignore = "rbuild")
      .ignore_auto_dest_local_title(yml_dest2)

      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("_rbuild_only", rbuildignore)))

      # Test ignore = NULL (both)
      yml_dest3 <- list(path = "_both", ignore = NULL)
      .ignore_auto_dest_local_title(yml_dest3)

      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("_both", gitignore)))
    }
  )
})

test_that(".ignore_auto_dest_local returns FALSE when no local destinations", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create YAML without local destinations
      yml_content <- "
build:
  github:
    id: test-release
"
      writeLines(yml_content, "_projr.yml")

      result <- .ignore_auto_dest_local(archive_local = FALSE)
      expect_false(result)
    }
  )
})

# ==============================================================================
# Core internal helpers
# ==============================================================================

test_that(".ignore_auto_file ignores files in both git and rbuild", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_file(c("test.log", "output.pdf"))
      expect_true(result)

      # Check both .gitignore and .Rbuildignore
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("test.log", gitignore)))

      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("test", rbuildignore)))
    }
  )
})

test_that(".ignore_auto_file_git handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_file_git(character(0))
      expect_false(result)

      result <- .ignore_auto_file_git(c("", ""))
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_file_git handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_file_git(123)
      expect_false(result)

      result <- .ignore_auto_file_git(NULL)
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_dir ignores directories in both git and rbuild", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_dir(c("temp_dir", "cache_dir"))
      expect_true(result)

      # Check .gitignore (should have /** appended)
      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("temp_dir", gitignore)))
      expect_true(any(grepl("/\\*\\*", gitignore)))

      # Check .Rbuildignore
      rbuildignore <- readLines(".Rbuildignore")
      expect_true(any(grepl("temp_dir", rbuildignore)))
    }
  )
})

test_that(".ignore_auto_dir_git appends /** to directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_dir_git(c("mydir", "anotherdir"))
      expect_true(result)

      gitignore <- readLines(".gitignore")
      # Should have /** appended
      expect_true(any(grepl("mydir/\\*\\*", gitignore)))
      expect_true(any(grepl("anotherdir/\\*\\*", gitignore)))
    }
  )
})

test_that(".ignore_auto_dir_git handles paths already containing /**", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_dir_git(c("mydir/**", "normal_dir"))
      expect_true(result)

      gitignore <- readLines(".gitignore")
      # mydir/** should not become mydir/**/**
      expect_true(any(grepl("mydir/\\*\\*", gitignore)))
      expect_false(any(grepl("mydir/\\*\\*/\\*\\*", gitignore)))
    }
  )
})

test_that(".ignore_auto_dir_git handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_dir_git(character(0))
      expect_false(result)

      result <- .ignore_auto_dir_git(c("", ""))
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_file_rbuild converts paths to regex", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_file_rbuild(c("test.txt", "data.csv"))
      expect_true(result)

      rbuildignore <- readLines(".Rbuildignore")
      # glob2rx converts to regex patterns
      expect_true(length(rbuildignore) > 0)
    }
  )
})

test_that(".ignore_auto_file_rbuild removes trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_file_rbuild(c("file.txt///", "data.csv/"))
      expect_true(result)

      rbuildignore <- readLines(".Rbuildignore")
      # Should process correctly after removing slashes
      expect_true(length(rbuildignore) > 0)
    }
  )
})

test_that(".ignore_auto_dir_rbuild creates directory-specific patterns", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_dir_rbuild(c("mydir", "anotherdir"))
      expect_true(result)

      rbuildignore <- readLines(".Rbuildignore")
      # Should have patterns for directories
      expect_true(any(grepl("mydir", rbuildignore)))
      expect_true(any(grepl("anotherdir", rbuildignore)))
    }
  )
})

test_that(".ignore_auto_dir_rbuild handles trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_dir_rbuild(c("dir1///", "dir2/"))
      expect_true(result)

      rbuildignore <- readLines(".Rbuildignore")
      # Should handle correctly after removing slashes
      expect_true(any(grepl("dir1", rbuildignore)))
      expect_true(any(grepl("dir2", rbuildignore)))
    }
  )
})

test_that(".ignore_auto_path_add adds patterns to projr section", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create .gitignore with projr section
      writeLines(c(
        "# Start of projr section: do not edit by hand (update with projr_ignore_auto())",
        "# End of projr section"
      ), ".gitignore")

      result <- .ignore_auto_path_add(c("pattern1", "pattern2"), ".gitignore")
      expect_true(result)

      gitignore <- readLines(".gitignore")
      expect_true(any(grepl("pattern1", gitignore)))
      expect_true(any(grepl("pattern2", gitignore)))
    }
  )
})

test_that(".ignore_auto_path_add handles empty input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_path_add(character(0), ".gitignore")
      expect_false(result)

      result <- .ignore_auto_path_add(c("", ""), ".gitignore")
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_path_add handles non-character input", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .ignore_auto_path_add(123, ".gitignore")
      expect_false(result)

      result <- .ignore_auto_path_add(NULL, ".gitignore")
      expect_false(result)
    }
  )
})

test_that(".ignore_auto_path_get_updated_content merges patterns", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      existing_content <- c("existing1", "existing2")
      new_patterns <- c("new1", "new2", "existing1")

      # Test override = FALSE (merge)
      result <- .ignore_auto_path_get_updated_content(
        FALSE, new_patterns, existing_content
      )

      # Should have all unique patterns
      expect_true("existing1" %in% result)
      expect_true("existing2" %in% result)
      expect_true("new1" %in% result)
      expect_true("new2" %in% result)
      # Should be unique
      expect_equal(length(result), length(unique(result)))
    }
  )
})

test_that(".ignore_auto_path_get_updated_content overrides when specified", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      existing_content <- c("existing1", "existing2")
      new_patterns <- c("new1", "new2")

      # Test override = TRUE
      result <- .ignore_auto_path_get_updated_content(
        TRUE, new_patterns, existing_content
      )

      # Should only have new patterns
      expect_true("new1" %in% result)
      expect_true("new2" %in% result)
      expect_false("existing1" %in% result)
      expect_false("existing2" %in% result)
    }
  )
})
