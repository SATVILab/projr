# Tests for restore function validation and edge cases

# Tests for projr_restore parameter validation
test_that("projr_restore validates label parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal manifest.csv
      .init()
      dir.create("raw-data", showWarnings = FALSE)
      writeLines("label,fn,version,hash\nraw-data,test.txt,v0.0.1,abc123", "manifest.csv")
      
      # Valid inputs
      expect_error(projr_restore(label = NULL), NA)  # NULL is valid
      expect_error(projr_restore(label = "raw-data"), NA)  # character vector is valid
      
      # Invalid inputs
      expect_error(
        projr_restore(label = 123),
        "'label' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(label = TRUE),
        "'label' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(label = character(0)),
        "'label' must have at least one element if not NULL"
      )
      expect_error(
        projr_restore(label = list("raw-data")),
        "'label' must be NULL or a character vector"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_restore validates pos parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal manifest.csv
      .init()
      dir.create("raw-data", showWarnings = FALSE)
      writeLines("label,fn,version,hash\nraw-data,test.txt,v0.0.1,abc123", "manifest.csv")
      
      # Valid inputs
      expect_error(projr_restore(pos = NULL), NA)  # NULL is valid
      expect_error(projr_restore(pos = "source"), NA)  # valid value
      expect_error(projr_restore(pos = "dest"), NA)  # valid value
      expect_error(projr_restore(pos = c("source", "dest")), NA)  # both valid
      
      # Invalid inputs
      expect_error(
        projr_restore(pos = 123),
        "'pos' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(pos = TRUE),
        "'pos' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(pos = character(0)),
        "'pos' must have at least one element if not NULL"
      )
      expect_error(
        projr_restore(pos = "invalid"),
        "'pos' must be 'source' or 'dest'"
      )
      expect_error(
        projr_restore(pos = c("source", "invalid")),
        "'pos' must be 'source' or 'dest'"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_restore validates type parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal manifest.csv
      .init()
      dir.create("raw-data", showWarnings = FALSE)
      writeLines("label,fn,version,hash\nraw-data,test.txt,v0.0.1,abc123", "manifest.csv")
      
      # Valid inputs
      expect_error(projr_restore(type = NULL), NA)  # NULL is valid
      expect_error(projr_restore(type = "local"), NA)  # valid value
      expect_error(projr_restore(type = "osf"), NA)  # valid value
      expect_error(projr_restore(type = "github"), NA)  # valid value
      
      # Invalid inputs
      expect_error(
        projr_restore(type = 123),
        "'type' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(type = TRUE),
        "'type' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(type = character(0)),
        "'type' must have at least one element if not NULL"
      )
      expect_error(
        projr_restore(type = "invalid"),
        "'type' must be one of: local, osf, github"
      )
      expect_error(
        projr_restore(type = c("local", "osf")),
        "'type' must be a single character value"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_restore validates title parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal manifest.csv
      .init()
      dir.create("raw-data", showWarnings = FALSE)
      writeLines("label,fn,version,hash\nraw-data,test.txt,v0.0.1,abc123", "manifest.csv")
      
      # Valid inputs
      expect_error(projr_restore(title = NULL), NA)  # NULL is valid
      expect_error(projr_restore(title = "mytitle"), NA)  # character is valid
      
      # Invalid inputs
      expect_error(
        projr_restore(title = 123),
        "'title' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(title = TRUE),
        "'title' must be NULL or a character vector"
      )
      expect_error(
        projr_restore(title = character(0)),
        "'title' must have at least one element if not NULL"
      )
      expect_error(
        projr_restore(title = c("title1", "title2")),
        "'title' must be a single character value"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Tests for projr_restore edge cases
test_that("projr_restore handles missing manifest.csv", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Don't create manifest.csv
      expect_error(
        projr_restore(),
        "No manifest.csv file found"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_restore handles empty label vector", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Create empty manifest
      writeLines("label,fn,version,hash", "manifest.csv")
      
      # Should return FALSE with message (or no raw labels found)
      result <- projr_restore()
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_restore returns correct success/failure values", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      dir.create("raw-data", showWarnings = FALSE)
      
      # Create manifest with empty files (nothing to restore)
      writeLines("label,fn,version,hash\nraw-data,,v0.0.1,", "manifest.csv")
      
      # Should return FALSE when nothing to restore
      result <- projr_restore(label = "raw-data")
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# Tests for projr_restore_repo parameter validation
test_that("projr_restore_repo validates repo parameter", {
  skip_if(.is_test_select())
  
  # Valid input (will fail at git clone but should pass validation)
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo")),
    regexp = NA,
    class = "validation_error"
  )
  
  # Invalid inputs
  expect_error(
    projr_restore_repo(repo = NULL),
    "'repo' cannot be NULL"
  )
  expect_error(
    projr_restore_repo(repo = 123),
    "'repo' must be a character string"
  )
  expect_error(
    projr_restore_repo(repo = TRUE),
    "'repo' must be a character string"
  )
  expect_error(
    projr_restore_repo(repo = character(0)),
    "'repo' must have at least one element"
  )
  expect_error(
    projr_restore_repo(repo = c("repo1", "repo2")),
    "'repo' must be a single character value"
  )
  expect_error(
    projr_restore_repo(repo = ""),
    "'repo' cannot be an empty string"
  )
})

test_that("projr_restore_repo validates path parameter", {
  skip_if(.is_test_select())
  
  # Valid inputs
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", path = NULL)),
    regexp = NA,
    class = "validation_error"
  )
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", path = ".")),
    regexp = NA,
    class = "validation_error"
  )
  
  # Invalid inputs
  expect_error(
    projr_restore_repo(repo = "owner/repo", path = 123),
    "'path' must be NULL or a character string"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", path = TRUE),
    "'path' must be NULL or a character string"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", path = c("path1", "path2")),
    "'path' must be a single character value"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", path = ""),
    "'path' cannot be an empty string"
  )
})

test_that("projr_restore_repo validates label parameter", {
  skip_if(.is_test_select())
  
  # Valid inputs
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", label = NULL)),
    regexp = NA,
    class = "validation_error"
  )
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", label = "raw-data")),
    regexp = NA,
    class = "validation_error"
  )
  
  # Invalid inputs
  expect_error(
    projr_restore_repo(repo = "owner/repo", label = 123),
    "'label' must be NULL or a character vector"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", label = character(0)),
    "'label' must have at least one element if not NULL"
  )
})

test_that("projr_restore_repo validates pos parameter", {
  skip_if(.is_test_select())
  
  # Valid inputs
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", pos = NULL)),
    regexp = NA,
    class = "validation_error"
  )
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", pos = "source")),
    regexp = NA,
    class = "validation_error"
  )
  
  # Invalid inputs
  expect_error(
    projr_restore_repo(repo = "owner/repo", pos = "invalid"),
    "'pos' must be 'source' or 'dest'"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", pos = character(0)),
    "'pos' must have at least one element if not NULL"
  )
})

test_that("projr_restore_repo validates type parameter", {
  skip_if(.is_test_select())
  
  # Valid inputs
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", type = NULL)),
    regexp = NA,
    class = "validation_error"
  )
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", type = "local")),
    regexp = NA,
    class = "validation_error"
  )
  
  # Invalid inputs
  expect_error(
    projr_restore_repo(repo = "owner/repo", type = "invalid"),
    "'type' must be one of: local, osf, github"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", type = c("local", "osf")),
    "'type' must be a single character value"
  )
})

test_that("projr_restore_repo validates title parameter", {
  skip_if(.is_test_select())
  
  # Valid inputs
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", title = NULL)),
    regexp = NA,
    class = "validation_error"
  )
  expect_error(
    suppressWarnings(projr_restore_repo(repo = "owner/repo", title = "mytitle")),
    regexp = NA,
    class = "validation_error"
  )
  
  # Invalid inputs
  expect_error(
    projr_restore_repo(repo = "owner/repo", title = 123),
    "'title' must be NULL or a character vector"
  )
  expect_error(
    projr_restore_repo(repo = "owner/repo", title = c("title1", "title2")),
    "'title' must be a single character value"
  )
})

# Tests for projr_restore_repo_wd parameter validation
test_that("projr_restore_repo_wd accepts all parameters", {
  skip_if(.is_test_select())
  
  # Should accept all parameters that projr_restore_repo accepts
  expect_error(
    suppressWarnings(projr_restore_repo_wd(
      repo = "owner/repo",
      label = NULL,
      pos = NULL,
      type = NULL,
      title = NULL
    )),
    regexp = NA,
    class = "validation_error"
  )
  
  # Should validate parameters same as projr_restore_repo
  expect_error(
    projr_restore_repo_wd(repo = NULL),
    "'repo' cannot be NULL"
  )
  expect_error(
    projr_restore_repo_wd(repo = "owner/repo", label = 123),
    "'label' must be NULL or a character vector"
  )
})

test_that("projr_restore_repo returns success/failure correctly", {
  skip_if(.is_test_select())
  
  # When git clone fails, should return FALSE
  expect_message(
    result <- suppressWarnings(projr_restore_repo(repo = "nonexistent/repo")),
    regexp = "Error in projr_restore_repo"
  )
  # The function should complete and return a value
  expect_true(is.logical(result))
  expect_false(result)
})
