test_that(".local_dir_create works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  # skips

  # setup
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_false(.run_output_check(bump_component = NULL))
      expect_false(.run_output_check(bump_component = FALSE))
      expect_false(.run_output_check(bump_component = "dev"))
      expect_true(.run_output_check(bump_component = "patch"))
      expect_true(.run_output_check(bump_component = "minor"))
      expect_true(.run_output_check(bump_component = "major"))
      expect_false(.run_output_check(output_run = FALSE))
      expect_true(.run_output_check(output_run = TRUE))
      expect_true(.run_output_check(
        output_run = TRUE, bump_component = FALSE
      ))
    }
  )
})

test_that("projr_use_data works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  # skips

  # setup
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # saving one object
      x <- "1"
      path_tmp <- projr_use_data(x)
      expect_true(fs::path_has_parent(path_tmp, "_tmp"))
      expect_true(file.exists(path_tmp))
      path_final <- projr_use_data(x, safe = FALSE)
      expect_true(fs::path_has_parent(path_final, "data/"))
      expect_true(file.exists("data/x.rda"))
      invisible(file.remove(path_tmp))
      invisible(file.remove(path_final))

      # saving multiple objects
      y <- "c"
      paths_tmp <- projr_use_data(x, y)
      expect_identical(length(paths_tmp), 2L)
      expect_true(all(file.exists(paths_tmp)))
      expect_true(all(fs::path_has_parent(path_tmp, "_tmp")))
      paths_final <- projr_use_data(x, y, safe = FALSE)
      expect_identical(length(paths_final), 2L)
      expect_true(all(file.exists(paths_final)))
      expect_true(all(fs::path_has_parent(paths_final, "data")))
    }
  )
})

test_that(".list_add_list works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  expect_true(
    "a" %in% names(
      .list_add_list(
        x = "val", nm = "a", list_base = list(b = 2)
      )
    )
  )
})

# Infix operators
test_that("%||% works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  expect_identical(NULL %||% "default", "default")
  expect_identical("value" %||% "default", "value")
  expect_identical(0 %||% "default", 0)
  expect_identical(FALSE %||% "default", FALSE)
})

test_that("%@@% works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  expect_identical(stop("error") %@@% "fallback", "fallback")
  expect_identical(5 + 3 %@@% "fallback", 8)
  # log(-1) produces NaN with warning, not an error, so it returns NaN
  suppressWarnings(expect_true(is.nan(log(-1) %@@% 0)))
})

# String/utility functions
test_that(".try_err_msg_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  err <- try(stop("Test error message"), silent = TRUE)
  msg <- .try_err_msg_get(err)
  expect_true(grepl("Test error message", msg))

  # Test with non-try-error object
  expect_error(.try_err_msg_get("not an error", require_try_error = TRUE))
  expect_null(.try_err_msg_get("not an error", require_try_error = FALSE))
})

test_that(".newline_append works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  temp_file <- tempfile()
  writeLines(c("line1", "line2"), temp_file)
  .newline_append(temp_file)
  content <- readLines(temp_file, warn = FALSE)
  expect_identical(content[length(content)], "")

  # Already has newline
  .newline_append(temp_file)
  content2 <- readLines(temp_file, warn = FALSE)
  expect_identical(length(content2), length(content))

  unlink(temp_file)
})

test_that(".dir_count_lvl works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Returns named integer vector
  expect_identical(unname(.dir_count_lvl(".")), 0L)
  expect_identical(unname(.dir_count_lvl("a")), 1L)
  expect_identical(unname(.dir_count_lvl("a/b")), 2L)
  expect_identical(unname(.dir_count_lvl("a/b/c")), 3L)
  expect_identical(unname(.dir_count_lvl("/a/b/c/")), 3L)

  # Vector input
  levels <- .dir_count_lvl(c(".", "a", "a/b"))
  expect_identical(unname(levels), c(0L, 1L, 2L))
  expect_identical(names(levels), c(".", "a", "a/b"))
})

# Environment variable functions
test_that(".env_var_set works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  .env_var_set("TEST_VAR_MISC", "test_value")
  expect_identical(Sys.getenv("TEST_VAR_MISC"), "test_value")
  Sys.unsetenv("TEST_VAR_MISC")
})

test_that(".env_var_nm_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  expect_identical(.env_var_nm_get("VAR_NAME=value"), "VAR_NAME")
  expect_identical(.env_var_nm_get("MY_VAR = some value"), "MY_VAR")
  expect_identical(.env_var_nm_get("  VAR  =  val  "), "VAR")
})

test_that(".env_var_val_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  expect_identical(.env_var_val_get("VAR=value"), "value")
  expect_identical(.env_var_val_get("VAR=value with spaces"), "value with spaces")
  # With spaces around =, the regex doesn't match (requires no spaces)
  expect_identical(.env_var_val_get("VAR = value"), "")
  expect_identical(.env_var_val_get("123VAR=value"), "")
  expect_identical(.env_var_val_get("VAR-NAME=value"), "")
})

# Dots handling functions
test_that(".dots_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # .dots_get uses as.list(...) which has quirky behavior
  # It works with unnamed args but not named args
  test_fn <- function(...) .dots_get(...)

  # Unnamed args: as.list() gets first arg only
  result <- test_fn(1, 2, 3)
  expect_identical(length(result), 1L)

  # Named args: as.list() fails and returns empty list
  result_named <- test_fn(a = 1, b = 2)
  expect_identical(length(result_named), 0L)

  # Empty dots
  result_empty <- test_fn()
  expect_identical(length(result_empty), 0L)
})

test_that(".dots_get_chr works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  test_fn <- function(...) .dots_get_chr(...)
  # Only gets first unnamed arg due to as.list(...) behavior
  result <- test_fn(1, 2, "three")
  expect_true(is.character(result))
  expect_identical(length(result), 1L)
  expect_identical(result[[1]], "1")

  result_empty <- test_fn()
  expect_identical(length(result_empty), 0L)
})

test_that(".dots_get_chr_vec works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  test_fn <- function(...) .dots_get_chr_vec(...)
  # Only gets first unnamed arg, then unlist it
  result <- test_fn(1, c(2, 3), "four")
  expect_true(is.character(result))
  expect_identical(length(result), 1L)
  expect_identical(result, "1")

  result_empty <- test_fn()
  expect_identical(length(result_empty), 0L)
})

# Path/package utility functions
test_that(".pkg_nm_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      pkg_name <- .pkg_nm_get()
      expect_true(is.character(pkg_name))
      expect_identical(length(pkg_name), 1L)
    }
  )
})

test_that(".path_rscript_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  rscript_path <- .path_rscript_get()
  expect_true(is.character(rscript_path))
  expect_true(nzchar(rscript_path))
})

# Bump component function
test_that(".bump_component_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  expect_identical(.bump_component_get(NULL), "dev")
  expect_identical(.bump_component_get(TRUE), "dev")
  expect_identical(.bump_component_get(FALSE), "dev")
  expect_identical(.bump_component_get("patch"), "patch")
  expect_identical(.bump_component_get("minor"), "minor")
  expect_identical(.bump_component_get("major"), "major")
})

# with_dir function
test_that("with_dir works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  original_dir <- getwd()
  temp_dir <- tempdir()

  result <- with_dir(temp_dir, {
    getwd()
  })

  # Normalize paths to handle /private/var vs /var on macOS
  expect_identical(normalizePath(result, winslash = "/"), normalizePath(temp_dir, winslash = "/"))
  expect_identical(getwd(), original_dir)
})

# Zip functions
test_that(".zip_file works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  temp_dir <- tempfile()
  dir.create(temp_dir)
  temp_file <- file.path(temp_dir, "test.txt")
  writeLines("test content", temp_file)

  zip_path <- .zip_file("test.txt", temp_dir, "archive")
  expect_true(file.exists(zip_path))
  expect_true(grepl("\\.zip$", zip_path))

  unlink(temp_dir, recursive = TRUE)
  unlink(dirname(zip_path), recursive = TRUE)

  # Empty file list
  result <- .zip_file(character(), temp_dir, "archive")
  expect_identical(length(result), 0L)
})

test_that(".zip_dir works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  temp_dir <- tempfile()
  dir.create(temp_dir)
  dir.create(file.path(temp_dir, "subdir"))
  writeLines("content1", file.path(temp_dir, "file1.txt"))
  writeLines("content2", file.path(temp_dir, "subdir", "file2.txt"))

  zip_path <- tempfile(fileext = ".zip")
  result <- .zip_dir(temp_dir, zip_path)

  expect_identical(result, zip_path)
  expect_true(file.exists(zip_path))

  # With exclusions
  zip_path2 <- tempfile(fileext = ".zip")
  result2 <- .zip_dir(temp_dir, zip_path2, dir_exc = "subdir")
  expect_true(file.exists(zip_path2))

  unlink(temp_dir, recursive = TRUE)
  unlink(zip_path)
  unlink(zip_path2)
})

test_that(".opt_remote_get_structure works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  structures <- .opt_remote_get_structure()
  expect_identical(structures, c("archive", "latest"))
})

test_that(".opt_remote_get_type works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  types <- .opt_remote_get_type()
  expect_true("local" %in% types)
  expect_true("github" %in% types)
})

test_that(".opt_dir_get_label works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      labels <- .opt_dir_get_label(NULL)
      expect_true(is.character(labels))
      expect_true("docs" %in% labels)
      expect_true("data" %in% labels)
    }
  )
})

test_that(".opt_cue_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  cues <- .opt_cue_get()
  expect_true("if-change" %in% cues)
  expect_true("always" %in% cues)
})

test_that(".opt_remote_strategy_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  strategies <- .opt_remote_strategy_get()
  expect_true("upload-missing" %in% strategies)
  expect_true("sync-diff" %in% strategies)
})

test_that(".opt_remote_conflict_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  conflicts <- .opt_remote_conflict_get()
  expect_true("overwrite" %in% conflicts)
  expect_true("skip" %in% conflicts)
})

test_that(".opt_remote_inspect_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  inspects <- .opt_remote_inspect_get()
  expect_true("file" %in% inspects)
  expect_true("manifest" %in% inspects)
})

test_that(".opt_remote_transfer_names_get works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  names <- .opt_remote_transfer_names_get()
  expect_true("cue" %in% names)
  expect_true("strategy" %in% names)
})

# Retry function
test_that(".try_repeat works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Success case
  counter <- 0
  result <- .try_repeat(
    fn = function(x) {
      counter <<- counter + 1
      x + 1
    },
    args = list(x = 5),
    n_try = 3,
    n_sleep = 0
  )
  expect_identical(result, 6)

  # Retry until success
  counter2 <- 0
  result2 <- .try_repeat(
    fn = function() {
      counter2 <<- counter2 + 1
      if (counter2 < 3) stop("Not yet")
      "success"
    },
    args = list(),
    n_try = 5,
    n_sleep = 0
  )
  expect_identical(result2, "success")
  expect_true(counter2 >= 3)

  # All attempts fail
  expect_error(
    .try_repeat(
      fn = function() stop("Always fail"),
      args = list(),
      n_try = 2,
      n_sleep = 0
    ),
    "All attempts failed"
  )
})

# Init content functions
test_that(".init_readme_std_contents_rmd works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- .init_readme_std_contents_rmd()
  expect_true(is.character(content))
  expect_true(length(content) > 0L)
  expect_true(any(grepl("README", content)))
  expect_true(any(grepl("projr", content)))
})

test_that(".init_readme_std_contents_md works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- .init_readme_std_contents_md()
  expect_true(is.character(content))
  expect_true(length(content) > 0L)
  expect_true(any(grepl("README", content)))
  expect_true(any(grepl("CHANGELOG", content)))
})

test_that(".init_desc_std_contents works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- .init_desc_std_contents()
  expect_true(is.character(content))
  expect_true(any(grepl("Package:", content)))
  expect_true(any(grepl("Version:", content)))
})

test_that(".init_engine_bookdown_contents_bookdown works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- .init_engine_bookdown_contents_bookdown()
  expect_true(is.character(content))
  expect_true(any(grepl("bookdown::gitbook", content)))

  # With metadata
  nm_list <- list(title = "Test Book", gh = "testuser", pkg = "testpkg")
  content_meta <- .init_engine_bookdown_contents_bookdown(nm_list)
  expect_true(any(grepl("Test Book", content_meta)))
  expect_true(any(grepl("testuser", content_meta)))
})

test_that("init_engine_bookdown_contents_output works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- init_engine_bookdown_contents_output()
  expect_true(is.character(content))
  expect_true(any(grepl("book_filename", content)))
  expect_true(any(grepl("output_dir", content)))
})

test_that("init_engine_bookdown_contents_index works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- init_engine_bookdown_contents_index()
  expect_true(is.character(content))
  expect_true(any(grepl("title:", content)))

  # With metadata
  nm_list <- list(first = "John", last = "Doe", pkg = "TestPkg")
  content_meta <- init_engine_bookdown_contents_index(nm_list)
  expect_true(any(grepl("John Doe", content_meta)))
  expect_true(any(grepl("TestPkg", content_meta)))
})

test_that(".init_engine_quarto_projects_content_yml works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  content <- .init_engine_quarto_projects_content_yml()
  expect_true(is.list(content))
  expect_true("project" %in% names(content))
  expect_true("website" %in% names(content))
  expect_identical(content$project$type, "website")
})

# Usethis helper functions
test_that(".usethis_get_objs_from_dots works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dots <- alist(x, y, z)
  objs <- .usethis_get_objs_from_dots(dots)
  expect_identical(objs, c("x", "y", "z"))

  # Duplicates
  dots_dup <- alist(a, b, a)
  objs_dup <- .usethis_get_objs_from_dots(dots_dup)
  expect_identical(length(objs_dup), 2L)
  expect_true("a" %in% objs_dup)
  expect_true("b" %in% objs_dup)

  # Empty
  expect_error(.usethis_get_objs_from_dots(list()), "Nothing to save")

  # Non-names
  expect_error(.usethis_get_objs_from_dots(list(1, 2)), "Can only save existing named objects")
})

test_that(".usethis_dots works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  test_fn <- function(...) .usethis_dots(...)
  result <- test_fn(x, y, z)
  expect_true(is.list(result))
  expect_identical(length(result), 3L)
})

test_that(".usethis_proj_desc works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      desc_obj <- .usethis_proj_desc()
      expect_true(inherits(desc_obj, "description"))
    }
  )
})
