# Tests for metadata.R functions
# ================================

test_that(".metadata_get_author_host works with git", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Should return a character string
      author <- .metadata_get_author_host()
      expect_true(.is_chr(author))
      expect_true(length(author) == 1)
    }
  )
})

test_that(".metadata_get_author_host works without git", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Should fall back to non-git method
      author <- .metadata_get_author_host()
      expect_true(.is_chr(author))
      expect_true(length(author) == 1)
    }
  )
})

test_that(".metadata_get_author_host_non_git works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should return a character string from environment or system
  author <- .metadata_get_author_host_non_git()
  expect_true(.is_chr(author))
  expect_true(length(author) == 1)
})

test_that(".metadata_get_author_host_env works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Ensure environment variable is set so function returns a non-empty value
  os <- .metadata_get_os()
  if (identical(os, "Windows")) {
    withr::local_envvar(USERNAME = "projr-test-user")
  } else if (identical(os, "Linux") || identical(os, "Darwin")) {
    withr::local_envvar(USER = "projr-test-user")
  }

  # Should return username from environment variable
  author <- .metadata_get_author_host_env()
  expect_true(.is_chr(author))
  expect_true(length(author) == 1)

  # Should match system environment
  expected <- switch(os,
    "Windows" = Sys.getenv("USERNAME"),
    "Linux" = Sys.getenv("USER"),
    "Darwin" = Sys.getenv("USER"),
    NULL
  )
  if (!is.null(expected)) {
    expect_identical(author, expected)
  }
})

test_that(".metadata_get_author_sys_info works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Should return a character string
  author <- .metadata_get_author_sys_info()
  expect_true(.is_chr(author))
  expect_true(length(author) == 1)

  # Test fallback logic: tries user, then login, then HOSTNAME
  user_info <- Sys.info()[["user"]]
  login_info <- Sys.info()[["login"]]

  if (.is_len_1(user_info) && .is_string(user_info)) {
    expect_identical(author, user_info)
  } else if (.is_len_1(login_info) && !identical(login_info, "unknown") && .is_string(login_info)) {
    expect_identical(author, login_info)
  } else {
    # Should fall back to HOSTNAME or "anonymous-user"
    expect_true(nchar(author) > 0)
  }
})

test_that(".metadata_get_os works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  os <- .metadata_get_os()
  expect_true(.is_chr(os))
  expect_true(length(os) == 1)
  expect_identical(os, Sys.info()[["sysname"]])

  # Should be one of the common OS names
  expect_true(os %in% c("Windows", "Linux", "Darwin", "SunOS", "FreeBSD"))
})

test_that(".metadata_get_host works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  host <- .metadata_get_host()
  expect_true(.is_chr(host))
  expect_true(length(host) == 1)
  expect_identical(host, Sys.info()[["nodename"]])
})

test_that(".metadata_get_time works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  time_str <- .metadata_get_time()
  expect_true(.is_chr(time_str))
  expect_true(length(time_str) == 1)

  # Should match HH:MM:SS format
  expect_true(grepl("^\\d{2}:\\d{2}:\\d{2}$", time_str))

  # Parse and check it's a valid time
  time_parts <- strsplit(time_str, ":")[[1]]
  expect_identical(length(time_parts), 3L)
  hour <- as.integer(time_parts[1])
  minute <- as.integer(time_parts[2])
  second <- as.integer(time_parts[3])
  expect_true(hour >= 0 && hour <= 23)
  expect_true(minute >= 0 && minute <= 59)
  expect_true(second >= 0 && second <= 59)
})

test_that(".metadata_get_date works", {
  skip_if(.is_test_select())

  date_str <- .metadata_get_date()
  expect_true(.is_chr(date_str))
  expect_true(length(date_str) == 1)

  # Should match YYYY-MM-DD format
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str))

  # Parse and check it's a valid date
  date_parts <- strsplit(date_str, "-")[[1]]
  expect_identical(length(date_parts), 3L)
  year <- as.integer(date_parts[1])
  month <- as.integer(date_parts[2])
  day <- as.integer(date_parts[3])
  expect_true(year >= 1970 && year <= 2100)
  expect_true(month >= 1 && month <= 12)
  expect_true(day >= 1 && day <= 31)
})


# Tests for yml-metadata.R functions
# ===================================

test_that(".yml_metadata_get_nm works", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with non-existent key
      result <- .yml_metadata_get_nm("nonexistent", "default")
      expect_null(result)

      # Test with version-format (common metadata key)
      version_format <- .yml_metadata_get_nm("version-format", "default")
      # Should be NULL or a string
      expect_true(is.null(version_format) || .is_string(version_format))
    }
  )
})

test_that(".yml_metadata_set_nm works with non-empty value", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Set a metadata value
      .yml_metadata_set_nm("test-value", "test-key", "default")

      # Read it back
      result <- .yml_metadata_get_nm("test-key", "default")
      expect_identical(result, "test-value")
    }
  )
})

test_that(".yml_metadata_set_nm works with empty value", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # First set a value
      .yml_metadata_set_nm("test-value", "test-key", "default")
      result <- .yml_metadata_get_nm("test-key", "default")
      expect_identical(result, "test-value")

      # Now set it to NULL (should remove it)
      .yml_metadata_set_nm(NULL, "test-key", "default")
      result <- .yml_metadata_get_nm("test-key", "default")
      expect_null(result)
    }
  )
})

test_that(".yml_metadata_get works", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Should return a list (empty or with metadata)
      metadata <- .yml_metadata_get("default")
      expect_true(is.list(metadata))
    }
  )
})

test_that(".yml_metadata_set works", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Set entire metadata section
      new_metadata <- list(
        "key1" = "value1",
        "key2" = "value2"
      )
      .yml_metadata_set(new_metadata, "default")

      # Read it back
      metadata <- .yml_metadata_get("default")
      expect_identical(metadata[["key1"]], "value1")
      expect_identical(metadata[["key2"]], "value2")

      # Can also retrieve individual keys
      key1 <- .yml_metadata_get_nm("key1", "default")
      expect_identical(key1, "value1")
    }
  )
})
