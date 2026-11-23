test_that("GitHub API URL follows explicit, environment and default precedence", {
  skip_if(.is_test_select())
  withr::local_envvar(GITHUB_API_URL = NA_character_)
  expect_identical(.github_api_base(), "https://api.github.com")
  withr::local_envvar(GITHUB_API_URL = "https://enterprise.invalid/api/v3///")
  expect_identical(.github_api_base(), "https://enterprise.invalid/api/v3")
  expect_identical(
    .github_api_base("https://explicit.invalid/"),
    "https://explicit.invalid"
  )
})

test_that("GitHub remote URLs resolve to owner/repository", {
  skip_if(.is_test_select())
  urls <- c(
    "https://github.com/owner/repo.git",
    "https://github.com/owner/repo",
    "git@github.com:owner/repo.git",
    "git@github.com:owner/repo",
    "https://www.github.com/owner/repo",
    "https://github.enterprise.com/owner/repo",
    "ssh://git@github.com/owner/repo.git"
  )
  for (url in urls) {
    expect_identical(.gh_repo_from_remote_url(url), "owner/repo")
  }
  expect_error(.gh_repo_from_remote_url("invalid"), "parse owner/repo")
})

test_that("GitHub release queries distinguish HTTP errors and missing assets", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  repo <- "fixture/project"
  tag <- "candidate/test"
  base <- "https://fixture.invalid"
  url <- paste0(base, "/repos/", repo, "/releases/tags/candidate%2Ftest")
  status <- 200L
  body <- list(assets = list())
  previous <- httr::set_callback("request", function(req) {
    stopifnot(
      req$method == "GET",
      req$url == url,
      req$headers[["Authorization"]] == "token fixture-token"
    )
    .test_github_response(status, body)
  })
  withr::defer(httr::set_callback("request", previous))
  exists <- function() {
    .remote_check_exists_github_httr(repo, tag, base, "fixture-token")
  }
  info <- function() {
    .remote_get_info_github_httr(repo, tag, base, "fixture-token")
  }
  asset <- function() {
    .remote_final_get_info_github_httr(
      repo,
      tag,
      "wanted.zip",
      base,
      "fixture-token"
    )
  }

  expect_true(exists())
  expect_error(asset(), "No assets found")
  body <- list(assets = list(list(name = "other.zip")))
  expect_error(asset(), "Asset 'wanted.zip' not found", fixed = TRUE)
  status <- 404L
  expect_false(exists())
  expect_error(info(), "Release not found")
  for (status in c(401L, 403L)) {
    expect_error(
      exists(),
      paste0("authentication error \\(status ", status, "\\)")
    )
  }
  status <- 503L
  expect_error(
    exists(),
    "Unexpected HTTP status from GitHub API: 503",
    fixed = TRUE
  )
  expect_error(info(), "Failed to get release: HTTP 503", fixed = TRUE)
})

test_that("GitHub release creation preserves server validation errors", {
  skip_if(.is_test_select())
  skip_if_not_installed("httr")
  status <- 201L
  previous <- httr::set_callback("request", function(req) {
    stopifnot(
      req$method == "POST",
      req$url == "https://fixture.invalid/repos/fixture/project/releases"
    )
    body <- jsonlite::fromJSON(rawToChar(req$options$postfields))
    expect_identical(body$tag_name, "candidate")
    .test_github_response(
      status,
      if (status == 201L) list(id = 12L) else list(message = "already_exists")
    )
  })
  withr::defer(httr::set_callback("request", previous))
  create <- function() {
    .gh_release_create_httr(
      "fixture/project",
      "candidate",
      "test",
      "https://fixture.invalid",
      "fixture-token"
    )
  }
  expect_identical(create()$id, 12L)
  status <- 422L
  expect_error(create(), "HTTP 422 - already_exists")
})
