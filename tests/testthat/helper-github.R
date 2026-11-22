# Live GitHub tests require an explicit opt-in before credential or network access.
.test_can_modify_github <- function() {
  identical(Sys.getenv("PROJR_TEST_LIVE_GITHUB"), "true")
}

.test_skip_if_cannot_modify_github <- function() {
  if (!.test_can_modify_github()) {
    testthat::skip("Live GitHub tests disabled (PROJR_TEST_LIVE_GITHUB=true to opt in)")
  }
  invisible(TRUE)
}

# Repository lifecycle tests need an explicitly managed fixture. Never create or
# delete a repository automatically, even when the live lane is enabled.
.test_github_repo_create <- function(...) {
  testthat::skip("Automatic GitHub repository creation is disabled; use a managed fixture")
}

.test_github_response <- function(status, body = list()) {
  structure(list(
    status_code = as.integer(status),
    headers = list("Content-Type" = "application/json"),
    content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  ), class = "response")
}

# Each fixture has its own releases and one asset slot per release. Intercept
# httr before transport so unknown routes fail without opening a connection.
.test_local_github_releases <- function(tags, env = parent.frame()) {
  api <- "https://fixture.invalid"
  repo <- "fixture/project"
  withr::local_envvar(c(GITHUB_API_URL = api, GITHUB_PAT = "fixture-token"), .local_envir = env)
  testthat::local_mocked_bindings(.gh_repo_get = function() repo, .package = "projr", .env = env)
  files <- file.path(withr::local_tempdir(.local_envir = env), seq_along(tags))
  asset_names <- rep(NA_character_, length(tags))
  releases <- paste0(api, "/repos/", repo, "/releases")
  release_urls <- paste0(releases, "/", seq_along(tags))
  upload_urls <- paste0(release_urls, "/assets")
  asset_urls <- paste0(releases, "/assets/", 100L + seq_along(tags))

  handle <- function(req) {
    url <- sub("[?].*$", "", req$url)
    index <- match(url, paste0(releases, "/tags/", tags))
    if (req$method == "GET" && !is.na(index)) {
      assets <- if (file.exists(files[index])) {
        list(list(id = 100L + index, name = asset_names[index], url = asset_urls[index]))
      } else {
        list()
      }
      return(.test_github_response(200, list(
        id = index, assets = assets, upload_url = paste0(upload_urls[index], "{?name,label}")
      )))
    }
    if (req$method == "GET" && startsWith(url, paste0(releases, "/tags/"))) {
      return(.test_github_response(404))
    }
    index <- match(url, upload_urls)
    if (req$method == "POST" && !is.na(index)) {
      asset_names[index] <<- httr::parse_url(req$url)$query$name
      bytes <- req$options$readfunction(req$options$postfieldsize_large + 1)
      writeBin(bytes, files[index])
      return(.test_github_response(201, list(id = 100L + index, name = asset_names[index])))
    }
    index <- match(url, asset_urls)
    if (!is.na(index) && file.exists(files[index])) {
      if (req$method == "GET") {
        file.copy(files[index], req$output$path, overwrite = TRUE)
        return(.test_github_response(200))
      }
      if (req$method == "DELETE") {
        file.remove(files[index])
        return(.test_github_response(204))
      }
    }
    index <- match(url, release_urls)
    if (req$method == "DELETE" && !is.na(index)) {
      tags[index] <<- NA_character_
      unlink(files[index])
      return(.test_github_response(204))
    }
    stop("Unexpected fixture request: ", req$method, " ", req$url)
  }
  previous <- httr::set_callback("request", handle)
  withr::defer(httr::set_callback("request", previous), envir = env)
}
