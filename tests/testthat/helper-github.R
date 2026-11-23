# Live GitHub tests require an explicit opt-in before credential or network access.
.test_can_modify_github <- function() {
  identical(Sys.getenv("PROJR_TEST_LIVE_GITHUB"), "true")
}

.test_skip_if_cannot_modify_github <- function() {
  if (!.test_can_modify_github()) {
    testthat::skip(
      "Live GitHub tests disabled (PROJR_TEST_LIVE_GITHUB=true to opt in)"
    )
  }
  invisible(TRUE)
}

# Repository lifecycle tests need an explicitly managed fixture. Never create or
# delete a repository automatically, even when the live lane is enabled.
.test_github_repo_create <- function(...) {
  testthat::skip(
    "Automatic GitHub repository creation is disabled; use a managed fixture"
  )
}

.test_github_response <- function(status, body = list()) {
  structure(
    list(
      status_code = as.integer(status),
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
    ),
    class = "response"
  )
}

# Each fixture has its own releases and monotonically assigned asset IDs. Intercept
# httr before transport so unknown routes fail without opening a connection.
.test_local_github_releases <- function(
  tags,
  env = parent.frame(),
  repo = "fixture/project",
  checkout_repo = repo
) {
  api <- "https://fixture.invalid"
  withr::local_envvar(
    c(GITHUB_API_URL = api, GITHUB_PAT = "fixture-token"),
    .local_envir = env
  )
  testthat::local_mocked_bindings(
    .gh_guess_repo = function(...) checkout_repo,
    .git_repo_is_worktree = function() FALSE,
    .package = "projr",
    .env = env
  )
  directory <- withr::local_tempdir(.local_envir = env)
  assets <- data.frame(
    id = integer(),
    name = character(),
    release = integer(),
    url = character()
  )
  next_id <- 100L
  fail_upload <- NULL
  releases <- paste0(api, "/repos/", repo, "/releases")
  release_urls <- paste0(releases, "/", seq_along(tags))
  upload_urls <- paste0(release_urls, "/assets")

  handle <- function(req) {
    url <- sub("[?].*$", "", req$url)
    index <- match(url, paste0(releases, "/tags/", tags))
    if (req$method == "GET" && !is.na(index)) {
      owned <- which(assets$release == index)
      entries <- lapply(owned, function(i) {
        as.list(assets[i, c("id", "name", "url")])
      })
      return(.test_github_response(
        200,
        list(
          id = index,
          assets = entries,
          upload_url = paste0(upload_urls[index], "{?name,label}")
        )
      ))
    }
    if (req$method == "GET" && startsWith(url, paste0(releases, "/tags/"))) {
      return(.test_github_response(404))
    }
    index <- match(url, upload_urls)
    if (req$method == "POST" && !is.na(index)) {
      bytes <- req$options$readfunction(req$options$postfieldsize_large + 1)
      name <- httr::parse_url(req$url)$query$name
      if (identical(name, fail_upload)) {
        fail_upload <<- NULL
        return(.test_github_response(503))
      }
      if (any(assets$release == index & assets$name == name)) {
        return(.test_github_response(422))
      }
      next_id <<- next_id + 1L
      item <- data.frame(
        id = next_id,
        name = name,
        release = index,
        url = paste0(releases, "/assets/", next_id)
      )
      assets <<- rbind(assets, item)
      writeBin(bytes, file.path(directory, next_id))
      return(.test_github_response(201, as.list(item[c("id", "name", "url")])))
    }
    index <- match(url, assets$url)
    if (!is.na(index)) {
      file <- file.path(directory, assets$id[index])
      if (req$method == "GET") {
        file.copy(file, req$output$path, overwrite = TRUE)
        return(.test_github_response(200))
      }
      if (req$method == "DELETE") {
        assets <<- assets[-index, , drop = FALSE]
        file.remove(file)
        return(.test_github_response(204))
      }
    }
    index <- match(url, release_urls)
    if (req$method == "DELETE" && !is.na(index)) {
      tags[index] <<- NA_character_
      unlink(file.path(directory, assets$id[assets$release == index]))
      assets <<- assets[assets$release != index, , drop = FALSE]
      return(.test_github_response(204))
    }
    stop("Unexpected fixture request: ", req$method, " ", req$url)
  }
  previous <- httr::set_callback("request", handle)
  withr::defer(httr::set_callback("request", previous), envir = env)
  invisible(function(name) fail_upload <<- name)
}
