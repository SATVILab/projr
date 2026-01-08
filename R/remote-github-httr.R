# ========================
# check existence
# ========================

#' Check if a GitHub release exists using httr
#'
#' @description
#' Uses the GitHub API directly to check if a release with the given tag exists.
#' This is faster and more explicit than relying on piggyback internals.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag to check.
#' @param api_url Character string. Optional GitHub API URL for enterprise instances.
#' @param token Character string. Optional GitHub token. If not provided, uses
#'   `.auth_get_github_pat_find()`.
#'
#' @return Logical TRUE/FALSE if release exists/doesn't exist and stops on auth errors
#' @keywords internal
.remote_check_exists_github_httr <- function(
  repo,
  tag,
  api_url = NULL,
  token = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_exists_httr(); please install it.")
  }

  base <- .github_api_base(api_url)

  # Encode tag safely (handles slashes, spaces, etc.)
  tag_enc <- utils::URLencode(tag, reserved = TRUE)

  url <- sprintf("%s/repos/%s/releases/tags/%s", base, repo, tag_enc)

  # Use explicit token if provided, otherwise reuse existing PAT finder
  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)

  headers <- c("Accept" = "application/vnd.github+json")
  if (.is_string(tok)) {
    # GitHub accepts both "token" and "Bearer"; use conventional "token"
    headers["Authorization"] <- paste("token", tok)
  }

  resp <- httr::GET(url, httr::add_headers(.headers = headers))
  status <- httr::status_code(resp)

  if (status == 200L) {
    TRUE
  } else if (status == 404L) {
    FALSE
  } else if (status == 401L || status == 403L) {
    stop(
      "GitHub API authentication error (status ", status, ") ",
      "when checking release existence for tag '", tag, "' in repo '", repo, "'. ",
      "Please check your GITHUB_PAT is set correctly.",
      call. = FALSE
    )
  } else {
    stop(
      "Unexpected HTTP status from GitHub API: ",
      status, " (url: ", url, ")",
      call. = FALSE
    )
  }
}

# ========================
# check existence of remote_final
# ========================


.remote_final_check_exists_github_httr <- function(repo,
                                                   tag,
                                                   asset,
                                                   api_url = NULL,
                                                   token = NULL) {
  assets <- .remote_ls_final_github_httr(
    repo = repo,
    tag = tag,
    api_url = api_url,
    token = token
  )
  if (length(assets) == 0L) {
    return(FALSE)
  }
  asset %in% assets
}

# ========================
# Create release
# ========================

#' Create a GitHub release using httr
#'
#' @description
#' Creates a GitHub release for the given tag using the GitHub REST API.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag.
#' @param description Character string. Release body text.
#' @param api_url Character string. Optional GitHub API URL (for Enterprise).
#' @param token Character string. Optional GitHub token. If not supplied,
#'   `.auth_get_github_pat_find()` is used.
#' @param draft Logical. Whether the release should be created as a draft.
#* @param prerelease Logical. Whether the release is a prerelease.
#' @param target_commitish Character string. Optional target commitish
#'   (branch or SHA). If NULL, GitHub uses the default branch.
#'
#' @return Parsed release object (list) from GitHub API on success.
#' @keywords internal
.gh_release_create_httr <- function(
  repo,
  tag,
  description,
  api_url = NULL,
  token = NULL,
  draft = FALSE,
  prerelease = FALSE,
  target_commitish = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_create_httr(); please install it.")
  }

  base <- .github_api_base(api_url)
  url <- sprintf("%s/repos/%s/releases", base, repo)

  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)

  headers <- c(
    "Accept"       = "application/vnd.github+json",
    "Content-Type" = "application/json"
  )
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  body <- list(
    tag_name   = tag,
    name       = tag, # you can change this if you want a nicer title
    body       = description,
    draft      = isTRUE(draft),
    prerelease = isTRUE(prerelease)
  )
  if (!is.null(target_commitish)) {
    body$target_commitish <- target_commitish
  }

  resp <- httr::POST(
    url,
    httr::add_headers(.headers = headers),
    body = body,
    encode = "json"
  )

  status <- httr::status_code(resp)

  if (status == 201L) {
    httr::content(resp, as = "parsed")
  } else if (status == 422L) {
    # Typically "Validation Failed" – often because the release already exists
    detail <- tryCatch(
      httr::content(resp, as = "parsed")$message,
      error = function(e) NULL
    )
    stop(
      "Failed to create release (likely already exists): HTTP 422",
      if (!is.null(detail)) paste0(" - ", detail) else "",
      call. = FALSE
    )
  } else {
    stop(
      "Failed to create release: HTTP ",
      status, " (url: ", url, ")",
      call. = FALSE
    )
  }
}


# ========================
# List all final remotes in a particular pre-remote
# ========================

#' Get release information by tag using httr
#'
#' @description
#' Fetches release information from GitHub API using the tag name.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag.
#' @param api_url Character string. Optional GitHub API URL.
#' @param token Character string. Optional GitHub token.
#'
#' @return List containing release information from GitHub API.
#' @keywords internal
.remote_ls_final_github_httr <- function(
  repo,
  tag,
  api_url = NULL,
  token = NULL
) {
  asset_list <- .remote_get_info_github_httr(
    repo = repo,
    tag = tag,
    api_url = api_url,
    token = token
  )
  if (length(asset_list) == 0L) {
    character(0L)
  } else {
    vapply(
      asset_list,
      function(x) x$name,
      FUN.VALUE = character(1L)
    )
  }
}

.remote_get_info_github_httr <- function(repo,
                                         tag,
                                         api_url = NULL,
                                         token = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .remote_final_check_exists_github_httr(); please install it.") # nolint
  }

  base <- .github_api_base(api_url)
  tag_enc <- utils::URLencode(tag, reserved = TRUE)
  url <- sprintf("%s/repos/%s/releases/tags/%s", base, repo, tag_enc)

  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers <- c("Accept" = "application/vnd.github+json")
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  resp <- httr::GET(url, httr::add_headers(.headers = headers))
  status <- httr::status_code(resp)

  if (status == 200L) {
    return(httr::content(resp, as = "parsed")$assets)
  } else if (status == 404L) {
    stop("Release not found: ", tag, " in ", repo, call. = FALSE)
  } else {
    stop(
      "Failed to get release: HTTP ",
      status, " (url: ", url, ")",
      call. = FALSE
    )
  }
}

# =======================
# Get information about the final remote
# =======================

.remote_final_get_info_github_httr <- function(repo,
                                               tag,
                                               asset_name,
                                               api_url = NULL,
                                               token = NULL) {
  asset_list <- .remote_get_info_github_httr(
    repo = repo,
    tag = tag,
    api_url = api_url,
    token = token
  )

  if (length(asset_list) == 0L) {
    stop(
      "No assets found in release '", tag,
      "' for repo '", repo, "'.",
      call. = FALSE
    )
  }

  for (asset in asset_list) {
    if (asset$name == asset_name) {
      return(asset)
    }
  }

  stop(
    "Asset '", asset_name, "' not found in release '", tag, "' for repo '", repo, "'.",
    call. = FALSE
  )
}


# ========================
# Delete a remote
# ========================

#' Delete a GitHub release using httr
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag to delete.
#' @param api_url Optional GitHub API base URL.
#' @param token Optional GitHub token.
#' @return Logical TRUE on success, FALSE if not found.
#' @keywords internal
.remote_rm_github_httr <- function(repo,
                                   tag,
                                   api_url = NULL,
                                   token = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .remote_rm_github_httr(); please install it.")
  }
  .assert_string(repo, TRUE)
  .assert_string(tag, TRUE)

  base <- .github_api_base(api_url)
  tag_enc <- utils::URLencode(tag, reserved = TRUE)

  # First get the release to obtain its ID (required for DELETE)
  url_release <- sprintf("%s/repos/%s/releases/tags/%s", base, repo, tag_enc)

  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers <- c("Accept" = "application/vnd.github+json")
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  resp_get <- httr::GET(url_release, httr::add_headers(.headers = headers))
  status_get <- httr::status_code(resp_get)

  if (status_get == 404L) {
    return(FALSE)
  }
  if (status_get != 200L) {
    stop(
      "Failed to fetch release for deletion: HTTP ",
      status_get, " (url: ", url_release, ")",
      call. = FALSE
    )
  }

  release_obj <- httr::content(resp_get, as = "parsed")
  release_id <- release_obj$id %||% NA_integer_
  if (is.na(release_id)) {
    stop("Failed to obtain release id for tag '", tag, "'.", call. = FALSE)
  }

  url_delete <- sprintf("%s/repos/%s/releases/%s", base, repo, release_id)
  resp_del <- httr::DELETE(url_delete, httr::add_headers(.headers = headers))
  status_del <- httr::status_code(resp_del)

  if (status_del == 204L) {
    return(TRUE)
  }
  if (status_del == 404L) {
    return(FALSE)
  }
  stop(
    "Failed to delete release: HTTP ",
    status_del, " (url: ", url_delete, ")",
    call. = FALSE
  )
}

# ========================
# Empty final remote
# ========================

#' Delete a release asset using httr
#'
#' @description
#' Deletes an asset from a GitHub release by filename.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag.
#' @param fn Character string. Filename of the asset to delete.
#' @param api_url Character string. Optional GitHub API URL.
#' @param token Character string. Optional GitHub token.
#'
#' @return Logical. TRUE if successful.
#' @keywords internal
.remote_final_rm_github_httr <- function(
  repo,
  tag,
  fn,
  api_url = NULL,
  token = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .remote_final_rm_github_httr(); please install it.")
  }
  if (length(fn) != 1L) {
    stop("Expected exactly one fn to delete, got ", length(fn), call. = FALSE)
  }

  assetid <- .gh_httr_get_assetid(
    repo = .gh_repo_get(),
    tag = tag,
    asset_name = fn,
    api_url = api_url,
    token = token
  )

  base <- .github_api_base(api_url)
  url <- sprintf("%s/repos/%s/releases/assets/%s", base, repo, assetid)

  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers <- c("Accept" = "application/vnd.github+json")
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  resp <- httr::DELETE(url, httr::add_headers(.headers = headers))
  status <- httr::status_code(resp)

  if (status == 204L) {
    TRUE
  } else {
    stop(
      "Failed to delete asset: HTTP ",
      status, " (url: ", url, ")",
      call. = FALSE
    )
  }
}

# ========================
# Download all files
# ========================

#' Download all assets from a GitHub release using httr
#'
#' @description
#' Downloads every asset attached to a GitHub release (identified by tag)
#' into a local directory. Uses the GitHub API via `httr` and supports
#' GitHub Enterprise via `api_url`.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag.
#' @param fn Character string. Asset filename to download.
#' @param dest_dir Character string. Local directory to save assets into.
#'   Created if it does not exist.
#' @param api_url Character string. Optional GitHub API URL.
#' @param token Character string. Optional GitHub token. If not supplied,
#'   `.auth_get_github_pat_find()` is used.
#' @param overwrite Logical. If FALSE, existing files are left untouched.
#'
#' @return Character vector of downloaded file paths (invisibly).
#' @keywords internal
.remote_file_get_all_github_httr <- function(
  repo,
  tag,
  fn,
  dest_dir,
  api_url = NULL,
  token = NULL,
  overwrite = TRUE
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .remote_file_get_all_github_httr(); please install it.")
  }

  # Get release info (includes assets) using existing helper
  asset <- .remote_final_get_info_github_httr(
    repo = repo,
    tag = tag,
    asset_name = fn,
    api_url = api_url,
    token = token
  ) %||% character()

  if (length(asset) == 0L) {
    .cli_debug(
      "GitHub release: No assets found in release '{tag}' for repo '{repo}'"
    )
    return(invisible(character()))
  }

  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Prepare headers for authenticated download if needed
  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers <- c("Accept" = "application/octet-stream")
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  downloaded <- character(0)

  asset_name <- asset$name
  download_url <- asset$url

  dest_file <- file.path(dest_dir, asset_name)

  if (!overwrite && file.exists(dest_file)) {
    .cli_debug(
      "GitHub release: Skipping existing asset '{asset_name}' in '{dest_file}'"
    )
    downloaded <- c(downloaded, dest_file)
    return(invisible(downloaded))
  }

  .cli_debug(
    "GitHub release: Downloading asset '{asset_name}' from tag '{tag}' to '{dest_file}'"
  )

  resp <- httr::GET(
    download_url,
    httr::add_headers(.headers = headers),
    httr::write_disk(dest_file, overwrite = TRUE)
  )

  status <- httr::status_code(resp)

  if (status >= 200L && status < 300L) {
    downloaded <- c(downloaded, dest_file)
  } else {
    stop(
      "Failed to download asset '", asset_name, "': HTTP ",
      status, " (url: ", download_url, ")",
      call. = FALSE
    )
  }

  .cli_debug(
    "GitHub release: Downloaded {length(downloaded)} asset(s) from tag '{tag}'"
  )

  invisible(downloaded)
}

.gh_httr_get_assetid <- function(repo,
                                 tag,
                                 asset_name,
                                 api_url = NULL,
                                 token = NULL) {
  # about all assets in the release
  .remote_final_get_info_github_httr(
    repo = repo,
    tag = tag,
    asset_name = asset_name,
    api_url = api_url,
    token = token
  )$id
}

#' Upload an asset to a GitHub release using httr
#'
#' @description
#' Uploads a file as an asset to an existing GitHub release using the GitHub API directly.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag.
#' @param file_path Character string. Path to file to upload.
#' @param asset_name Character string. Optional name for the asset.
#'   If NULL, uses basename of file_path.
#' @param overwrite Logical. If TRUE and asset exists, deletes it first.
#' @param api_url Character string. Optional GitHub API URL.
#' @param token Character string. Optional GitHub token.
#'
#' @return List containing asset information from GitHub API.
#' @keywords internal
.gh_release_asset_upload_httr <- function(repo,
                                          tag,
                                          file_path,
                                          asset_name = NULL,
                                          overwrite = TRUE,
                                          api_url = NULL,
                                          token = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_asset_upload_httr(); please install it.") # nolint
  }

  .assert_string(tag, TRUE)
  .assert_string(repo, TRUE)
  if (!file.exists(file_path)) {
    stop("File to upload does not exist: ", file_path, call. = FALSE)
  }

  # Determine asset name
  if (is.null(asset_name)) {
    asset_name <- basename(file_path)
  }
  .assert_string(asset_name, TRUE)

  # Check that release exists
  if (!.remote_check_exists_github_httr(repo = repo, tag = tag, api_url = api_url, token = token)) {
    stop("Release '", tag, "' not found in repo '", repo, "'.", call. = FALSE)
  }

  # If asset exists, optionally delete it first when overwrite = TRUE
  if (.remote_final_check_exists_github_httr(repo = repo, tag = tag, asset = asset_name, api_url = api_url, token = token)) {
    if (isTRUE(overwrite)) {
      .cli_debug(
        "GitHub release: Asset '{asset_name}' exists in release '{tag}', deleting before upload",
        asset_name = asset_name,
        tag = tag
      )
      .remote_final_rm_github_httr(repo = repo, tag = tag, fn = asset_name, api_url = api_url, token = token)
    } else {
      stop(
        "Asset '", asset_name, "' already exists in release '", tag,
        "' and overwrite is FALSE.",
        call. = FALSE
      )
    }
  }
  # Always fetch release object to obtain upload_url (works for public and enterprise)
  base <- .github_api_base(api_url)
  tag_enc <- utils::URLencode(tag, reserved = TRUE)
  release_url <- sprintf("%s/repos/%s/releases/tags/%s", base, repo, tag_enc)

  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers_release <- c("Accept" = "application/vnd.github+json")
  if (.is_string(tok)) {
    headers_release["Authorization"] <- paste("token", tok)
  }

  resp_release <- httr::GET(release_url, httr::add_headers(.headers = headers_release))
  status_release <- httr::status_code(resp_release)

  if (status_release == 200L) {
    release_obj <- httr::content(resp_release, as = "parsed")
    upload_url <- release_obj$upload_url %||% ""
    if (!nzchar(upload_url)) {
      stop("Could not obtain upload_url for release '", tag, "' in repo '", repo, "'.", call. = FALSE)
    }
    # strip template placeholder and add name
    upload_url <- sub("\\{\\?name,label\\}$", "", upload_url)
    upload_url <- httr::modify_url(
      upload_url,
      query = list(name = utils::URLencode(asset_name, reserved = TRUE))
    )
  } else if (status_release == 404L) {
    stop("Release not found: ", tag, " in ", repo, call. = FALSE)
  } else {
    stop("Failed to get release: HTTP ", status_release, " (url: ", release_url, ")", call. = FALSE)
  }

  # Prepare headers for upload
  headers_upload <- c(
    "Accept" = "application/vnd.github+json",
    "Content-Type" = "application/octet-stream"
  )
  if (.is_string(tok)) {
    headers_upload["Authorization"] <- paste("token", tok)
  }

  .cli_debug(
    "GitHub release: Uploading asset '{asset_name}' to tag '{tag}' in repo '{repo}'",
    asset_name = asset_name,
    tag = tag,
    repo = repo
  )

  resp <- httr::POST(
    upload_url,
    httr::add_headers(.headers = headers_upload),
    body = httr::upload_file(file_path)
  )

  status <- httr::status_code(resp)

  if (status == 201L) {
    httr::content(resp, as = "parsed")
  } else if (status == 422L) {
    # Validation failed - possibly asset already present (race condition)
    detail <- tryCatch(httr::content(resp, as = "parsed")$message, error = function(e) NULL)
    stop("Failed to upload asset (HTTP 422)", if (!is.null(detail)) paste0(": ", detail) else "", call. = FALSE)
  } else {
    stop("Failed to upload asset: HTTP ", status, " (url: ", upload_url, ")", call. = FALSE)
  }
}

# ========================
# Miscellaneous
# =======================

#' Get GitHub API base URL
#'
#' @description
#' Resolves the GitHub API base URL from explicit argument, environment variable,
#' or defaults to public GitHub API.
#'
#' @param api_url Character string. Explicit API URL to use.
#'   Takes precedence over environment variable.
#' @return Character string with trailing slashes removed.
#' @keywords internal
.github_api_base <- function(api_url = NULL) {
  # Argument wins, then env var, then hard-coded default
  base <- api_url %||% Sys.getenv("GITHUB_API_URL", "https://api.github.com")
  sub("/+$", "", base)
}
