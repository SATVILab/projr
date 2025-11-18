# GitHub Release API helpers using httr
# =======================================
# Direct GitHub API implementations for release management
# All functions suffixed with _httr to distinguish from piggyback (_pb) implementations

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
.gh_api_base <- function(api_url = NULL) {
  # Argument wins, then env var, then hard-coded default
  base <- api_url %||% Sys.getenv("GITHUB_API_URL", "https://api.github.com")
  sub("/+$", "", base)
}

#' Check if a GitHub release exists using httr
#'
#' @description
#' Uses the GitHub API directly to check if a release with the given tag exists.
#' This is faster and more explicit than relying on piggyback internals.
#' Returns NULL on authentication errors (401/403) to allow fallback to piggyback.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param tag Character string. Release tag to check.
#' @param api_url Character string. Optional GitHub API URL for enterprise instances.
#' @param token Character string. Optional GitHub token. If not provided, uses
#'   `.auth_get_github_pat_find()`.
#'
#' @return Logical TRUE/FALSE if release exists/doesn't exist, NULL on auth errors.
#' @keywords internal
.gh_release_exists_httr <- function(
  repo,
  tag,
  api_url = NULL,
  token  = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_exists_httr(); please install it.")
  }

  base <- .gh_api_base(api_url)

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
    # Auth errors - return NULL to signal caller should fallback to piggyback
    NULL
  } else {
    stop(
      "Unexpected HTTP status from GitHub API: ",
      status, " (url: ", url, ")",
      call. = FALSE
    )
  }
}

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
.gh_release_get_httr <- function(
  repo,
  tag,
  api_url = NULL,
  token = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_get_httr(); please install it.")
  }

  base <- .gh_api_base(api_url)
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
    httr::content(resp, as = "parsed")
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

#' Delete a release asset using httr
#'
#' @description
#' Deletes an asset from a GitHub release by asset ID.
#'
#' @param repo Character string. Repository in format "owner/repo".
#' @param asset_id Numeric. Asset ID to delete.
#' @param api_url Character string. Optional GitHub API URL.
#' @param token Character string. Optional GitHub token.
#'
#' @return Logical. TRUE if successful.
#' @keywords internal
.gh_release_asset_delete_httr <- function(
  repo,
  asset_id,
  api_url = NULL,
  token = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_asset_delete_httr(); please install it.")
  }

  base <- .gh_api_base(api_url)
  url <- sprintf("%s/repos/%s/releases/assets/%s", base, repo, asset_id)

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
.gh_release_asset_upload_httr <- function(
  repo,
  tag,
  file_path,
  asset_name = NULL,
  overwrite = TRUE,
  api_url = NULL,
  token = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr is required for .gh_release_asset_upload_httr(); please install it.")
  }

  # Get release information
  release_info <- .gh_release_get_httr(repo, tag, api_url, token)

  # Determine asset name
  if (is.null(asset_name)) {
    asset_name <- basename(file_path)
  }

  # Check if asset already exists and handle overwrite
  if (overwrite && !is.null(release_info$assets)) {
    for (asset in release_info$assets) {
      if (asset$name == asset_name) {
        .gh_release_asset_delete_httr(repo, asset$id, api_url, token)
        break
      }
    }
  }

  # Prepare upload URL
  upload_url <- release_info$upload_url
  # Remove template placeholder from upload_url
  upload_url <- sub("\\{\\?name,label\\}$", "", upload_url)
  upload_url <- paste0(upload_url, "?name=", utils::URLencode(asset_name, reserved = TRUE))

  # Prepare headers
  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers <- c(
    "Accept" = "application/vnd.github+json",
    "Content-Type" = "application/octet-stream"
  )
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  # Upload file
  resp <- httr::POST(
    upload_url,
    httr::add_headers(.headers = headers),
    body = httr::upload_file(file_path)
  )

  status <- httr::status_code(resp)

  if (status == 201L) {
    httr::content(resp, as = "parsed")
  } else {
    stop(
      "Failed to upload asset: HTTP ",
      status, " (url: ", upload_url, ")",
      call. = FALSE
    )
  }
}
