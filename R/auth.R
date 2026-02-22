# github
.auth_get_github_pat <- function(init = FALSE,
                                 use_gh_if_available = TRUE,
                                 use_gitcreds_if_needed = TRUE) {
  pat <- .auth_get_github_pat_find(
    use_gh_if_available = use_gh_if_available,
    use_gitcreds_if_needed = use_gitcreds_if_needed
  )
  if (.is_string(pat)) {
    return(invisible(pat))
  }
  .auth_get_github_pat_warn(init)
  pat
}


.auth_get_github_pat_find <- function(api_url = NULL,
                                      use_gh_if_available = TRUE,
                                      use_gitcreds_if_needed = TRUE) {
  # 1. Direct env vars we explicitly honour
  pat <- .auth_token_normalize(Sys.getenv("GITHUB_PAT", unset = ""))
  if (.is_string(pat)) {
    return(pat)
  }

  # 2. Prefer gh if installed, but do not require it
  if (requireNamespace("gh", quietly = TRUE) && use_gh_if_available) {
    token <- tryCatch(
      suppressWarnings(gh::gh_token(api_url)) |>
        .auth_token_normalize(),
      error = function(e) ""
    )
    if (nzchar(token)) {
      return(token)
    }
  }

  # 3. Fallback: gitcreds, if available
  if (requireNamespace("gitcreds", quietly = TRUE) && use_gitcreds_if_needed) {
    token <- .auth_get_github_pat_find_gitcreds(api_url)
    if (nzchar(token)) {
      return(token)
    }
  }

  # 4. Fallback to GH_TOKEN
  token <- Sys.getenv("GH_TOKEN", "") |>
    .auth_token_normalize()
  if (nzchar(token)) {
    return(token)
  }

  # 5. Final fallback to GITHUB_TOKEN
  Sys.getenv("GITHUB_TOKEN", "") |>
    .auth_token_normalize()
}


.auth_get_github_pat_find_gitcreds <- function(api_url = NULL) {
  url <- api_url %||% Sys.getenv("GITHUB_API_URL", "https://api.github.com")

  # Logic update: Robustly handle Standard vs Enterprise GitHub hosts
  if (grepl("api.github.com", url, fixed = TRUE)) {
    host <- "https://github.com"
  } else {
    # For Enterprise, strip the /api/v3 suffix (and optional trailing slash)
    host <- sub("/api/v3/?$", "", url)
  }

  creds <- tryCatch(
    suppressWarnings(gitcreds::gitcreds_get(host)),
    gitcreds_nogit_error    = function(e) NULL,
    gitcreds_no_credentials = function(e) NULL,
    error                   = function(e) NULL,
    # Catch any other condition types
    condition               = function(e) NULL
  )

  if (is.null(creds)) "" else .auth_token_normalize(creds$password)
}

# Helper function to normalize authentication tokens
# Ensures consistent representation of empty/missing tokens as ""
.auth_token_normalize <- function(token) {
  if (is.null(token) || length(token) != 1L || is.na(token)) {
    return("")
  }
  if (!is.character(token)) {
    return("")
  }
  token
}

.auth_get_github_pat_warn <- function(init = FALSE) {
  warning(
    "No GitHub authentication could be found.\n", # nolint
    "\n", # nolint
    .auth_get_github_pat_instr(),
    "\n", # nolint
    .auth_get_github_pat_instr_init(init),
    call. = FALSE
  )
}

.auth_get_github_pat_instr <- function() {
  .dep_install_only("usethis")
  c(
    "GitHub authentication is required to create or use a GitHub ", # nolint
    "repository.\n", # nolint
    "\n", # nolint
    "You can set this up in under two minutes:\n", # nolint
    "1. If you do not have a GitHub account, create one at ", # nolint
    "https://github.com\n", # nolint
    "2. In R, run usethis::create_github_token() and follow the ", # nolint
    "   prompts in your browser to create a personal access token\n", # nolint
    "3. In R, run gitcreds::gitcreds_set()\n", # nolint
    "4. Paste the token you copied from your browser into the R ", # nolint
    "   console and press Enter\n", # nolint
    "\n", # nolint
    "After this, projr will automatically detect your GitHub ", # nolint
    "credentials when needed.\n", # nolint
    "\n", # nolint
    "For more details, see https://happygitwithr.com/https-pat#tldr\n" # nolint
  )
}

.auth_get_github_pat_instr_init <- function(init = TRUE) {
  if (!init) {
    return(NULL)
  }
  c(
    "After doing the above:\n", # nolint
    "1. In R, rerun projr::projr_init()\n", # nolint
    "It will skip what's been done already and try set up GitHub again." # nolint
  )
}

#' @title Two-minutes or less authorisation instructions
#'
#' @description Print easy-to-follow,
#' step-by-step instructions for authorisation
#' to GitHub.
#' @export
#' @rdname instr_auth
projr_instr_auth_github <- function() {
  .cli_info(.auth_get_github_pat_instr())
}

# auth check functions (throw errors if auth is missing)
# ------------------------------------------------------

#' Check GitHub authentication
#'
#' @description
#' Checks if GitHub authentication is available. Throws an error if not.
#' Used internally before making GitHub API calls.
#'
#' @param context Character string describing the operation context for error messages.
#' Default is NULL.
#'
#' @return Invisible TRUE if authentication is available.
#' @keywords internal
.auth_check_github <- function(context = NULL) {
  pat <- .auth_get_github_pat_find()
  if (.is_string(pat)) {
    return(invisible(TRUE))
  }
  .auth_check_github_error(context)
}

.auth_check_github_error <- function(context = NULL) {
  context_msg <- if (!is.null(context)) {
    paste0("GitHub authentication is required for: ", context, "\n\n")
  } else {
    "GitHub authentication is required.\n\n"
  }

  stop(
    context_msg,
    paste(.auth_get_github_pat_instr(), collapse = ""),
    call. = FALSE
  )
}
