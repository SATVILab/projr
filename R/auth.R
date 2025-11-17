

# github
.auth_get_github_pat <- function(init = FALSE) {
  pat <- .auth_get_github_pat_find()
  if (.is_string(pat)) {
    return(invisible(pat))
  }
  .auth_get_github_pat_warn(init)
  pat
}

.auth_get_github_pat_find_gitcreds <- function(api_url = NULL) {
  url  <- api_url %||% Sys.getenv("GITHUB_API_URL", "https://api.github.com")
  host <- sub("^https?://api\\.", "https://", url)

  creds <- suppressWarnings(tryCatch(
    gitcreds::gitcreds_get(host),
    gitcreds_nogit_error    = function(e) NULL,
    gitcreds_no_credentials = function(e) NULL,
    error                   = function(e) NULL
  ))

  if (is.null(creds)) "" else .auth_token_normalize(creds$password)
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
    token <- suppressWarnings(gh::gh_token(api_url)) |>
      .auth_token_normalize()
    if (nzchar(token)) {
      return(token)
    }
  }

  # 3. Fallback: gitcreds, if available
  if (requireNamespace("gitcreds", quietly = TRUE) && use_gitcreds_if_needed) {
    .auth_get_github_pat_find_gitcreds(api_url)
  }
  Sys.getenv("GITHUB_TOKEN", "") |>
    .auth_token_normalize()
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
    "\n",                                         # nolint
    .auth_get_github_pat_instr(),
    "\n",                                         # nolint
    .auth_get_github_pat_instr_init(init),
    call. = FALSE
  )
}

.auth_get_github_pat_instr <- function() {
  .dep_install_only("usethis")
  c(
    "GitHub authentication is required to create or use a GitHub ",      # nolint
    "repository.\n",                                                     # nolint
    "\n",                                                                # nolint
    "You can set this up in under two minutes:\n",                       # nolint
    "1. If you do not have a GitHub account, create one at ",           # nolint
    "https://github.com\n",                                              # nolint
    "2. In R, run usethis::create_github_token() and follow the ",      # nolint
    "   prompts in your browser to create a personal access token\n",    # nolint
    "3. In R, run gitcreds::gitcreds_set()\n",                           # nolint
    "4. Paste the token you copied from your browser into the R ",      # nolint
    "   console and press Enter\n",                                      # nolint
    "\n",                                                                # nolint
    "After this, projr will automatically detect your GitHub ",         # nolint
    "credentials when needed.\n",                                        # nolint
    "\n",                                                                # nolint
    "For more details, see https://happygitwithr.com/https-pat#tldr\n"   # nolint
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

# osf
.auth_get_osf_pat <- function() {
  pat <- .auth_get_osf_pat_find()
  if (.is_string(pat)) {
    return(invisible(pat))
  }
  warning(
    "\n",
    "OSF_PAT environment variable not set", # nolint
    "\n",
    .auth_get_osf_pat_instr(),
    call. = FALSE
  )
  invisible("")
}

.auth_get_osf_pat_find <- function() {
  pat <- Sys.getenv("OSF_PAT")
  # Normalize the result to ensure consistent "" for missing/empty tokens
  .auth_token_normalize(pat)
}

.auth_get_osf_pat_warn <- function() {
  warning(
    "OSF_PAT environment variable not found.\n",
    "\n",
    .auth_get_osf_pat_instr(),
    call. = FALSE
  )
}

.auth_get_osf_pat_instr <- function() {
  c(
    "OSF_PAT is needed to transfer to and from OSF.\n",
    "\n",
    "Please set it (in less than two minutes) by doing the following:\n",
    "\n",
    "1. If you do not have an OSF account, create one here: https://osf.io\n",
    "\n",
    "2. Create a token:\n",
    "  i. Go to https://osf.io/settings/tokens/\n",
    "  ii. Click the blue `Create token` button\n",
    "  iii. Name the token (can be anything)\n",
    "  iv. Choose scopes (choose all to be sure you can authenticate properly)\n", # nolint
    "  v. Click the blue `Create token` button\n",
    "  vi. Copy token (the really long code)\n",
    "\n",
    "3. In R:\n",
    "  - Run usethis::edit_r_environ()\n",
    "  - Paste the following in there: OSF_PAT=<your_copied_pat>\n",
    "    - For example, paste OSF_PAT=adsfjk3r930923kkrf923kjdskf203i23rj23ri23j93j2r\n", # nolint
    "  - Leave an empty line after OSF_PAT (IMPORTANT!)\n",
    "\n",
    "4. Restart R (open and close RStudio, if you're using that)\n",
    "\n",
    "Note that this is one method, chosen because OSF_PAT\n",
    "will be automatically detected afterwards."
  )
}

#' @title Two-minutes or less authorisation instructions
#'
#' @description .instr_auth_github` and
#' .instr_auth_osf` print easy-to-follow,
#' step-by-step instructions for authorisation
#' to GitHub and OSF.
#' @export
#' @rdname instr_auth
projr_instr_auth_github <- function() {
  message(.auth_get_github_pat_instr())
}

#' @title Authorisation instructions
#' @export
#' @rdname instr_auth
projr_instr_auth_osf <- function() {
  message(.auth_get_osf_pat_instr())
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

#' Check OSF authentication
#'
#' @description
#' Checks if OSF authentication is available. Throws an error if not.
#' Used internally before making OSF API calls.
#'
#' @param context Character string describing the operation context for error messages.
#' Default is NULL.
#'
#' @return Invisible TRUE if authentication is available.
#' @keywords internal
.auth_check_osf <- function(context = NULL) {
  pat <- .auth_get_osf_pat_find()
  if (.is_string(pat)) {
    return(invisible(TRUE))
  }

  context_msg <- if (!is.null(context)) {
    paste0("OSF authentication is required for: ", context, "\n\n")
  } else {
    "OSF authentication is required.\n\n"
  }

  stop(
    context_msg,
    paste(.auth_get_osf_pat_instr(), collapse = ""),
    call. = FALSE
  )
}
