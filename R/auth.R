# github
.projr_auth_get_github_pat <- function(init = FALSE) {
  pat <- .projr_auth_get_github_pat_find()
  if (nzchar(pat)) {
    return(invisible(pat))
  }
  .projr_auth_get_github_pat_warn(init = init)
  pat
}

.projr_auth_get_github_pat_find <- function() {
  # try GITHUB_PAT
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) {
    return(invisible(pat))
  }
  # try gitcreds
  if (!requireNamespace("gitcreds", quietly = TRUE)) {
    .projr_dep_install("gitcreds")
  } else {
    .projr_dep_add("gitcreds")
  }
  # taken from rstudio/renv and modified
  tryCatch(
    invisible(gitcreds::gitcreds_get()$password),
    error = function(e) {
      # remove as a forced dependency if this didn't work
      .projr_dep_rm("gitcreds")
      invisible(character())
    }
  )
}
.projr_auth_get_github_pat_warn <- function(init = FALSE) {
  warning(
    paste0(
      "
      GITHUB_PAT environment variable not found.",
      "
      ",
      .projr_auth_get_github_pat_instr(),
      "
      ",
      .projr_auth_get_github_pat_instr_init(init)
    )
  )
}

.projr_auth_get_github_pat_instr <- function() {
  "
  GITHUB_PAT is needed to create a GitHub repository.
  Please set it (in less than two minutes) by doing the following:
  1. If you do not have a GitHub account, create one here: https://github.com
  2. In R, run usethis::create_github_token()
  3. In R, run gitcreds::gitcreds_set()
  4. Paste the token from step 1 into the R command line (terminal), and press enter
  For more details, see https://happygitwithr.com/https-pat#tldr"
}

.projr_auth_get_github_pat_instr_init <- function(init = TRUE) {
  if (!init) {
    NULL
  } else {
    "
    After doing the above:
    1. In R, rerun projr::projr_init()
    It will skip what's been done already and try set up GitHub again."
  }
}

# osf
.projr_auth_get_osf_pat <- function() {
  pat <- .projr_auth_get_osf_pat_find()
  warning(
    paste0(
      "
      ",
      paste0(
        "OSF_PAT environment variable not set",
        .projr_auth_get_osf_pat_instr()
      )
    )
  )
  invisible(character())
}

.projr_auth_get_osf_pat_find <- function() {
  Sys.getenv("OSF_PAT")
}

.projr_auth_get_osf_pat_warn <- function() {
  warning(paste0(
    "
    OSF_PAT environment variable not found.",
    "
    ",
    .projr_auth_get_osf_pat_instr()
  ))
}

.projr_auth_get_osf_pat_instr <- function() {
  "
  OSF_PAT is needed to transfer to and from OSF.
  Please set it (in less than two minutes) by doing the following:
  1. If you do not have an OSF account, create one here: https://osf.io
  2. Create a token:
    i. Go to https://osf.io/settings/tokens/
    ii. Click the blue `Create token` button
    iii. Name the token (can be anything)
    iv. Choose scopes (choose all to be sure you can authenticate properly)
    v. Click the blue `Create token` button
    vi. Copy token (the really long code)
  3. In R:
    - Run usethis::edit_r_environ()
    - Paste the following in there: OSF_PAT=<your_copied_pat>
      - For example, paste OSF_PAT=adsfjk3r930923kkrf923kjdskf203i23rj23ri23j93j2r
    - Leave an empty line after OSF_PAT (IMPORTANT!)
  4. Restart R (open and close RStudio, if you're using that)
  Note that this is one method, chosen because OSF_PAT
  will be automatically detected afterwards.
  "
}

#' @title Two-minutes or less authorisation instructions
#'
#' @description `projr_instr_auth_github` and
#' `projr_instr_auth_osf` print easy-to-follow,
#' step-by-step instructions for authorisation
#' to GitHub and OSF.
#' @export
#' @rdname instr_auth
projr_instr_auth_github <- function() {
  message(.projr_auth_get_github_pat_instr())
}

#' @title Authorisation instructions
#' @export
#' @rdname instr_auth
projr_instr_auth_osf <- function() {
  message(.projr_auth_get_osf_pat_instr())
}
