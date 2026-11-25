#' @title Clone a project and fetch its directories
#' @description
#' `projr_restore_repo()` clones a GitHub repository and fetches its
#' directories with [projr_source()]. `projr_restore_repo_wd()` clones into
#' the current directory.
#'
#' @param repo character. GitHub repository, as `"owner/repo"`, or `"repo"`
#'   for one owned by the authenticated user.
#' @param path character. Directory to clone into. Defaults to the
#'   repository name.
#' @inheritParams projr_source
#' @return Invisibly returns `TRUE` after fetching, or `FALSE` with a message
#'   if the clone or fetch fails.
#' @seealso [projr_source()], [projr_restore()]
#' @examples
#' \dontrun{
#' projr_restore_repo("owner/repo")
#' projr_restore_repo("owner/repo", label = "raw-data", remote = "github:archive")
#' }
#' @export
projr_restore_repo <- function(
  repo,
  path = NULL,
  label = NULL,
  remote = NULL
) {
  .assert_string(repo, TRUE)
  .assert_string(path)
  .assert_chr(label)
  .assert_string(remote)

  result <- tryCatch(
    {
      path <- path %||% basename(repo)
      .git_clone(repo, path)
      .restore_repo_labels(path, label, remote)
    },
    error = function(e) {
      .cli_info("Error in projr_restore_repo: {e$message}")
      return(FALSE)
    }
  )
  invisible(result)
}

#' @rdname projr_restore_repo
#' @export
projr_restore_repo_wd <- function(repo, label = NULL, remote = NULL) {
  projr_restore_repo(repo, path = ".", label = label, remote = remote)
}

.restore_repo_labels <- function(path, label, remote) {
  orig_wd <- getwd()
  on.exit(setwd(orig_wd), add = TRUE)
  setwd(path)
  projr_source(label, remote, cue = "always", conflict = "error")
}
