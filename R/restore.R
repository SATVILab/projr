#' Restore project artefact directories
#'
#' `projr_content_update()` is deprecated. Use [projr_source()], which it now
#' calls with the latest version and `cue = "always"`.
#' `projr_restore_repo()` clones a GitHub repository and then fetches its
#' directories with [projr_source()].
#'
#' @param label character vector or NULL. Directory labels to restore.
#'   Default is `NULL`, restoring every declared raw directory.
#' @param pos Ignored. Kept for compatibility.
#' @param type character or NULL. Remote type, `"local"` or `"github"`, used
#'   with `title` to name the remote as `<type>:<title>`.
#' @param title character or NULL. Title of a remote registered under
#'   `remotes`. Default is `NULL`, using `build.source` or the single
#'   registered remote that published each label.
#' @param repo character. GitHub repository (`"owner/repo"` or `"repo"`).
#'   (Only for repository restoration functions.)
#' @param path character or NULL. Local path for cloning the repository. Default is `NULL`,
#'   creating a subdirectory named after the repo. `"."` restores directly into the
#'   current directory.
#' @param destination No longer supported. Must be `NULL`; [projr_source()]
#'   reads the address of the registered remote.
#' @param clear logical. If `TRUE`, local changes are replaced
#'   (`conflict = "remote"`). Otherwise, local changes stop the restore
#'   (`conflict = "error"`). Default is `FALSE`.
#'
#' @return Invisibly returns `TRUE`. `projr_restore_repo()` returns `FALSE` if
#'   cloning or restoration fails.
#'
#' @examples
#' \dontrun{
#' # Fetch raw directories in an existing project
#' projr_source()
#'
#' # Clone repository into subdirectory and restore artefacts
#' projr_restore_repo("owner/repo")
#'
#' # Clone to specific path
#' projr_restore_repo("owner/repo", path = "my-project")
#'
#' # Clone repository into current directory and restore artefacts
#' projr_restore_repo_wd("owner/repo")
#' }
#'
#' @name projr_content_update
#' @rdname projr_content_update
#' @export
projr_content_update <- function(
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  clear = FALSE,
  destination = NULL
) {
  msg <- "projr_content_update() is deprecated. Use projr_source() instead."
  if (!is.null(pos)) {
    msg <- paste(msg, "`pos` is ignored.")
  }
  .Deprecated("projr_source", msg = msg)
  .assert_flag(clear, TRUE)
  projr_source(
    label,
    .restore_remote(type, title, destination),
    cue = "always",
    conflict = if (clear) "remote" else "error"
  )
}

# Map the historical `type`/`title` selectors to a `projr_source()` remote.
.restore_remote <- function(type, title, destination) {
  .assert_string(type)
  .assert_in(type, c("local", "github"))
  .assert_string(title)
  if (!is.null(destination)) {
    stop(
      "`destination` is no longer supported. projr_source() reads the ",
      "address of the remote registered under `remotes`.",
      call. = FALSE
    )
  }
  if (is.null(title)) {
    if (!is.null(type)) {
      stop(
        "`type` alone no longer selects a remote. Name one with `title`.",
        call. = FALSE
      )
    }
    return(NULL)
  }
  if (is.null(type)) title else paste0(type, ":", title)
}
