#' @rdname projr_content_update
#' @export
projr_restore_repo <- function(repo,
                               path = NULL,
                               label = NULL,
                               pos = NULL,
                               type = NULL,
                               title = NULL,
                               destination = NULL) {
  .assert_string(repo, TRUE)
  .assert_string(path)
  .assert_restore_filters(label, pos, type, title, destination)

  .title <- title
  result <- tryCatch(
    {
      path <- path %||% basename(repo)
      .git_clone(repo, path)
      .restore_repo_labels(path, label, pos, type, .title, destination)
    },
    error = function(e) {
      .cli_info("Error in projr_restore_repo: {e$message}")
      return(FALSE)
    }
  )
  invisible(result)
}

#' @rdname projr_content_update
#' @export
projr_restore_repo_wd <- function(repo,
                                  label = NULL,
                                  pos = NULL,
                                  type = NULL,
                                  title = NULL,
                                  destination = NULL) {
  projr_restore_repo(repo, path = ".", label = label, pos = pos, type = type, title = title, destination = destination)
}

.restore_repo_labels <- function(path,
                                 label,
                                 pos,
                                 type,
                                 .title,
                                 destination = NULL) {
  if (!is.null(path)) {
    orig_wd <- getwd()
    on.exit(setwd(orig_wd), add = TRUE)
    setwd(path)
  }
  projr_content_update(label, pos, type, .title, destination = destination)
}
