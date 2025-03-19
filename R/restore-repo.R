projr_restore_repo <- function(repo,
                               path = NULL,
                               label = NULL,
                               type = NULL,
                               title = NULL) {
  .title <- title
  .assert_string(repo, TRUE)
  .git_clone(repo, path)
  tryCatch(
    .restore_repo_labels(path, label, type, .title),
    error = function(e) {
      message("Error restoring labels: ", e$message)
    }
  )
}

projr_restore_repo_wd <- function(repo, label = TRUE) {
  projr_restore_repo(repo, label = label, path = ".")
}

.restore_repo_labels <- function(path,
                                 label,
                                 type,
                                 .title) {
  if (!is.null(path)) {
    orig_wd <- getwd()
    on.exit(setwd(orig_wd), add = TRUE)
    setwd(path)
  }
  projr_restore(label, type, .title)
  invisible(TRUE)
}
