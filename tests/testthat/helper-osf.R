.projr_osf_label_get_random <- function(prefix) {
  label <- paste0(
    prefix, rnorm(1) |> signif(4) |> as.character()
  )
  # add it to yml_projr
  yml_projr <- projr_yml_get_unchecked()
  yml_projr[["directories"]][[label]] <- list(
    path = "_some_random_path"
  )
  .projr_yml_set(yml_projr)
  label
}

.projr_osf_rm_node_id_defer <- function(id, env = parent.frame()) {
  # store label in tempdir() to be removed later
  path_dir_rm <- file.path(tempdir(), "osf_node_to_remove")
  if (!dir.exists(path_dir_rm)) {
    dir.create(path_dir_rm, recursive = TRUE)
  }
  path_file_rm <- file.path(path_dir_rm, id)
  invisible(file.create(path_file_rm, showWarnings = FALSE))
  withr::defer(
    {
      try(.projr_osf_rm_node_id(id), silent = TRUE)
      eval(parse(text = paste0("unlink('", path_file_rm, "')")))
    },
    envir = env
  )
  invisible(TRUE)
}
