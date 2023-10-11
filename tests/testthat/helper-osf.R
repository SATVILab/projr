.projr_osf_label_get_random <- function(prefix) {
  label <- paste0(
    prefix, rnorm(1) |> signif(15) |> as.character()
  )
  # add it to yml_projr
  yml_projr <- projr_yml_get_unchecked()
  yml_projr[["directories"]][[label]] <- list(
    path = "_some_random_path"
  )
  .projr_yml_set(yml_projr)
  label
}

.projr_test_osf_create_project <- function(prefix) {
  project <- paste0(
    prefix, "ProjrOSFTest", rnorm(1) |> signif(4) |> as.character()
  )
  .projr_osf_get_node(
    title = project,
    category = "project",
    public = FALSE,
    parent_id = NULL
  )
}

.projr_osf_rm_node_id_defer <- function(id, env = parent.frame()) {
  # store label in tempdir() to be removed later
  path_file_rm <- file.path(temp_path_dir_osf_rm, id)
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

# NUCLEAR OPTION - USE WITH CAUTION BUT IT
# SHOULD WORK JUST FINE.
# DO NOT CHANGE WITHOUT THINKING HARD ABOUT
# WHAT YOU ARE DOING.
.projr_osf_rm_project_all <- function() {
  osf_tbl <- osfr::osf_retrieve_user("me") |>
    osfr::osf_ls_nodes()
  id_vec <- osf_tbl[grepl("ProjrOSFTest", osf_tbl[["name"]]), ][["id"]]
  if (length(id_vec) > 5) {
    stop(paste0("Potentially deleting many things"))
  }
  for (id in id_vec) {
    osfr::osf_rm(x = osfr::osf_retrieve_node(id), check = FALSE, recurse = TRUE)
  }
}
