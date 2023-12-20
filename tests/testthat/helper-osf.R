.projr_osf_label_get_random <- function(prefix) {
  label <- paste0(
    prefix, rnorm(1) |> signif(15) |> as.character()
  )
  # add it to yml_projr
  yml_projr <- .projr_yml_get()
  yml_projr[["directories"]][[label]] <- list(
    path = "_some_random_path"
  )
  .projr_yml_set(yml_projr)
  label
}

.projr_test_osf_create_project <- function(prefix) {
  project <- paste0(
    prefix, "ProjrOSFTest", rnorm(1) |> signif(6) |> as.character()
  )
  .projr_remote_create(
    type = "osf",
    name = project,
    category = "project",
    public = FALSE,
    id_parent = NULL
  )
}

.projr_osf_rm_node_id_defer <- function(id, env = parent.frame()) {
  # store label in tempdir() to be removed later
  path_file_rm <- file.path(.projr_test_osf_remote_dir_get_tmp(), id)
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
.projr_remote_host_rm_all_osf <- function() {
  osf_tbl <- osfr::osf_retrieve_user("me") |>
    osfr::osf_ls_nodes(n_max = Inf)
  ind_vec <- grepl("ProjrOSFTest", osf_tbl[["name"]])
  id_vec <- osf_tbl[ind_vec, ][["id"]]
  name_vec <- osf_tbl[ind_vec, ][["name"]]
  if (length(name_vec) == 0L) {
    message("No OSF projects to delete.")
    return(invisible(FALSE))
  }
  cat(name_vec, sep = "\n")
  opt_vec <- c("Yes", "No", "Definitely not")[sample(1:3, size = 3)]
  yes_ind <- which(opt_vec == "Yes")
  delete_opt <- utils::menu(
    choices = opt_vec,
    title = "Do you want to delete all the above OSF projects?"
  )
  if (delete_opt != yes_ind) {
    return(invisible(FALSE))
  }
  if (length(name_vec) > 5) {
    cat(name_vec, sep = "\n")
    opt_vec <- c("Yes", "No", "Actually - no")[sample(1:3, size = 3)]
    yes_ind <- which(opt_vec == "Yes")
    delete_opt <- utils::menu(
      choices = opt_vec,
      title = "Are you SURE you want to delete all the above OSF projects?"
    )
    if (delete_opt != yes_ind) {
      return(invisible(FALSE))
    }
  }
  for (id in id_vec) {
    .projr_remote_host_rm("osf", host = id)
  }
}

.projr_test_osf_remote_dir_get_tmp <- function() {
  path_dir <- file.path(tempdir(), "osf_node_to_remove")
  .dir_create(path_dir)
  path_dir
}
