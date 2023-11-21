.projr_test_debug_copy_projr_yml <- function() {
  path_dir_to <- file.path("/workspaces/projr/_tmp")
  if (!dir.exists(path_dir_to)) {
    dir.create(path_dir_to, recursive = TRUE)
  }
  file.copy(
    from = "_projr.yml",
    to = file.path(path_dir_to, "_projr.yml")
  )
}

.projr_test_debug_save_rds <- function(x) {
  nm <- deparse(substitute(x))
  path_dir_to <- file.path("/workspaces/projr/_tmp")
  if (!dir.exists(path_dir_to)) {
    dir.create(path_dir_to, recursive = TRUE)
  }
  saveRDS(x, file.path("/workspaces/projr/_tmp", paste0(nm, ".rds")))
}

.projr_test_debug_read_rds <- function(x) {
  nm <- deparse(substitute(x))
  nm <- gsub("\\.rds$", "", nm)
  nm <- paste0(nm, ".rds")
  path_dir_from <- file.path("/workspaces/projr/_tmp")
  readRDS(file.path(path_dir_from, nm))
}
