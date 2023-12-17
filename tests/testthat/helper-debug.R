.projr_test_debug_copy_projr_yml <- function() {
  path_dir_to <- file.path("/workspaces/projr/_tmp")
  .projr_dir_create(path_dir_to)
  file.copy(
    from = "_projr.yml",
    to = file.path(path_dir_to, "_projr.yml")
  )
}

.projr_test_debug_save_rds <- function(x) {
  nm <- deparse(substitute(x))
  path_dir_to <- file.path("/workspaces/projr/_tmp")
  .projr_dir_create(path_dir_to)
  saveRDS(x, file.path("/workspaces/projr/_tmp", paste0(nm, ".rds")))
}

.projr_test_debug_read_rds <- function(x) {
  nm <- deparse(substitute(x))
  nm <- gsub("\\.rds$", "", nm)
  nm <- paste0(nm, ".rds")
  path_dir_from <- file.path("/workspaces/projr/_tmp")
  readRDS(file.path(path_dir_from, nm))
}

.projr_test_debug_copy_file <- function(path) {
  file.copy(
    from = path,
    to = file.path("/workspaces/projr/_tmp", basename(path)),
    overwrite = TRUE
  )
}
