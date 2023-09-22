.projr_osf_filter_dir <- function(x) {
  x[.projr_osf_is_dir(x), ]
}
.projr_osf_is_dir <- function(x) {
  vapply(seq_len(nrow(x)), function(i) {
    x$meta[[i]][["attributes"]][["kind"]] == "folder"
  }, logical(1))
}
