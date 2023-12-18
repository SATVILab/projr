#' @title Download a specific label's source file
projr_download_label <- function(label) {
  yml_projr_dir_label <- projr_yml_get_unchecked()[["directories"]][[label]]
  if ("osf" %in% names(yml_projr_dir_label)) {
    return(.projr_osf_download_label(label))
  }
}

#' @title Download all source files
#' @export
projr_download <- function() {
  yml_projr_dir <- projr_yml_get_unchecked()[["directories"]]
  for (label in names(yml_projr_dir)) {
    projr_download_label(label)
  }
}
