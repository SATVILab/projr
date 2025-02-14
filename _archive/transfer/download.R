#' @title Download a specific label's source file
projr_download_label <- function(label) {
  yml.dir_label <- .yml_get()[["directories"]][[label]]
  if ("osf" %in% names(yml.dir_label)) {
    return(.osf_download_label(label))
  }
}

#' @title Download all source files
#' @export
projr_download <- function() {
  yml.dir <- .yml_get()[["directories"]]
  for (label in names(yml.dir)) {
   .download_label(label)
  }
}
