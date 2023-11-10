.projr_remote_complete_osf_category <- function(category) {
  if (!is.null(category)) category else "project"
}
.projr_remote_complete_osf_public <- function(public) {
  if (!is.null(public)) public else FALSE
}
