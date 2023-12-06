#' @rdname cat
#' @title Cat useful information
#' @description
#'  Cat useful information.
#' `projr_cat_changelog`:
#' @export
projr_cat_changelog <- function() {
  .projr_changelog_read() |> cat()
}
