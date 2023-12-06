#' @rdname cat
#' @title Cat useful information
#' @description
#'  Cat useful information.
#' `projr_cat_changelog`:
#' @param n_row integer.
#' Number of rows to cat.
#' @export
projr_cat_changelog <- function(n_row = 10) {
  .projr_changelog_read()[seq_len(n_row)] |> cat()
}
