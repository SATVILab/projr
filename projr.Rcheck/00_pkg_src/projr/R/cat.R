#' @rdname cat
#' @title Cat useful information
#' @description
#'  Cat useful information.
#' .cat_changelog`:
#' @param n_row integer.
#' Number of rows to cat.
#' @export
projr_cat_changelog <- function(n_row = 10) {
  n_row <- n_row %||% 10
  .changelog_read()[seq_len(max(n_row, 1))] |> cat()
}
