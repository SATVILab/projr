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
  .assert_count(n_row, required = TRUE)
  cat(utils::head(.changelog_read(), n_row))
}
