#' @title Get project parameters
#'
#' @description
#' Get project parameters from `param` key
#' in `projr` configuration.
#'
#' @param ... character.
#' Sequential names to specify path in list.
#' For example, .param_get("a", "b")`
#' returns the value of `projr$param$a$b`.
#' @param profile character.
#' If `NULL`, then the active profile is used.
#' Default is `NULL`.
#' @export
projr_par_get <- function(..., profile = NULL) {
  yml.param <- .par_get_list(profile)
  if (is.null(yml.param)) {
    return(NULL)
  }
  par_vec <- list(...) |> unlist()
  if (length(par_vec) == 0) {
    return(yml.param)
  }
  .par_get_option(yml.param, par_vec)
}

.par_get_list <- function(profile) {
  yml_projr <- .yml_get(profile)
  par_vec_in <- which(par_nm_vec %in% names(yml_projr))
  if (length(par_vec_in) > 1) {
   stop(paste0(
     "Multiple `parameters` keys found in `projr` configuration: ",
     paste0(par_nm_vec[par_vec_in], collapse = ", ")
   ))
  }
  yml_projr[[par_nm_vec[par_vec_in]]]
}

.par_get_option <- function(par_list, par_vec) {
  parse_txt <- paste0(
    "par_list[['",
    paste0(par_vec, sep = "", collapse = "']][['"),
    "']]",
    collapse = ""
  )
  eval(parse(text = parse_txt))
}

#' @rdname projr_par_get
#' @export
projr_param_get <- projr_par_get

#' @title Add the `parameters` key
#'
#' @description
#' Add the `parameters` key to the `projr` configuration.
#'
#' @param profile character.
#' If `NULL`, then the default profile is used.
#' Default is `"default"`.
#' @export
projr_yml_par_add <- function(profile = "default") {
  profile <- if (is.null(profile)) "default" else profile
  .assert_chr(profile, TRUE)
  .yml_par_add_empty(profile)
}

.yml_par_add_empty <- function(profile) {
  yml_projr <- .yml_get(profile)
  if (!.yml_par_add_empty_check(yml_projr)) {
    return(invisible(FALSE))
  }
  yml_projr[["parameters"]] <- list()
  .yml_order(yml_projr) |>
    .yml_set(profile)
  invisible(TRUE)
}
.yml_par_add_empty_check <- function(yml) {
  !any(par_nm_vec %in% names(yml))
}
