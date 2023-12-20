#' @title Get project parameters
projr_par_get <- function(...) {
  yml_projr_param <- .projr_par_get_list()
  if (is.null(yml_projr_param)) {
    return(NULL)
  }
  par_vec <- list(...) |> unlist()
  if (length(par_vec) == 0) {
    return(yml_projr_param)
  }
  .projr_par_get_option(yml_projr_param, par_vec)
}

.projr_par_get_list <- function() {
  yml_projr <- .projr_yml_get(NULL)
  par_nm <- names(yml_projr)[grepl("^par", names(yml_projr))][[1]]
  yml_projr[[par_nm]]
}

.projr_par_get_option <- function(par_list, par_vec) {
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
