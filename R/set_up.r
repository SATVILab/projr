#' @param dir_proj character. Path to directory for project.
#' If it exists, it is used. If it does not exist, it is created
#' so long as the parent path exists.
#' @param fn_bib character vector.
#' Path to bibliography file(s) relative to 
#' \code{dir_proj}, e.g. if \code{fn_bib} = `bib/references.bib`, then
#' the \code{.bib} files will be in \code{<dir_proj>/bib/references.bib}.
#' Parent directories are created automatically.
#' Default is \code{bib/references.bib}.
#' 
setup_proj2pkg <- function(dir_proj,
                           fn_bib = "bib/references.bib",
                           config = list(
                             dir_cache = "here::here(''.cache')",
                             dir_data_raw = "",
                             dir_manual = ""
                           ),
                           dir_rmd) {
  if (!dir.exists(dir_proj)) {
    if (!dir.exists(dirname(dir_proj))) {
      stop("Neither path nor its parent directory exist.")
    } else {
      dir.create(dir_proj)
    }
  }
  usethis::create_package(path = dir_proj)

  # create `.bib` folder
  fn_bib_full <- file.path(
    dir_proj, bib
  )
  for (i in seq_along(fn_bib)) {
    fn_bib_full <- file.path(
      dir_proj, fn_bib[i]
    )
    if (!dir.exists(dirname(fn_bib_full))) {
      dir.create(fn_bib_full, recursive = TRUE)
    }
    file.create(fn_bib_full)
  }
  # 
  ?renv::init()
}

set_up_package <- function(dir_proj) {
  usethis::create_package(path = dir_proj)

  # add view_report function
}

set_up_project = function(dir_proj) {

}