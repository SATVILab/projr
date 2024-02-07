# roxygen
# --------------------
.projr_build_roxygenise <- function(output_run) {
  if (!.projr_build_roxygenise_check(output_run)) {
    return(invisible(FALSE))
  }
  .projr_dep_install_only("roxygen2")
  suppressMessages(suppressWarnings(invisible(
    roxygen2::roxygenise(package.dir = .dir_proj_get())
  )))
  invisible(TRUE)
}

.projr_build_roxygenise_check <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  if (!dir.exists("R")) {
    return(invisible(FALSE))
  }
  fn_vec <- list.files("R", full.names = TRUE)
  for (i in seq_along(fn_vec)) {
    if (.projr_build_roxygen_check_fn(fn_vec[[i]])) {
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

.projr_build_roxygen_check_fn <- function(fn) {
  if (!file.exists(fn)) {
    return(invisible(FALSE))
  }
  txt <- readLines(fn)
  if (length(txt) == 0) {
    return(invisible(FALSE))
  }
  any(grepl("^#'\\s+@", txt))
}

# citation
# --------------------
.projr_build_cite <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  cite_vec <- .projr_yml_cite_get(NULL)
  for (i in seq_along(cite_vec)) {
    switch(cite_vec[[i]],
      "cff" = .projr_cite_cff_set(),
      "codemeta" = .projr_cite_codemeta_set(),
      "inst-citation" = .projr_cite_citation_set()
    )
  }
  invisible(TRUE)
}

# readme
# --------------------
.projr_build_readme_rmd_render <- function(output_run) {
  if (!.projr_build_readme_rmd_render_check(output_run)) {
    return(invisible(FALSE))
  }
  .projr_build_readme_rmd_render_add_devtools_if_needed()
  .projr_build_readme_rmd_render_actual()
}

.projr_build_readme_rmd_render_check <- function(output_run) {
  if ((!file.exists(.dir_proj_get("README.Rmd"))) || (!output_run)) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_build_readme_rmd_render_detect_pkg_use <- function() {
  readme_rmd <- readLines(.dir_proj_get("README.Rmd"))
  pkg_use_detected_lib <- grepl(
    paste0(
      "library\\(", projr_name_get(), "\\)|",
      'library\\("', projr_name_get(), '"\\)|',
      "library\\('", projr_name_get(), "'\\)"
    ),
    readme_rmd
  ) |>
    any()
  pkg_use_detected_dots <- grepl(
    paste0(projr_name_get(), "::"),
    readme_rmd
  ) |>
    any()
  pkg_use_detected_lib | pkg_use_detected_dots
}

.projr_build_readme_rmd_render_add_devtools_if_needed <- function() {
  if (.projr_build_readme_rmd_render_detect_pkg_use()) {
    .projr_dep_add("devtools")
    devtools::install()
  }
}

.projr_build_readme_rmd_render_actual <- function() {
  rmarkdown::render(
    .dir_proj_get("README.Rmd"),
    output_format = "md_document", quiet = TRUE
  )
  invisible(TRUE)
}

# revert version upon failure
# --------------------
.projr_build_version_set_post <- function(version_run_on_list,
                                          success) {
  if (success) {
    return(invisible(FALSE))
  }
  projr_version_set(version_run_on_list$desc[["failure"]])
  invisible(TRUE)
}
