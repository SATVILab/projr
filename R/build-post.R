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
  cite_vec <- .projr_build_cite_get_yml()
  for (i in seq_along(cite_vec)) {
    switch(cite_vec[[i]],
      "cff" = .projr_cite_cff_set(),
      "codemeta" = .projr_cite_codemeta_set(),
      "inst-citation" = .projr_cite_citation_set()
    )
  }
  invisible(TRUE)
}

.projr_build_cite_get_yml <- function() {
  cite_vec <- .projr_yml_cite_get(NULL)
  if (is.null(cite_vec) || isTRUE(cite_vec)) {
    return(c("cff", "codemeta", "inst-citation"))
  } else if (isFALSE(cite_vec)) {
    return(character(0))
  } else {
    .assert_chr(cite_vec)
    cite_vec
  }
}

.projr_build_cite_cff <- function() {
  if (!file.exists(.dir_proj_get("CITATION.cff"))) {
    return(invsiible(FALSE))
  }
  .projr_build_cite_cff_update_file()
}

.projr_build_cite_cff_update_file <- function() {
  file_vec <- readLines(.dir_proj_get("CITATION.cff"))
  file_vec <- gsub(
    "^version::\\s*\"?[^\"]*\"",
    paste0("version: ", projr_version_get()),
    file_vec
  )
    # Update the preferred-citation version if it exists
  preferred_citation_index <- grep("^preferred-citation:", file_vec)[[1]]
  if (length(preferred_citation_index) > 0) {
    for (i in (preferred_citation_index + 1):length(file_vec)) {
      if (grepl("^\\s*version\\s*:\\s*\"?[^\"]*\"", file_vec[i])) {
        file_vec[i] <- gsub(
          "^\\s*version\\s*:\\s*\"?[^\"]*\"",
          paste0("  version: ", projr_version_get()),
          file_vec[i]
        )
        break
      } else if (!grepl("^\\s", file_vec[i])) {
        break
      }
    }
  }
  writeLines(file_vec, .dir_proj_get("CITATION.cff"))
}

.projr_build_cite_inst_citation <- function() {
  if (!file.exists(.dir_proj_get("inst", "CITATION"))) {
    return(invisible(FALSE))
  }
  .projr_build_cite_inst_citation_update_file()
}

.projr_build_cite_inst_citation_update_file <- function() {
  file_vec <- readLines(.dir_proj_get("inst", "CITATION"))
  file_vec <- gsub(
    "^\\s*version\\s*=\\s*\"[^\"]*\"(,?)",
    paste0("version = ", projr_version_get(), "\\1"),
    file_vec
  )
  writeLines(file_vec, .dir_proj_get("inst", "CITATION"))
  invisible(TRUE)
}

.projr_build_cite_codemeta <- function() {
  if (!file.exists(.dir_proj_get("codemeta.json"))) {
    return(invisible(FALSE))
  }
  .projr_build_cite_codemeta_update_file()
}

.projr_build_cite_codemeta_update_file <- function() {
  file_vec <- readLines(.dir_proj_get("codemeta.json"))
  file_vec <- gsub(
    "^\\s*\"version\"\\s*:\\s*\"[^\"]*\"(,?)",
    paste0("\"version\": \"", projr_version_get(), "\"\\1"),
    file_vec
  )
  writeLines(file_vec, .dir_proj_get("codemeta.json"))
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
