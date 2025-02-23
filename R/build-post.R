# roxygen
# --------------------
.build_roxygenise <- function(output_run) {
  if (!.build_roxygenise_check(output_run)) {
    return(invisible(FALSE))
  }
  .dep_install_only("roxygen2")
  suppressMessages(suppressWarnings(invisible(
    roxygen2::roxygenise(package.dir = .path_get())
  )))
  invisible(TRUE)
}

.build_roxygenise_check <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  if (!dir.exists("R")) {
    return(invisible(FALSE))
  }
  fn_vec <- list.files("R", full.names = TRUE)
  for (i in seq_along(fn_vec)) {
    if (.build_roxygen_check_fn(fn_vec[[i]])) {
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

.build_roxygen_check_fn <- function(fn) {
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
.build_cite <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  cite_vec <- .build_cite_get_yml()
  for (i in seq_along(cite_vec)) {
    switch(cite_vec[[i]],
      "cff" = .cite_cff_set(create = FALSE),
      "codemeta" = .cite_codemeta_set(create = FALSE),
      "inst-citation" = .cite_citation_set(create = FALSE)
    )
  }
  invisible(TRUE)
}

.build_cite_get_yml <- function() {
  cite_vec <- .yml_cite_get(NULL)
  if (is.null(cite_vec) || isTRUE(cite_vec)) {
    return(c("cff", "codemeta", "inst-citation"))
  } else if (isFALSE(cite_vec)) {
    return(character(0))
  } else {
    .assert_chr(cite_vec)
    cite_vec
  }
}

.build_cite_cff <- function() {
  if (!file.exists(.path_get("CITATION.cff"))) {
    return(invsiible(FALSE))
  }
  .build_cite_cff_update_file()
}

.build_cite_cff_update_file <- function() {
  file_vec <- readLines(.path_get("CITATION.cff"))
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
  writeLines(file_vec, .path_get("CITATION.cff"))
}

.build_cite_inst_citation <- function() {
  if (!file.exists(.path_get("inst", "CITATION"))) {
    return(invisible(FALSE))
  }
  .build_cite_inst_citation_update_file()
}

.build_cite_inst_citation_update_file <- function() {
  file_vec <- readLines(.path_get("inst", "CITATION"))
  file_vec <- gsub(
    "^\\s*version\\s*=\\s*\"[^\"]*\"(,?)",
    paste0("version = ", projr_version_get(), "\\1"),
    file_vec
  )
  writeLines(file_vec, .path_get("inst", "CITATION"))
  invisible(TRUE)
}

.build_cite_codemeta <- function() {
  if (!file.exists(.path_get("codemeta.json"))) {
    return(invisible(FALSE))
  }
  .build_cite_codemeta_update_file()
}

.build_cite_codemeta_update_file <- function() {
  file_vec <- readLines(.path_get("codemeta.json"))
  file_vec <- gsub(
    "^\\s*\"version\"\\s*:\\s*\"[^\"]*\"(,?)",
    paste0("\"version\": \"", projr_version_get(), "\"\\1"),
    file_vec
  )
  writeLines(file_vec, .path_get("codemeta.json"))
  invisible(TRUE)
}



# readme
# --------------------
.build_readme_rmd_render <- function(output_run) {
  if (!.build_readme_rmd_render_check(output_run)) {
    return(invisible(FALSE))
  }
  .build_readme_rmd_render_add_devtools_if_needed()
  .build_readme_rmd_render_impl()
}

.build_readme_rmd_render_check <- function(output_run) {
  if ((!file.exists(.path_get("README.Rmd"))) || (!output_run)) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.build_readme_rmd_render_detect_pkg_use <- function() {
  readme_rmd <- readLines(.path_get("README.Rmd"))
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

.build_readme_rmd_render_add_devtools_if_needed <- function() {
  if (.build_readme_rmd_render_detect_pkg_use()) {
    .dep_add("devtools")
    devtools::install()
  }
}

.build_readme_rmd_render_impl <- function() {
  rmarkdown::render(
    .path_get("README.Rmd"),
    output_format = "md_document", quiet = TRUE
  )
  invisible(TRUE)
}

# revert version upon failure
# --------------------
.build_version_set_post <- function(version_run_on_list,
                                    success) {
  if (success) {
    return(invisible(FALSE))
  }
  projr_version_set(version_run_on_list$desc[["failure"]])
  invisible(TRUE)
}
