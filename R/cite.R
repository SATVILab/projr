.cite_citation_set <- function(create = TRUE) {
  path_inst_citation <- file.path("inst", "CITATION")
  if (!file.exists(path_inst_citation) && !create) {
    return(invisible(FALSE))
  }
  message("Creating CITATION file")
  .file_rm(path_inst_citation)
  .cite_citation_inst_write()
  .cite_citation_inst_add_header()
  invisible(TRUE)
}

.cite_citation_inst_write <- function() {
  .dep_install("cffr")
  cffr::cff_write_citation(
    cffr::cff_create(),
    file = .path_get("inst", "CITATION")
  )
}

.cite_citation_inst_add_header <- function() {
  path_citation_inst <- .path_get("inst", "CITATION") # nolint object_usage_linter.
  header_txt <- paste0(
    'citHeader("To cite `',
    .pkg_nm_get(), # nolint: object_usage_linter.
    '` in publications use:")'
  )
  citation_vec_init <- readLines(path_citation_inst)
  citation_vec <- c(header_txt, citation_vec_init)
  writeLines(citation_vec, path_citation_inst)
}

.cite_codemeta_set <- function(create = TRUE) {
  .dep_install("codemeta")
  if (!file.exists(.path_get("codemeta.json")) && !create) {
    return(invisible(FALSE))
  }
  message("Creating codemeta.json file")
  try(codemeta::write_codemeta(path = .path_get()))
  invisible(TRUE)
}

.cite_bibtex_get <- function() {
  .dep_install("cffr")
  bibtex_txt <- utils::toBibtex(
    cffr::as_bibentry(cffr::cff_create())
  ) |>
    as.character()
  c(
    "",
    "## Citation",
    "",
    paste0(
      "To cite `",
      .pkg_nm_get(),
      "` in publications use:"
    ),
    bibtex_txt
  )
}

.cite_cff_set <- function(create = TRUE) {
  .dep_install("cffr")
  if (!file.exists(.path_get("CITATION.cff")) && !create) {
    return(invisible(FALSE))
  }
  message("Creating CITATION.cff file")
  try(cffr::cff_write())
  invisible(TRUE)
}
