.cite_citation_set <- function() {
  path_inst_citation <- file.path("inst", "CITATION")
  .file_rm(path_inst_citation)
  .cite_citation_inst_write()
  .cite_citation_inst_add_header()
}

.cite_citation_inst_write <- function() {
  .dep_install("cffr")
  cffr::cff_write_citation(
    cffr::cff_create(),
    file = .path_get("inst", "CITATION")
  )
}

.cite_citation_inst_add_header <- function() {
  path_citation_inst <- .path_get("inst", "CITATION") # nolint: object_usage_linter.
  header_txt <- paste0(
    'citHeader("To cite `',
    .pkg_nm_get(), # nolint: object_usage_linter.
    '` in publications use:")'
  )
  citation_vec_init <- readLines(path_citation_inst)
  citation_vec <- c(header_txt, citation_vec_init)
  writeLines(citation_vec, path_citation_inst)
}

.cite_codemeta_set <- function() {
  .dep_install("codemeta")
  try(codemeta::write_codemeta(path = .path_get()))
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

.cite_cff_set <- function() {
  .dep_install("cffr")
  try(cffr::cff_write())
}
