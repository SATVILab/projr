.projr_cite_citation_set <- function() {
  path_inst_citation <- file.path("inst", "CITATION")
  .file_rm(path_inst_citation)
  .projr_cite_citation_inst_write()
  .projr_cite_citation_inst_add_header()
}

.projr_cite_citation_inst_write <- function() {
  cffr::write_citation(cffr::cff_create())
}

.projr_cite_citation_inst_add_header <- function() {
  path_citation_inst <- .dir_proj_get(
    "inst", "CITATION"
  ) # nolint: object_usage_linter.
  header_txt <- paste0(
    'citHeader("To cite `',
    .projr_pkg_nm_get(), # nolint: object_usage_linter.
    '` in publications use:")'
  )
  citation_vec_init <- readLines(path_citation_inst)
  citation_vec <- c(header_txt, citation_vec_init)
  writeLines(citation_vec, path_citation_inst)
}

.projr_cite_codemeta_set <- function() {
  codemeta::write_codemeta(path = .dir_proj_get())
}

.projr_cite_bibtex_get <- function() {
  bibtex_txt <- utils::toBibtex(
    cffr::cff_to_bibtex(cffr::cff_create())
  ) |>
    as.character()
  c(
    "",
    "## Citation",
    "",
    paste0(
      "To cite `",
      .projr_pkg_nm_get(),
      "` in publications use:"
    ),
    bibtex_txt
  )
}

.projr_cite_cff_set <- function() {
  cffr::cff_write()
}
