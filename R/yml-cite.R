# cite
# -----------------

#' @rdname yml-cite
#' @title Set citation options
#' @export
#'
#' @description
#' Set citation options for the project.
#' The options are:
#' \itemize{
#'  \item{codemeta}{Whether to generate a codemeta.json file.}
#' \item{cff}{Whether to generate a CITATION.cff file.}
#' \item{inst-citation}{Whether to generate a CITATION file in the inst/ directory.}
#' }
#' The default is to leave all the settings unchanged.
#'
#' @param all_three logical.
#' Whether to generate all three files.
#' If `NULL`, then `codemeta`, `cff` and `inst_citation` are used.
#' @param codemeta logical.
#' Whether to generate a codemeta.json file.
#' If `NULL`, then setting is not changed.
#' @param cff logical.
#' Whether to generate a CITATION.cff file.
#' If `NULL`, then setting is not changed.
#' @param inst_citation logical.
#' Whether to generate a CITATION file in the inst/ directory.
#' If `NULL`, then setting is not changed.
#' @param profile character.
#' Profile to add the script to.
#' If `"default"`` (the default),
#' the script is added to the default profile,
#' which is `_projr.yml`.
#'
#'
projr_yml_cite_set <- function(all_three = NULL,
                               codemeta = NULL,
                               cff = NULL,
                               inst_citation = NULL,
                               profile = "default") {
  # Note: codemeta, cff and inst_citation
  # being FALSE does not set the corresponding yml
  # values to FALSE.
  # The function never sets such values to FALSE,
  # it's only for making such values TRUE.
  # So, FALSE just means "don't force to TRUE".

  all_three <- all_three %||% FALSE
  codemeta <- codemeta %||% FALSE
  cff <- cff %||% FALSE
  inst_citation <- inst_citation %||% FALSE

  # set all three
  set_all_three <- .projr_yml_cite_set_all_three(
    force_all_three = all_three,
    code_meta = codemeta,
    cff = cff,
    inst_citation = inst_citation,
    profile = profile
  )
  if (set_all_three) {
    return(invisible(TRUE))
  }
  .projr_yml_cite_add_ind(
    codemeta = codemeta,
    cff = cff,
    inst_citation = inst_citation,
    profile = profile
  )
}

.projr_yml_cite_unset <- function(profile) {
  .projr_yml_cite_set(NULL, profile)
}

.projr_yml_cite_check_all_three <- function(force_all_three = FALSE,
                                            code_meta = FALSE,
                                            cff = FALSE,
                                            inst_citation = FALSE,
                                            profile) {
  if (.projr_yml_cite_check_all_three_direct(force_all_three)) {
    return(TRUE)
  }
  yml_cite <- .projr_yml_cite_get(profile)
  missing_vec <- setdiff(
    c("codemeta", "cff", "inst-citation"),
    c(
      yml_projr_cite,
      "code_meta"[code_meta],
      "cff"[cff],
      "inst_citation"[inst_citation]
    )
  )
  identical(missing_vec, character())
}

.projr_yml_cite_check_all_three_direct <- function(force_all_three) {
  if (.projr_state_null(force_all_three)) {
    return(FALSE)
  }
  .projr_check_lgl_single(force_all_three, "force_all_three")
  force_all_three
}

.projr_yml_cite_check_all_three_implied <- function(code_meta,
                                                    cff,
                                                    inst_citation) {
  any_null <- any(.projr_state_null(c(code_meta, cff, inst_citation)))
  if (any_null) {
    return(FALSE)
  }
  all(c(code_meta, cff, inst_citation))
}

.projr_yml_cite_set_all_three <- function(force_all_three,
                                          code_meta,
                                          cff,
                                          inst_citation,
                                          profile) {
  all_three_lgl <- .projr_yml_cite_check_all_three(
    force_all_three = force_all_three,
    code_meta = code_meta,
    cff = cff,
    inst_citation = inst_citation,
    profile = profile
  )
  if (!all_three_lgl) {
    return(TRUE)
  }
  .projr_yml_cite_set(TRUE, profile)
}

.projr_yml_cite_add_ind <- function(codemeta = FALSE,
                                    cff = FALSE,
                                    inst_citation = FALSE,
                                    profile) {
  yml_cite <- .projr_yml_cite_add_ind_yml(
    yml_cite = yml_projr_root[["build"]][["cite"]],
    codemeta = codemeta,
    cff = cff,
    inst_citation = inst_citation,
    profile = profile
  )

  if (isFALSE(yml_cite)) {
    .projr_yml_cite_unset(profile)
  }
  .projr_yml_cite_set(yml_cite, profile)
}

.projr_yml_cite_add_ind_yml <- function(yml_cite = character(),
                                        codemeta = FALSE,
                                        cff = FALSE,
                                        inst_citation = FALSE) {
  if (isTRUE(yml_cite)) {
    return(invisible(TRUE))
  }
  if (isFALSE(yml_cite)) {
    yml_cite <- character()
  }
  yml_cite <- yml_cite |>
    c("codemeta"[codemeta], "cff"[cff], "inst-citation"[inst_citation]) |>
    unique()
  if (length(setdiff(c("codemeta", "cff", "inst-citation"), yml_cite)) == 0L) {
    yml_cite <- TRUE
  }
  if (length(yml_cite) == 0L) {
    yml_cite <- FALSE
  }
  yml_cite
}

.projr_yml_cite_get <- function(method, profile) {
  yml_cite <- projr_yml_get_unchecked(profile)[["build"]][["script"]]
  cite_vec <- c("codemeta", "cff", "inst-citation")
  if (isTRUE(yml_cite)) {
    return(cite_vec)
  }
  if (.projr_state_null(yml_cite) || isFALSE(yml_cite)) {
    return(character())
  }
  cite_vec
}

.projr_yml_cite_set <- function(yml_cite, profile = NULL) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["build"]][["cite"]] <- yml_cite
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_cite_complete <- function(yml_cite) {
  for (x in c("codemeta", "cff", "inst-citation")) {
    yml_cite[[x]] <- .projr_yml_cite_complete_nm(
      yml_cite = yml_cite, nm = x, val = TRUE
    )
  }
  yml_cite
}

.projr_yml_cite_complete_nm <- function(yml_cite, nm, val) {
  .projr_check_chr_single(nm, "nm", required = TRUE)
  if (nm %in% names(yml_cite)) {
    return(yml_cite[[nm]])
  }
  val
}
