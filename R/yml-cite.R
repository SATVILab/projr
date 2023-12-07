# cite
# -----------------

.projr_yml_set_cite <- function(all_three = FALSE,
                                codemeta = FALSE,
                                cff = FALSE,
                                inst_citation = FALSE) {
  # Note: codemeta, cff and inst_citation
  # being FALSE does not set the corresponding yml
  # values to FALSE.
  # The function never sets such values to FALSE,
  # it's only for making such values TRUE.
  # So, FALSE just means "don't force to TRUE".

  # set all three
  set_all_three <- .projr_yml_set_cite_all_three(
    force_all_three = all_three,
    code_meta = codemeta,
    cff = cff,
    inst_citation = inst_citation
  )
  if (set_all_three) {
    return(invisible(TRUE))
  }
  .projr_yml_cite_add_ind(
    codemeta = codemeta,
    cff = cff,
    inst_citation = inst_citation
  )
}

.projr_yml_unset_cite <- function() {
  .projr_yml_set_cite(all_three = FALSE)
}

.projr_yml_cite_check_all_three <- function(force_all_three = FALSE,
                                            code_meta = FALSE,
                                            cff = FALSE,
                                            inst_citation = FALSE) {
  if (force_all_three) {
    return(TRUE)
  }
  yml_projr_cite <- .projr_yml_get_root_default()[["build"]][["cite"]]
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

.projr_yml_set_cite_all_three <- function(force_all_three,
                                          code_meta,
                                          cff,
                                          inst_citation) {
  all_three_lgl <- .projr_yml_cite_check_all_three(
    force_all_three = force_all_three,
    code_meta = code_meta,
    cff = cff,
    inst_citation = inst_citation
  )
  if (!all_three_lgl) {
    return(TRUE)
  }
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["cite"]] <- TRUE
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}

.projr_yml_cite_add_ind <- function(codemeta = FALSE,
                                    cff = FALSE,
                                    inst_citation = FALSE) {
  # setup
  yml_projr_root <- .projr_yml_get_root_default()

  yml_cite <- .projr_yml_cite_add_ind_yml(
    yml_cite = yml_projr_root[["build"]][["cite"]],
    codemeta = codemeta,
    cff = cff,
    inst_citation = inst_citation
  )

  if (isFALSE(yml_cite)) {
    yml_projr_root[["build"]] <- yml_projr_root[["build"]][
      -which(names(yml_projr_root[["build"]]) == "cite")
    ]
  } else {
    yml_projr_root[["build"]] <- yml_cite
  }

  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
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

.projr_yml_cite_get <- function(method) {
  yml_cite <- .projr_yml_get_root_default()[["build"]][["cite"]]
  cite_vec <- c("codemeta", "cff", "inst-citation")
  if (isTRUE(yml_cite)) {
    return(cite_vec)
  }
  if (is.null(yml_cite) || isFALSE(yml_cite)) {
    return(character())
  }
  cite_vec
}
