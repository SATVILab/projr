# roxygen
# --------------------
.build_roxygenise <- function(output_run) {
  if (!.build_roxygenise_check(output_run)) {
    return(invisible(FALSE))
  }
  .cli_step("Updating roxygen documentation")
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
  txt <- readLines(fn, warn = FALSE)
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
    switch(
      cite_vec[[i]],
      "cff" = .cite_cff_set(create = FALSE),
      "codemeta" = .cite_codemeta_set(create = FALSE),
      "inst-citation" = .cite_citation_set(create = FALSE)
    )
  }
  invisible(TRUE)
}

.build_cite_get_yml <- function(profile = NULL) {
  cite_vec <- .yml_cite_get(profile)

  if (isTRUE(cite_vec)) {
    return(c("cff", "codemeta", "inst-citation"))
  } else if (is.null(cite_vec) || isFALSE(cite_vec)) {
    return(character(0))
  } else if (is.list(cite_vec)) {
    return(names(cite_vec)[vapply(cite_vec, isTRUE, logical(1))])
  } else {
    .assert_chr(cite_vec)
    cite_vec
  }
}

.build_cite_cff <- function() {
  if (!file.exists(.path_get("CITATION.cff"))) {
    return(invisible(FALSE))
  }
  .build_cite_cff_update_file()
}

.build_cite_cff_update_file <- function() {
  file_vec <- readLines(.path_get("CITATION.cff"), warn = FALSE)
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
  file_vec <- readLines(.path_get("inst", "CITATION"), warn = FALSE)
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
  file_vec <- readLines(.path_get("codemeta.json"), warn = FALSE)
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
  .cli_step("Rendering README.Rmd")
  readme <- as.character(.path_get("README.Rmd"))
  if (!.build_readme_rmd_render_detect_pkg_use()) {
    return(.build_readme_rmd_render_impl(readme))
  }

  .dep_install_only("pkgbuild")
  # Use the project's new version even if the caller already loaded an older one.
  render <- .build_readme_rmd_render_impl
  environment(render) <- baseenv()
  invisible(xfun::Rscript_call(
    render,
    list(readme, .libPaths()),
    options = "--vanilla"
  ))
}

.build_readme_rmd_render_check <- function(output_run) {
  if ((!file.exists(.path_get("README.Rmd"))) || (!output_run)) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.build_readme_rmd_render_detect_pkg_use <- function() {
  description <- .path_get("DESCRIPTION")
  if (!file.exists(description)) {
    return(FALSE)
  }
  package <- read.dcf(description, fields = "Package")[[1L]]
  if (is.na(package) || !nzchar(package)) {
    return(FALSE)
  }
  package <- gsub(".", "[.]", package, fixed = TRUE)
  pattern <- paste0(
    "\\b",
    package,
    "::|\\b(library|require|requireNamespace)\\s*\\(\\s*",
    "(?:package\\s*=\\s*)?['\"]?",
    package,
    "['\"]?\\s*[,)]"
  )
  any(grepl(
    pattern,
    readLines(.path_get("README.Rmd"), warn = FALSE),
    perl = TRUE
  ))
}

.build_readme_rmd_render_impl <- function(readme, libraries = NULL) {
  if (!is.null(libraries)) {
    # R removes this private library and source archive when the child exits.
    library <- tempfile("projr-readme-")
    dir.create(library)
    .libPaths(c(library, libraries))
    Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
    archive <- pkgbuild::build(
      dirname(readme),
      dest_path = tempdir(),
      vignettes = FALSE,
      quiet = TRUE
    )
    args <- c("CMD", "INSTALL", "-l", shQuote(library), shQuote(archive))
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "R"),
      args,
      stdout = TRUE,
      stderr = TRUE
    ))
    if (!is.null(attr(output, "status"))) {
      stop(
        "Could not install the project package for README.Rmd:\n",
        paste(output, collapse = "\n"),
        call. = FALSE
      )
    }
  }
  rmarkdown::render(readme, output_format = "md_document", quiet = TRUE)
  invisible(TRUE)
}

# revert version upon failure
# --------------------
.build_version_set_post <- function(version_run_on_list, success) {
  if (success || is.null(version_run_on_list)) {
    return(invisible(FALSE))
  }
  projr_version_set(version_run_on_list$desc[["failure"]])
  invisible(TRUE)
}
