.init_engine <- function(nm_list) {
  if (.init_engine_check_exists()) {
    return(invisible(FALSE))
  }
  switch(nm_list[["engine"]],
    "bookdown" = .init_engine_bookdown(nm_list),
    "quarto_project" = .init_engine_quarto_project(nm_list),
    "quarto_document" = .init_engine_quarto_document(nm_list),
    "rmd" = .init_engine_rmd(nm_list),
    stop("Document engine not recognised")
  )
}

.init_engine_check_exists <- function() {
  bookdown_exists_lgl <- file.exists(.path_get("_bookdown.yml"))
  quarto_project_exists_lgl <- file.exists(
    .path_get("_quarto.yml")
  )
  qmd_rmd_exists_lgl <- any(grepl(
    "\\.qmd|\\.Rmd|\\.rmd",
    list.files(.path_get())
  ))
  bookdown_exists_lgl || quarto_project_exists_lgl || qmd_rmd_exists_lgl
}

# bookdown
# ------------------------
.init_engine_bookdown <- function(nm_list) {
  .dep_install("bookdown")
  .init_engine_bookdown_bookdown()
  .init_engine_bookdown_output(nm_list)
  .init_engine_bookdown_index(nm_list)
}

.init_engine_bookdown_bookdown <- function() {
  yml_bd <- yaml::read_yaml(system.file(
    "project_structure", "_bookdown.yml",
    package = "projr"
  ))
  .yml_bd_set(yml_bd)
}

.init_engine_bookdown_output <- function(nm_list) {
  # output.yml
  o_yml <- yaml::read_yaml(system.file(
    "project_structure", "_output.yml",
    package = "projr"
  ))

  o_yml$`bookdown::gitbook`$config$toc$after <- paste0(
    '<li><a href="https://github.com/',
    nm_list[["gh"]], "/",
    nm_list[["pkg"]],
    '" target="blank">',
    nm_list[["gh"]], "/",
    nm_list[["pkg"]],
    "</a></li>\n"
  )

  o_yml$`bookdown::gitbook`$config$toc$before <-
    paste0(
      '<li><a href=\"./\">',
      nm_list[["title"]],
      "</a></li>\n"
    )
  path_yml <- .path_get("_output.yml")
  yaml::write_yaml(o_yml, path_yml)
  .newline_append(path_yml)

  invisible(TRUE)
}

.init_engine_bookdown_index <- function(nm_list) {
  # index.Rmd
  index <- readLines(system.file(
    "project_structure", "index.Rmd",
    package = "projr"
  ), warn = FALSE)
  index[2] <- paste0("title: ", nm_list[["pkg"]])
  author_ind <- which(grepl("^author", index))
  # last name
  nm_author <- paste0("author: ", nm_list[["first"]], " ", nm_list[["last"]])
  index[author_ind] <- nm_author
  description_ind <- which(grepl("^description", index))
  index[description_ind] <- paste0("description: ", nm_list[["title"]])
  writeLines(index, .path_get("index.Rmd"))
  invisible(TRUE)
}

# quarto project
# ---------------------

.init_engine_quarto_project <- function(nm_list) {
  .dep_install("quarto")
  .init_engine_quarto_project_yml(nm_list)
  .init_engine_quarto_project_index()
}

.init_engine_quarto_project_yml <- function(nm_list) {
  yml_quarto <- switch(nm_list[["format"]],
    "book" = list(
      "project" = list("type" = "book"),
      "book" = list(
        "title" = nm_list[["title"]],
        "author" = paste0(
          "author: ", nm_list[["first"]], " ", nm_list[["last"]]
        ),
        "date" = format(Sys.time(), "%Y/%m/%d"),
        "chapters" = list("index.qmd")
      ),
      "format" = list(
        "html" = list("theme" = "cosmo"),
        "pdf" = list("documentclass" = "scrreprt")
      )
    ),
    "website" = list(
      project = list(type = "website"),
      website = list(
        title = nm_list[["title"]],
        navbar = list(
          left = list(
            list(href = "index.qmd", text = "Home")
          )
        )
      ),
      format = list(
        html = list(
          theme = "cosmo",
          toc = TRUE
        )
      )
    )
  )
  .yml_quarto_set(yml_quarto)
  invisible(TRUE)
}

.init_engine_quarto_project_index <- function() {
  # index.Rmd
  c("# Introduction", "") |>
    writeLines(.path_get("index.qmd"))
  invisible(TRUE)
}

# quarto document
# ---------------------

.init_engine_quarto_document <- function(nm_list) {
  .dep_install("quarto")
  .init_engine_quarto_document_doc(nm_list)
}

.init_engine_quarto_document_doc <- function(nm_list) {
  # index.Rmd
  format <- switch(nm_list[["format"]],
    "word" = "docx",
    "powerpoint" = "pptx",
    nm_list[["format"]]
  )
  txt <- c(
    "---",
    paste0("title: ", nm_list[["title"]]),
    "format:",
    paste0("  ", format, ":"),
    "    embed-resources: true",
    "---",
    "",
    "# Introduction",
    ""
  )
  path_qmd <- .path_get(
    paste0(
      gsub("\\.qmd$", "", nm_list[["filename"]]),
      ".qmd"
    )
  )
  writeLines(txt, path_qmd)
  invisible(TRUE)
}

# Rmd document
# ---------------------

.init_engine_rmd <- function(nm_list) {
  .dep_install("rmarkdown")
  .init_engine_rmd_doc(nm_list)
}

.init_engine_rmd_doc <- function(nm_list) {
  # index.Rmd
  format <- switch(nm_list[["format"]],
    "html" = ,
    "pdf" = ,
    "word" = paste0(nm_list[["format"]], "_document"),
    "beamer" = ,
    "ioslides" = ,
    "slidy" = ,
    "powerpoint" = paste0(nm_list[["format"]], "_presentation"),
    "revealjs" = "revealjs::revealjs_presentation",
    nm_list[["format"]]
  )
  txt <- c(
    "---",
    paste0("title: ", nm_list[["title"]]),
    paste0("output: ", format),
    "---",
    "",
    "# Introduction",
    ""
  )
  path_rmd <- .path_get(
    paste0(
      gsub("\\.Rmd|\\.rmd$", "", nm_list[["filename"]]),
      ".Rmd"
    )
  )
  writeLines(txt, path_rmd)
  invisible(TRUE)
}
