.projr_init_engine <- function(nm_list) {
  switch(nm_list[["engine"]],
    "bookdown" = .projr_init_engine_bookdown(nm_list),
    "quarto_project" = .projr_init_engine_quarto_project(nm_list),
    "quarto_document" = .projr_init_engine_quarto_document(nm_list),
    "rmd_document" = .projr_init_engine_rmd(nm_list),
  )
}

# bookdown
# ------------------------
.projr_init_engine_bookdown <- function(nm_list) {
  .projr_init_engine_bookdown_bookdown()
  .projr_init_engine_bookdown_output(nm_list)
  .projr_init_engine_bookdown_index(nm_list)
}

.projr_init_engine_bookdown_bookdown <- function() {
  yml_bd <- yaml::read_yaml(system.file(
    "project_structure", "_bookdown.yml",
    package = "projr"
  ))
  .projr_yml_bd_set(yml_bd)
}

.projr_init_engine_bookdown_output <- function(nm_list) {
  dir_proj <- rprojroot::is_r_package$find_file()
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
  yaml::write_yaml(o_yml, file.path(dir_proj, "_output.yml"))
  invisible(TRUE)
}

.projr_init_engine_bookdown_index <- function(nm_list) {
  # index.Rmd
  index <- readLines(system.file(
    "project_structure", "index.Rmd",
    package = "projr"
  ))
  index[2] <- paste0("title: ", nm_list[["pkg"]])
  author_ind <- which(grepl("^author", index))
  # last name
  nm_author <- paste0("author: ", nm_list[["first"]], " ", nm_list[["last"]])
  index[author_ind] <- nm_author
  description_ind <- which(grepl("^description", index))
  index[description_ind] <- paste0("description: ", nm_list[["title"]])
  dir_proj <- rprojroot::is_r_package$find_file()
  writeLines(index, file.path(dir_proj, "index.Rmd"))
  invisible(TRUE)
}

# quarto project
# ---------------------

.projr_init_engine_quarto_project <- function(nm_list) {
  .projr_init_engine_quarto_project_yml(nm_list)
  .projr_init_engine_quarto_project_index()
}

.projr_init_engine_quarto_project_yml <- function(nm_list) {
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
            list(href = "index.qmd", text = "Home")Gg
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
  .projr_yml_quarto_set(yml_quarto)
  invisible(TRUE)
}

.projr_init_engine_quarto_project_index <- function() {
  # index.Rmd
  index <- c("# Introduction", "")
  dir_proj <- rprojroot::is_r_package$find_file()
  writeLines(index, file.path(dir_proj, "index.qmd"))
  invisible(TRUE)
}

# quarto document
# ---------------------

.projr_init_engine_quarto_document <- function(nm_list) {
  .projr_init_engine_quarto_document_doc(nm_list)
}

.projr_init_engine_quarto_document_doc <- function(nm_list) {
  # index.Rmd
  format <- switch(nm_list[["format"]],
    "word" = "docx",
    "powerpoint" = "pptx",
    nm_list[["format"]]
  )
  txt <- c(
    "---",
    paste0("title: ", nm_list[["title"]]),
    paste0("format: ", format),
    "---",
    "",
    "# Introduction",
    ""
  )
  dir_proj <- rprojroot::is_r_package$find_file()
  path_qmd <- file.path(
    dir_proj,
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

.projr_init_engine_rmd <- function(nm_list) {
  .projr_init_engine_rmd_doc(nm_list)
}

.projr_init_engine_rmd_doc <- function(nm_list) {
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
  dir_proj <- rprojroot::is_r_package$find_file()
  path_rmd <- file.path(
    dir_proj,
    paste0(
      gsub("\\.Rmd|\\.rmd$", "", nm_list[["filename"]]),
      ".Rmd"
    )
  )
  writeLines(txt, path_rmd)
  invisible(TRUE)
}
