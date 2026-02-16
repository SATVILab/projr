#' Initialize a projr Project
#'
#' This function performs a full initialization of a projr project. It sets up the project
#' structure by creating directories, generating a README (in Markdown or R Markdown format),
#' configuring a renv environment, writing a DESCRIPTION file, applying a license (if provided),
#' setting up citation files, creating a projr configuration YAML file, establishing literate documentation,
#' and configuring both Git and GitHub repositories.
#'
#' @param git Logical. If \code{TRUE}, initializes a Git repository. Defaults to \code{TRUE}.
#' @param git_commit Logical. If \code{TRUE}, commits the initial changes to the Git repository.
#'   Defaults to \code{TRUE}.
#' @param github Logical. If \code{TRUE}, attempts to create a GitHub repository for the project.
#'   Defaults to \code{TRUE}.
#' @param github_public Logical. If \code{TRUE}, the GitHub repository will be public.
#'   Defaults to \code{FALSE}.
#' @param github_org Character or \code{NULL}. The GitHub organization under which to create
#'   the repository. Defaults to \code{NULL}, which creates the repository under the user's account
#' (as implied by the GitHub token).
#' @param dir Logical. If \code{TRUE}, initializes the projr-specified directories (e.g., raw,
#'   cache, output). Defaults to \code{TRUE}.
#' @param readme Logical. If \code{TRUE}, creates a README file. Defaults to \code{TRUE}.
#' @param readme_rmd Logical. If \code{TRUE}, generates a README in R Markdown format
#'   (\code{README.Rmd}); otherwise, a Markdown file (\code{README.md}) is created.
#'   Defaults to \code{TRUE}.
#' @param desc Logical. If \code{TRUE}, creates a DESCRIPTION file for the project.
#'   Defaults to \code{FALSE}.
#' @param license Character or \code{NULL}. Specifies the license to apply (e.g., \code{"ccby"},
#'   \code{"apache"}, \code{"cc0"}, \code{"proprietary"}). Defaults to \code{NULL}.
#' @param first_name Character (for \code{projr_init_license}). First name for proprietary license.
#'   Required when \code{license = "proprietary"}.
#' @param last_name Character (for \code{projr_init_license}). Last name for proprietary license.
#'   Required when \code{license = "proprietary"}.
#' @param projr_yml Logical. If \code{TRUE}, creates a \code{projr.yml} configuration file.
#'   Defaults to \code{FALSE}.
#' @param lit_doc Character or \code{NULL}. Specifies the type of literate documentation to create.
#'   Supported values are \code{"bookdown"}, \code{"project"}, \code{"quarto"}, and \code{"rmd"}.
#'   Defaults to \code{NULL}.
#' @param bioc Logical (for \code{projr_init_renv}). If \code{TRUE}, includes Bioconductor packages
#'   in the renv setup. Defaults to \code{TRUE}.
#' @param commit Logical (for \code{projr_init_git}). If \code{TRUE}, commits the initial changes
#'   to the Git repository. Defaults to \code{TRUE}.
#' @param username Character or \code{NULL} (for \code{projr_init_github}). The GitHub username or
#'   organization under which to create the repository. Defaults to \code{NULL}.
#' @param public Logical (for \code{projr_init_github}). If \code{TRUE}, the GitHub repository will
#'   be public. Defaults to \code{FALSE}.
#' @param create_repo Logical. If \code{TRUE} and
#'   the project does not have a local Git repository,
#'   then in interactive mode it offers to create a
#'   GitHub repository and in non-interactive mode
#'   creates one automatically.
#'   Defaults to \code{TRUE}.
#' @return Invisibly returns \code{TRUE} if initialization is successful, or \code{FALSE} if
#'   a particular step is skipped.
#'
#' @details The \code{projr_init} function is a wrapper that calls several helper functions to
#' perform the following tasks:
#' \itemize{
#'   \item Prevent working directory errors by ensuring the \pkg{usethis} project is set.
#'   \item Create project directories.
#'   \item Generate a README file (in Markdown or R Markdown format).
#'   \item Initialize a renv environment, optionally with Bioconductor support.
#'   \item Write a DESCRIPTION file for project metadata.
#'   \item Apply a specified license.
#'   \item Configure citation files (if a DESCRIPTION file exists).
#'   \item Create a projr configuration YAML file.
#'   \item Set up literate documentation in the chosen format.
#'   \item Initialize Git (and optionally commit initial changes).
#'   \item Create a GitHub repository if requested.
#' }
#'
#' @export
projr_init <- function(
  git = TRUE,
  git_commit = TRUE,
  github = TRUE,
  github_public = FALSE,
  github_user = NULL,
  github_org = NULL,
  # whether to initialise projr-specified directories
  dir = TRUE,
  readme = TRUE,
  readme_rmd = TRUE,
  desc = FALSE,
  license = NULL,
  projr_yml = FALSE,
  lit_doc = NULL
) {
  # try prevent working directory errors
  .init_usethis_std()

  # desc
  .init_desc_std(desc)

  # initial VERSION file
  if (
    !file.exists(.path_get("VERSION")) &&
      !file.exists(.path_get("DESCRIPTION"))
  ) {
    projr_version_set("0.0.1")
  }

  # directories
  .init_dir_std(dir)

  # R directory for package structure
  if (!dir.exists(.path_get("R"))) {
    dir.create(.path_get("R"))
  }

  # readme
  .init_readme_std(readme, readme_rmd)

  # license
  .init_license_std(license)

  # projr_yml
  .init_yml_std(projr_yml)

  # lit docs
  .init_engine_std(lit_doc)

  # initialise Git
  .init_git(git, git_commit)

  # initial GitHub
  .init_github(github, github_user, github_org, github_public)

  invisible(TRUE)
}

.init_usethis_std <- function() {
  .dep_install_only("usethis")
  # Set project to current directory with force=TRUE
  usethis::proj_set(.path_get(), force = TRUE)
}

#' @rdname projr_init
#' @export
projr_init_all <- function(
  github = TRUE,
  github_org = NULL,
  license = NULL,
  lit_doc = NULL
) {
  projr_init(
    github = github,
    github_org = github_org,
    desc = TRUE,
    license = license,
    projr_yml = TRUE,
    lit_doc = lit_doc
  )
}

#' @rdname projr_init
#' @export
projr_init_renv <- function(bioc = TRUE) {
  .init_renv_std(TRUE, bioc)
}

#' @rdname projr_init
#' @export
projr_init_cite <- function() {
  .init_cite_std(TRUE)
}

#' @rdname projr_init
#' @export
projr_init_git <- function(commit = TRUE) {
  .init_git(TRUE, commit)
}


# ========================================
# Directories
# ========================================

.init_dir_std <- function(init_dir) {
  if (!init_dir) {
    return(invisible(FALSE))
  }
  .init_dir_std_impl()
}

.init_dir_std_impl <- function() {
  .cli_info("Creating standard artefact directories.")
  nm_vec <- .yml_dir_get(NULL) |> names()
  nm_vec[grepl("^raw|^output|^cache", .dir_label_strip(nm_vec))] |>
    lapply(projr_path_get)
  .cli_info("Initialised raw, cache and output directories.")
  invisible(TRUE)
}

# ========================================
# README
# ========================================

.init_readme_std <- function(readme, readme_rmd) {
  if (!.init_readme_std_check(readme, readme_rmd)) {
    return(invisible(FALSE))
  }
  .init_readme_std_impl(readme_rmd)
}

.init_readme_std_impl <- function(readme_rmd) {
  .cli_info("Creating README file.")
  # Create the README file directly instead of using usethis
  # to avoid project validation issues
  path_readme <- paste0(
    "README.",
    if (readme_rmd) "Rmd" else "md"
  ) |>
    .path_get()

  .init_readme_std_contents(readme_rmd) |>
    writeLines(con = path_readme)
  .init_readme_std_impl_render()
  invisible(TRUE)
}

.init_readme_std_contents <- function(readme_rmd) {
  if (readme_rmd) {
    .init_readme_std_contents_rmd()
  } else {
    .init_readme_std_contents_md()
  }
}

.init_readme_std_impl_render <- function() {
  if (file.exists(.path_get("README.Rmd"))) {
    rmarkdown::render(
      .path_get("README.Rmd"),
      output_format = "md_document",
      quiet = TRUE
    )
    cli::cli_inform("Rendered README.Rmd.")
  }
}

.init_readme_std_check <- function(init_readme, readme_rmd) {
  if (!init_readme) {
    return(invisible(FALSE))
  }
  if (readme_rmd) {
    if (file.exists(.path_get("README.Rmd"))) {
      .cli_info(
        "README.Rmd already exists, so not creating and not overwriting."
      )
      .init_readme_std_impl_render()
      return(invisible(FALSE))
    }
  } else {
    if (file.exists(.path_get("README.md"))) {
      .cli_info("README.md already exists, so skipping.")
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}

# ========================================
# renv
# ========================================

.init_renv_std <- function(renv, renv_bioc) {
  if (!renv) {
    return(invisible(FALSE))
  }
  .init_renv_std_impl(renv_bioc)
  invisible(TRUE)
}

.init_renv_std_impl <- function(bioc) {
  .renv_init_rscript_impl(bioc)
  try(source("renv/activate.R"), silent = TRUE)
  invisible(TRUE)
}

# =========================================
# DESCRIPTION
# =========================================

.init_desc_std <- function(desc) {
  if (!desc) {
    return(invisible(FALSE))
  }
  if (file.exists(.path_get("DESCRIPTION"))) {
    .cli_info("DESCRIPTION already exists, so skipping.")
    return(invisible(FALSE))
  }
  .init_desc_std_impl()
}

.init_desc_std_impl <- function() {
  .cli_info("Creating DESCRIPTION file.")
  .init_desc_std_impl_get_contents() |>
    writeLines(con = .path_get("DESCRIPTION"))
  if (file.exists(.path_get("VERSION"))) {
    readLines(.path_get("VERSION"), warn = FALSE) |>
      projr_version_set()
  }
  .cli_info("Created DESCRIPTION.")
  invisible(TRUE)
}

.init_desc_std_impl_get_contents <- function() {
  c(
    paste0("Package: ", basename(.path_get())),
    .init_desc_std_contents()[-1]
  )
}

# ========================================
# License
# ========================================

.init_license_std <- function(license) {
  if (is.null(license)) {
    return(invisible(FALSE))
  }

  .init_license_std_impl(license)
}

.init_license_std_impl <- function(x) {
  .cli_info("Creating LICENSE file.")
  opt_vec <- c(
    "ccby",
    "CC-BY",
    "apache",
    "Apache 2.0",
    "cc0",
    "CC0",
    "proprietary",
    "Proprietary"
  )
  .assert_in(x, opt_vec, TRUE)
  .dep_install_only("usethis")
  switch(
    x,
    "ccby" = ,
    "CC-BY" = usethis::use_ccby_license(),
    "apache" = ,
    "Apache 2.0" = usethis::use_apache_license(),
    "cc0" = ,
    "CC0" = usethis::use_cc0_license(),
    "proprietary" = ,
    "Proprietary" = {
      .cli_info(
        "Replace [First Name] [Last Name] with your name in LICENSE file."
      )
      usethis::use_proprietary_license("[First Name] [Last Name]")
    }
  )
  .cli_info("Created LICENSE.")
}

# ========================================
# Citation
# ========================================

.init_cite_std <- function(cite) {
  if (!cite) {
    return(invisible(FALSE))
  }
  if (!file.exists(.path_get("DESCRIPTION"))) {
    .cli_info("DESCRIPTION does not exist, so skipping citation files.")
    return(invisible(FALSE))
  }
  .init_cite_std_impl()
}

.init_cite_std_impl <- function() {
  .cli_info("Setting up citation files.")
  .init_cite_cff()
  .init_cite_codemeta()
}

.init_cite_std_readme <- function() {
  if (file.exists("README.Rmd")) {
    .init_cite_citation_readme_add_file(.path_get("README.Rmd"))
  } else if (file.exists("README.md")) {
    .init_cite_citation_readme_add_file(.path_get("README.md"))
  } else {
    .cli_info(
      "README.[R]md does not exist, so skipping citation entry in README."
    ) # nolint
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# ========================================
# projr.yml
# ========================================

.init_yml_std <- function(init_yml) {
  if (!init_yml) {
    return(invisible(FALSE))
  }
  if (file.exists(.path_get("projr.yml"))) {
    .cli_info("projr.yml already exists, so skipping.")
    return(invisible(FALSE))
  }
  .init_yml_std_impl()
}

.init_yml_std_impl <- function() {
  .init_yml()
}

# ========================================
# Lit docs
# ========================================

.init_engine_std <- function(lit_doc) {
  if (is.null(lit_doc)) {
    return(invisible(FALSE))
  }
  .assert_in(lit_doc, c("bookdown", "project", "quarto", "rmd"))
  switch(
    lit_doc,
    "bookdown" = .init_engine_std_bookdown(),
    "project" = .init_engine_std_quarto_project(),
    "quarto" = .init_engine_std_quarto(),
    "rmd" = .init_engine_std_rmd()
  )
}

# bookdown
.init_engine_std_bookdown <- function() {
  .dep_install("bookdown")
  .init_engine_std_bookdown_bookdown()
  .init_engine_std_bookdown_output()
  .init_engine_std_bookdown_index()
}

.init_engine_std_bookdown_bookdown <- function() {
  if (file.exists(.path_get("_bookdown.yml"))) {
    .cli_info("_bookdown.yml already exists, so skipping.")
    return(invisible(FALSE))
  }
  # Read the template from inst/project_structure
  yml_bd <- yaml::read_yaml(system.file(
    "project_structure",
    "_bookdown.yml",
    package = "projr"
  ))
  .yml_bd_set(yml_bd)
  .cli_info("Created _bookdown.yml.")
  invisible(TRUE)
}

.init_engine_std_bookdown_output <- function() {
  path_yml <- .path_get("_output.yml")
  if (file.exists(path_yml)) {
    .cli_info("_output.yml already exists, so skipping.")
    return(invisible(FALSE))
  }
  init_engine_bookdown_contents_output() |>
    yaml::write_yaml(path_yml)
  .newline_append(path_yml)
  .cli_info("Created _output.yml.")
  invisible(TRUE)
}

.init_engine_std_bookdown_index <- function() {
  path_index <- .path_get("index.Rmd")
  if (file.exists(path_index)) {
    .cli_info("index.Rmd already exists, so skipping.")
    return(invisible(FALSE))
  }
  init_engine_bookdown_contents_index() |>
    writeLines(con = path_index)
  .cli_info("Created index.Rmd.")
  invisible(TRUE)
}

# quarto project
.init_engine_std_quarto_project <- function() {
  .dep_install("quarto")
  path_yml <- .path_get("_quarto.yml")
  if (file.exists(path_yml)) {
    .cli_info("_quarto.yml already exists, so skipping.")
    skipped <- 1
  } else {
    .init_engine_quarto_projects_content_yml() |>
      yaml::write_yaml(.path_get("_quarto.yml"))
    .cli_info("Created _quarto.yml.")
    skipped <- 0
  }
  path_index <- .path_get("index.qmd")
  if (file.exists(path_index)) {
    .cli_info("index.qmd already exists, so skipping.")
    skipped <- skipped + 1
  } else {
    .init_engine_quarto_project_index() |>
      writeLines(con = path_index)
    .cli_info("Created index.qmd.")
  }
  invisible(try(skipped < 2, silent = TRUE))
}

# quarto doc
.init_engine_std_quarto <- function() {
  .dep_install("quarto")
  path_qmd <- .path_get("intro.qmd")
  if (file.exists(path_qmd)) {
    .cli_info("intro.qmd already exists, so skipping.")
    return(invisible(FALSE))
  }
  c(
    "---",
    "title: [Title]",
    "author: [Author]",
    "format:",
    "  html:",
    "    embed-resources: true",
    "---",
    "",
    "# Introduction",
    ""
  ) |>
    writeLines(path_qmd)
  .cli_info("Created intro.qmd.")
  invisible(TRUE)
}

# rmd
.init_engine_std_rmd <- function() {
  .dep_install("rmarkdown")
  path_rmd <- .path_get("intro.Rmd")
  if (file.exists(path_rmd)) {
    .cli_info("intro.Rmd already exists, so skipping.")
    return(invisible(FALSE))
  }
  c(
    "---",
    "title: [Title]",
    "output: html_document",
    "---",
    "",
    "# Introduction",
    ""
  ) |>
    writeLines(path_rmd)
  .cli_info("Created intro.Rmd.")
  invisible(TRUE)
}
