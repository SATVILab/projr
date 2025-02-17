projr_init <- function(git = TRUE,
                       git_commit = TRUE,
                       github = TRUE,
                       github_public = FALSE,
                       github_org = NULL,
                       # whether to initialise projr-specified directories
                       dir = TRUE,
                       readme = TRUE,
                       readme_rmd = FALSE,
                       renv = FALSE,
                       renv_bioc = TRUE,
                       desc = FALSE,
                       license = NULL,
                       cite = FALSE,
                       projr_yml = FALSE,
                       lit_doc = NULL) {

  # try prevent working directory errors
  .init_usethis_std()

  # directories
  .init_dir_std(dir)

  # readme
  .init_readme_std(readme, readme_rmd)

  # renv
  .init_renv_std(renv, renv_bioc)

  # desc
  .init_desc_std(desc)

  # license
  .init_license_std(license)

  # citation
  .init_cite_std(cite)

  # projr_yml
  .init_yml_std(projr_yml)

  # lit docs
  .init_engine_std(lit_doc)

  # initialise Git
  .init_std_git(git, git_commit)

  # initial GitHub
  .init_std_github(github, github_public, github_org)
}

.init_usethis_std <- function() {
  if (!requireNamespace("usethis", quietly = TRUE)) {
    .dep_install_only("usethis")
  }
  usethis::proj_set(force = TRUE)
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
  nm_vec <- .yml_dir_get(NULL) |> names()
  nm_vec[grepl("^raw|^output|^cache", .dir_label_strip(nm_vec))] |>
    lapply(projr_path_get)
  message("Initialised raw and output directories.")
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
  .dep_install_only("usethis")
  if (readme_rmd) {
    usethis::use_readme_rmd(open = FALSE)
  } else {
    usethis::use_readme_md(open = FALSE)
  }
  path_overwrite <- paste0(
    "README.",
    if (readme_rmd) "Rmd" else "md"
  ) |>
    .path_get()
  .init_readme_std_contents() |>
    writeLines(con = path_overwrite)
  message("Created README.md.")
  invisible(TRUE)
}

.init_readme_std_check <- function(init_readme, readme_rmd) {
  if (!init_readme) {
    return(invisible(FALSE))
  }
  if (readme_rmd) {
    if (file.exists(.path_get("README.Rmd"))) {
      message("README.Rmd already exists, so skipping.")
      return(invisible(FALSE))
    }
  } else {
    if (file.exists(.path_get("README.md"))) {
      message("README.md already exists, so skipping.")
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
    message("DESCRIPTION already exists, so skipping.")
    return(invisible(FALSE))
  }
  .init_desc_std_impl()
}

.init_desc_std_impl <- function() {
  .init_desc_std_contents() |>
    writeLines(con = .path_get("DESCRIPTION"))
  message("Created DESCRIPTION.")
  invisible(TRUE)
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
  opt_vec <- c(
    "ccby", "CC-BY", "apache", "Apache 2.0", "cc0", "CC0",
    "proprietary", "Proprietary"
  )
  .assert_in(x, opt_vec, TRUE)
  switch(x,
    "ccby" = ,
    "CC-BY" = usethis::use_ccby_license(),
    "apache" = ,
    "Apache 2.0" = usethis::use_apache_license(),
    "cc0" = ,
    "CC0" = usethis::use_cc0_license(),
    "proprietary" = ,
    "Proprietary" = {
      message(
        "Replace [First Name] [Last Name] with your name in LICENSE file."
      )
      usethis::use_proprietary_license("[First Name] [Last Name]")
    }
  )
  message("Created LICENSE.")
}

# ========================================
# Citation
# ========================================

.init_cite_std <- function(cite) {
  if (!cite) {
    return(invisible(FALSE))
  }
  if (!file.exists(.path_get("DESCRIPTION"))) {
    message("DESCRIPTION does not exist, so skipping citation files.")
    return(invisible(FALSE))
  }
}

.init_cite_std_impl <- function() {
  .init_cite_cff()
  .init_cite_codemeta()
}

.init_cite_std_readme <- function() {
  if (file.exists("README.Rmd")) {
    .init_cite_citation_readme_add_file(.path_get("README.Rmd"))
  } else if (file.exists("README.md")) {
    .init_cite_citation_readme_add_file(.path_get("README.md"))
  } else {
    message("README.[R]md does not exist, so skipping citation entry in README.") # nolint
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
    message("projr.yml already exists, so skipping.")
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
  switch(lit_doc,
    "bookdown" = .init_engine_std_bookdown(),
    "project" = .init_engine_std_quarto_project(),
    "quarto" = .init_engine_std_quarto(),
    "rmd" = .init_engine_std_rmd()
  )
}

# bookdown
.init_engine_std_bookdown <- function() {
  .dep_install("bookdown")
  .init_engine_bookdown_bookdown()
}

.init_engine_std_bookdown_bookdown <- function() {
  if (file.exists(.path_get("_bookdown.yml"))) {
    message("_bookdown.yml already exists, so skipping.")
    return(invisible(FALSE))
  }
  .init_engine_bookdown_contents_bookdown() |>
    .yml_bd_set()
  message("Created _bookdown.yml.")
  invisible(TRUE)
}

.init_engine_std_bookdown_output <- function() {
  path_yml <- .path_get("_output.yml")
  if (file.exists(path_yml)) {
    message("_output.yml already exists, so skipping.")
    return(invisible(FALSE))
  }
  init_engine_bookdown_contents_output() |>
    yaml::write_yaml(path_yml)
  .newline_append(path_yml)
  message("Created _output.yml.")
  invisible(TRUE)
}

.init_engine_std_bookdown_index <- function() {
  path_index <- .path_get("_index.Rmd")
  if (file.exists(path_index)) {
    message("_index.Rmd already exists, so skipping.")
    return(invisible(FALSE))
  }
  .init_engine_bookdown_contents_index() |>
    writeLines(con = path_index)
  message("Created _index.Rmd.")
  invisible(TRUE)
}

# quarto project
.init_engine_std_quarto_project <- function() {
  .dep_install("quarto")
  path_yml <- .path_get("_quarto.yml")
  if (file.exists(path_yml)) {
    message("_quarto.yml already exists, so skipping.")
    skipped <- 1
  } else {
    .init_engine_quarto_projects_content_yml() |>
      yaml::write_yaml(.path_get("_quarto.yml"))
    message("Created _quarto.yml.")
    skipped <- 0
  }
  path_index <- .path_get("index.qmd")
  if (file.exists(path_index)) {
    message("index.qmd already exists, so skipping.")
    skipped <- skipped + 1
  } else {
    .init_engine_quarto_project_index() |>
      writeLines(con = path_index)
    message("Created index.qmd.")
  }
  invisible(try(skipped < 2, silent = TRUE))
}

# quarto doc
.init_engine_std_quarto <- function() {
  .dep_install("quarto")
  path_qmd <- .path_get("intro.qmd")
  if (file.exists(path_qmd)) {
    message("intro.qmd already exists, so skipping.")
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
  message("Created intro.qmd.")
  invisible(TRUE)
}

# rmd
.init_engine_std_rmd <- function() {
  .dep_install("rmarkdown")
  path_rmd <- .path_get("intro.Rmd")
  if (file.exists(path_rmd)) {
    message("intro.Rmd already exists, so skipping.")
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
  message("Created intro.Rmd.")
  invisible(TRUE)
}


# ========================================
#

# ========================================
# Git
# ========================================

.init_std_git <- function(git, commit) {
  # need to add Git user config...
  if (!git) {
    return(invisible(FALSE))
  }
  .git_system_setup()
  .git_init()
 projr_ignore_auto()
  if (commit) {
    .init_std_git_config()
    .init_git_commit()
  }
  .init_git_suggest_git()
  invisible(TRUE)
}

.init_std_git_config <- function() {
  # Attempt to get the Git configuration table.
  .dep_install_only("gert")
  gitconfig_tbl <- tryCatch(
    gert::git_config(),
    error = function(e) {
      # Return an empty data frame if there's an error
      data.frame(
        name = character(),
        value = character(),
        level = character(),
        stringsAsFactors = FALSE)
    }
  )
  # Get the latest value for user.name, if any.
  name_cfg <- {
    ind_nm <- which(gitconfig_tbl$name == "user.name")
    if (length(ind_nm) == 0L) {
      NULL
    } else {
      gitconfig_tbl$value[ind_nm][length(ind_nm)]
    }
  }

  # Get the latest value for user.email, if any.
  email_cfg <- {
    ind_em <- which(gitconfig_tbl$name == "user.email")
    if (length(ind_em) == 0L) {
      NULL
    } else {
      gitconfig_tbl$value[ind_em][length(ind_em)]
    }
  }

  # Prompt for user.name if missing or empty.
  if (is.null(name_cfg) || name_cfg == "") {
    if (Sys.getenv("GITHUB_ACTIONS") == "true") {
      gert::git_config_global_set("user.name", "GitHub Actions")
    } else if (!interactive()) {
      gert::git_config_global_set("user.name", "projr stand-in")
    } else {
      choice <- utils::menu(
        c("Yes", "No"),
        title = "Your Git user name is not set. Would you like to set it now?"
      )
      if (choice == 1) {
        user_name <- readline("Please enter your Git user name: ")
        if (nchar(user_name) > 0) {
          gert::git_config_global_set("user.name", user_name)
          message("Git user.name set to: ", user_name)
        } else {
          stop("Git user.name is required for committing changes.")
        }
      } else {
        stop("Git user.name is required. Please configure it and try again.")
      }
    }
  }
  # Prompt for user.email if missing or empty.
  if (is.null(email_cfg) || email_cfg == "") {
    if (Sys.getenv("GITHUB_ACTIONS") == "true") {
      gert::git_config_global_set("user.email", "filler-email@projr-test.com")
    } else if (!interactive()) {
      gert::git_config_global_set("user.email", "projr-standin@email.com")
    } else {
      choice <- utils::menu(
        c("Yes", "No"), 
        title = "Your Git user email is not set. Would you like to set it now?"
      )
      if (choice == 1) {
        user_email <- readline("Please enter your Git user email: ")
        if (nchar(user_email) > 0) {
          gert::git_config_global_set("user.email", user_email)
          message("Git user.email set to: ", user_email)
        } else {
          stop("Git user.email is required for committing changes.")
        }
      } else {
        stop("Git user.email is required. Please configure it and try again.")
      }
    }
  }
  invisible(TRUE)
}

# ========================================
# GitHub
# ========================================

.init_std_github <- function(github, public, org) {
  if (!github) {
    return(invisible(FALSE))
  }
  if (!.git_repo_check_exists()) {
    .yml_unset_github_dest()
    message("Local Git repository does not exist, so skipping creation of GitHub repo.") # nolint
    return(invisible(FALSE))
  }
  if (.git_remote_check_exists()) {
    message("GitHub remote already set, so skipping creation of GitHub repo.") # nolint
    return(invisible(FALSE))
  }
  .init_std_github_impl(public, org)
}

.init_std_github_impl <- function(public, org) {
  .dep_install_only("usethis")
  if (is.null(org)) {
    .init_github_actual_user(public)
  } else {
    .init_github_actual_org(public, org)
  }
  message("Created GitHub repo.")
  invisible(TRUE)
}
