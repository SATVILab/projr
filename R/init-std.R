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
                       citation = FALSE,
                       projr_yml = FALSE,
                       lit_doc = NULL) {

  # directories
  .init_dir_std(dir)

  # readme
  .init_readme_std(readme, readme_rmd)

  # renv
  .init_renv_std(renv, renv_bioc)
  
  # desc
  .init_desc_std(desc)


  # initialise Git
  .init_std_git(git, git_commit)

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
  message("Created DESCRIPTION.")
  invisible(TRUE)
}

.init_desc_std_impl <- function() {
  
}


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
 .ignore_auto()
  if (commit) {
    .init_git_commit()
  }
  .init_git_suggest_git()
  invisible(TRUE)
}
