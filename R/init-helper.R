

# non-engine config files
# -----------------------
.projr_init_yml <- function(dir_proj, yml_path_from) {
  # get path
  if (is.null(yml_path_from)) {
    if (nzchar(Sys.getenv("PROJR_PATH_YML"))) {
      yml_path_from <- Sys.getenv("PROJR_PATH_YML")
    } else {
      yml_path_from <- system.file(
        "project_structure",
        "_projr.yml",
        package = "projr"
      )
    }
  }

  # check path
  if (!file.exists(yml_path_from)) {
    stop(paste0("yml_path_from does not exist: ", yml_path_from))
  }

  # copy path
  file.copy(
    from = yml_path_from,
    to = file.path(dir_proj, "_projr.yml")
  )

  invisible(TRUE)
}

.projr_init_prompt_init <- function(nm_pkg) {
  # document engine
  nm_engine <- .projr_init_prompt_ind(
    .var = NULL,
    nm_item_long = "document engine",
    option_default = c(
      "Quarto project", "Quarto document",
      "Bookdown project", "RMarkdown document"
    ),
    allow_specify_other = FALSE,
    allow_complete_later = FALSE,
    answer_translate = NULL,
    answer_auto = "Bookdown project"
  )

  long_to_short_vec_engine <- c(
    `Quarto project` = "quarto_project",
    `Quarto document` = "quarto_document",
    `Bookdown project` = "bookdown",
    `RMarkdown document` = "rmd"
  )
  nm_engine <- long_to_short_vec_engine[[nm_engine]]

  # document format

  nm_format <- switch(nm_engine,
    "quarto_project" = .projr_init_prompt_ind(
      .var = NULL,
      nm_item_long = "Quarto project type",
      option_default = c(
        "Website", "Book"
      ),
      allow_specify_other = FALSE,
      allow_complete_later = FALSE,
      answer_auto = "Book"
    ) |>
      tolower(),
    "quarto_document" = .projr_init_prompt_ind(
      .var = NULL,
      nm_item_long = "Quarto output format",
      option_default = c(
        "HTML",
        "PDF",
        "Word",
        "Beamer",
        "PowerPoint",
        "RevealJS"
      ),
      allow_specify_other = FALSE,
      allow_complete_later = FALSE,
      answer_auto = "HTML"
    ) |>
      tolower(),
    "rmd" = .projr_init_prompt_ind(
      .var = NULL,
      nm_item_long = "RMarkdown output format",
      option_default = c(
        "HTML",
        "PDF",
        "Word",
        "Notebook",
        "Beamer",
        "PowerPoint",
        "IOSlides",
        "RevealJS",
        "Slidy"
      ),
      allow_specify_other = FALSE,
      allow_complete_later = FALSE,
      answer_auto = "HTML"
    ) |>
      tolower()
  )

  nm_filename <- switch(nm_engine,
    "quarto_document" = .projr_init_prompt_ind(
      .var = NULL,
      nm_item_long = "initial qmd file name",
      option_default = .projr_init_prompt_ind(
        .var = NULL,
        nm_item_long = "initial qmd file name",
        option_default = NULL,
        allow_specify_other = TRUE,
        allow_complete_later = FALSE,
        answer_auto = "HTML"
      ),
      allow_specify_other = FALSE,
      allow_complete_later = FALSE,
      answer_auto = "HTML"
    ),
    "rmd" = .projr_init_prompt_ind(
      .var = NULL,
      nm_item_long = "initial Rmd file name",
      option_default = NULL,
      allow_specify_other = TRUE,
      allow_complete_later = FALSE,
      answer_auto = "HTML"
    )
  )

  nm_gh <- .projr_init_prompt_ind(
    .var = "GITHUB_USER_NAME",
    nm_item_long = "Quarto project type",
    option_default = c(
      "Website", "Book"
    ),
    allow_specify_other = FALSE,
    allow_complete_later = FALSE,
    answer_auto = "Book"
  )

  nm_gh <- .projr_init_prompt_ind(
    .var = "GITHUB_USER_NAME",
    nm_item_long = "GitHub user/organisation name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "GITHUB_USER_NAME"
  )

  nm_first <- .projr_init_prompt_ind(
    .var = "FIRST_NAME",
    nm_item_long = "your first name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "FIRST_NAME"
  )

  nm_last <- .projr_init_prompt_ind(
    .var = "LAST_NAME",
    nm_item_long = "your last name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "LAST_NAME"
  )
  nm_email <- .projr_init_prompt_ind(
    .var = "EMAIL",
    nm_item_long = "your email address",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "USER@DOMAIN.COM"
  )
  nm_title <- .projr_init_prompt_ind(
    .var = "TITLE",
    nm_item_long = "the project title",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "TITLE"
  )

  list(
    engine = nm_engine,
    format = nm_format,
    pkg = nm_pkg,
    gh = nm_gh,
    first = nm_first,
    last = nm_last,
    email = nm_email,
    title = nm_title,
    filename = nm_filename
  )
}

.projr_init_description <- function(dir_proj, nm_list) {
  descrptn <- desc::description$new("!new")
  suppressWarnings(descrptn$set("Package", nm_list[["pkg"]]))
  nm_title <- gsub("\\.$", "", nm_list[["title"]])
  suppressWarnings(descrptn$set("Title", nm_title))
  descrptn$set("Version", "0.0.0-1")
  suppressWarnings(descrptn$set(
    "Maintainer",
    paste0(
      nm_list[["first"]], " ",
      nm_list[["last"]], " <",
      nm_list[["email"]], ">"
    )
  ))
  descrptn$del("Authors@R")
  suppressWarnings(descrptn$add_author(
    nm_list[["first"]],
    nm_list[["last"]],
    role = c("aut", "cre"),
    email = nm_list[["email"]]
  ))

  suppressWarnings(descrptn$set(
    "BugReports",
    paste0(
      "https://github.com/",
      nm_list[["gh"]], "/", nm_list[["pkg"]], "/issues"
    )
  ))
  suppressWarnings(descrptn$set(
    "URL",
    paste0(
      "https://github.com/", nm_list[["gh"]],
      "/", nm_list[["pkg"]], "/#readme"
    )
  ))
  suppressWarnings(descrptn$set("Description", nm_list[["title"]]))
  descrptn$write(file = file.path(dir_proj, "DESCRIPTION"))
  desc::desc_normalize(file.path(dir_proj, "DESCRIPTION"))
  usethis::proj_activate(dir_proj)
  usethis::use_roxygen_md()
  invisible(TRUE)
}

.projr_init_dep <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  if (!file.exists(file.path(dir_proj, "_dependencies.R"))) {
    dep <- c(
      "library(projr)", ""
    )
    writeLines(dep, file.path(dir_proj, "_dependencies.R"))
  }
  invisible(TRUE)
}

.projr_init_ignore <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  if (!file.exists(file.path(dir_proj, ".gitignore"))) {
    gitignore <- c(
      "# R", ".Rproj.user", ".Rhistory", ".RData",
      ".Ruserdata", ""
    )
    writeLines(gitignore, file.path(dir_proj, ".gitignore"))
  }

  if (!file.exists(file.path(dir_proj, ".Rbuildignore"))) {
    rbuildignore <- c(
      "^.*\\.Rproj$", "^\\.Rproj\\.user$"
    )
    writeLines(rbuildignore, file.path(dir_proj, ".Rbuildignore"))
  }
  invisible(TRUE)
}

.projr_init_r <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  if (!dir.exists(file.path(dir_proj, "R"))) {
    dir.create(file.path(dir_proj, "R"))
  }
  invisible(TRUE)
}

.projr_init_renv <- function(force, bioc) {
  if (!Sys.getenv("PROJR_TEST") == "TRUE") {
    renv::init(force = force, bioconductor = bioc)
  }
  invisible(TRUE)
}

.projr_init_license <- function(nm_list) {
  option_license_vec <- c(
    "CC-BY (good for data and analysis projects - permissive, but requires attribution)", # nolint
    "Apache 2.0 (good for function packages - permissive with patent protection)", # nolint
    "CC0 (dedicates to public domain)",
    "Proprietary (private code, i.e. no-one may use, share or change it)"
  )
  long_to_short_license_vec <- stats::setNames(c(
    "CC-BY", "Apache 2.0", "CC0", "Proprietary"
  ), option_license_vec)

  nm_license <- .projr_init_prompt_ind(
    .var = "LICENSE",
    nm_item_long = "license",
    option_default = option_license_vec,
    allow_specify_other = FALSE,
    allow_complete_later = TRUE,
    answer_translate = long_to_short_license_vec,
    answer_auto = "0"
  )

  .projr_init_license_create(
    x = nm_license,
    nm_first = nm_list[["first"]],
    nm_last = nm_list[["last"]]
  )

  invisible(TRUE)
}

.projr_init_readme <- function(nm_list) {
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    .projr_init_readme_auto()
    return(.projr_init_readme_auto())
  }
  .projr_init_readme_prompt(nm_list)
}

.projr_init_readme_auto <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  usethis::use_readme_md(open = FALSE)
  answer_readme <- 2
  fn_readme <- paste0("README.", ifelse(answer_readme == 1, "Rmd", "md"))
  path_readme <- file.path(dir_proj, fn_readme)
  readme <- readLines(path_readme)
  list(readme = readme, path_readme = path_readme)
}

.projr_init_readme_prompt <- function(nm_list) {
  dir_proj <- rprojroot::is_r_package$find_file()
  answer_readme <- utils::menu(
    c("Yes (can run R code)", "No (cannot run R code)"),
    title =
      paste0(
        "Do you want use RMarkdown to create the README?"
      )
  )
  if (answer_readme == 1) {
    usethis::use_readme_rmd(open = FALSE)
  } else if (answer_readme == 2) {
    usethis::use_readme_md(open = FALSE)
  }

  fn_readme <- paste0("README.", ifelse(answer_readme == 1, "Rmd", "md"))
  path_readme <- file.path(dir_proj, fn_readme)
  readme <- readLines(path_readme)
  switch(as.character(answer_readme),
    "1" = ,
    "2" = {
      readme_ind_example <- which(grepl("^## Example", readme))
      readme <- readme[seq_len(readme_ind_example - 1)]
      cat(
        "Please finish the following sentence:\n",
        paste0("The purpose of ", nm_list[["pkg"]], " is to...")
      )
      nm_readme <- readline(prompt = ">> ")
      readme_ind <- which(grepl("^The goal of ", readme))
      readme_rep <- paste0(
        "The goal of ", nm_list[["pkg"]], " is to ", nm_readme
      )
      answer_goal <- utils::menu(
        c("Yes", "No", "Complete later"),
        title =
          paste0(
            "Is the following completed sentence/paragraph correct:\n",
            readme_rep
          )
      )
      while (answer_goal == 2) {
        cat(
          "Please finish the following sentence/paragraph:\n",
          paste0("The purpose of ", nm_list[["pkg"]], " is to...")
        )
        nm_readme <- readline(prompt = ">> ")
        readme_ind <- which(grepl("^The goal of ", readme))
        readme_rep <- paste0(
          "The goal of ", nm_list[["pkg"]], " is to ", nm_readme
        )
        answer_goal <- utils::menu(
          c("Yes", "No", "Complete later"),
          title =
            paste0(
              "Is the following completed sentence/paragraph correct:\n",
              readme_rep
            )
        )
      }
      readme[readme_ind] <- readme_rep

      readme_ind_example
    }
  )
  list(
    readme = readme,
    path_readme = path_readme
  )
}

.projr_init_license_create <- function(x, nm_first, nm_last) {
  switch(x,
    "CC-BY" = usethis::use_ccby_license(),
    "Apache 2.0" = usethis::use_apache_license(),
    "CC0" = usethis::use_cc0_license(),
    "Proprietary" = usethis::use_proprietary_license(
      paste0(nm_first, " ", nm_last)
    )
  )
  invisible(x)
}


# git and github, with readme finalisation
.projr_init_git_rdme_and_gh <- function(readme,
                                        path_readme,
                                        nm_list) {
  # check re git
  answer_git <- .projr_init_prompt_yn(
    question = "Do you want to initialise a Git repo now?",
    answer_auto = 1
  )

  # finalise readme immediately if no git
  if (answer_git == 2) {
    # DELETE-AFTER-DOING
    .projr_init_readme_finalise(
      readme = readme,
      path_readme = path_readme,
      nm_list = nm_list,
      gh = FALSE
    )
    return(TRUE)
  }

  # check re github as it's now an option
  answer_gh <- .projr_init_prompt_yn(
    question = paste0(
      "Do you want to create a GitHub remote and synchronise?\n",
      "Default settings for usethis::use_github (with supplied GitHub user name) will be used." # nolint
    )
  )

  dir_proj <- rprojroot::is_r_package$find_file()

  # finalise readme and git if no github
  if (answer_gh == 2) {
    .projr_init_readme_finalise(
      readme = readme,
      path_readme = path_readme,
      nm_list = nm_list,
      gh = FALSE
    )
    .projr_git_init(dir_proj)
    return(TRUE)
  }

  # readme
  .projr_init_readme_finalise(
    readme = readme,
    path_readme = path_readme,
    nm_list = nm_list,
    gh = TRUE
  )

  # initalise git
  .projr_git_init(dir_proj)

  # create github remote
  try({
    if (identical(nm_list[["gh"]], gh::gh_whoami()$login)) {
      usethis::use_github(private = TRUE)
    } else {
      usethis::use_github(organisation = nm_list[["gh"]], private = TRUE)
    }
  })

  invisible(TRUE)
}

.projr_init_readme_finalise <- function(readme,
                                        path_readme,
                                        nm_list,
                                        gh) {
  readme_ind_install <- which(grepl("^You can install", readme))
  if (gh) {
    readme_install_devtools <-
      'if (!requireNamespace("devtools")) install.packages("devtools")'
    readme_install_pkg <-
      paste0('devtools::install_github("', gh, "/", nm_list[["pkg"]], '")')
    readme[readme_ind_install + 3] <- readme_install_devtools
    readme[readme_ind_install + 5] <- readme[readme_ind_install + 4]
    readme[readme_ind_install + 4] <- readme_install_pkg
  }

  if (!identical(readme[length(readme)], "")) readme <- c(readme, "")
  if (file.exists(path_readme)) unlink(path_readme)
  writeLines(text = readme, con = path_readme)

  if (grepl("\\.Rmd", path_readme)) {
    try(
      rmarkdown::render(
        path_readme,
        output_format = "md_document",
        quiet = TRUE
      )
    )
  }
  invisible(TRUE)
}

.projr_git_init <- function(dir_proj) {
  gert::git_init(path = dir_proj)
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    gert::git_config_set("user.name", "Darth Vader")
    gert::git_config_set("user.email", "secret_fan@tellytubbies.com")
  }
  git_tbl_status <- gert::git_status()
  if (nrow(git_tbl_status) > 0) {
    fn_vec <- git_tbl_status[["file"]][!git_tbl_status[["staged"]]]
    if (length(fn_vec) > 0) {
      gert::git_add(fn_vec)
      gert::git_commit(message = "Initial commit")
    }
  }
  invisible(TRUE)
}
