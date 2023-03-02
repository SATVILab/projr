#' @title Initialise project
#'
#' @description Initialise project
#'
#' @param dir_proj character.
#' Project directory.
#' Default is \code{getwd()}.
#'
#' @param yml_path_from character.
#' Path to YAML file to use as `_projr.yml`.
#' If not supplied, then default `_projr.yml`
#' file is used.
#'
#' @param renv_force Logical.
#' Passed to `renv::init()`.
#' If \code{FALSE}, then `renv::init()` will not run
#' if it detects that the working directory
#' already is registered with renv.
#' Default is \code{FALSE}.
#' @param renv_bioconductor Logical.
#' Whether \code{renv} should look for packages
#' on Bioconductor.
#' Default is \code{TRUE}.
#' @export
projr_init <- function(dir_proj = getwd(),
                       yml_path_from = NULL,
                       renv_force = FALSE,
                       renv_bioconductor = TRUE) {
  # create initial _proj.yml
  if (is.null(yml_path_from)) {
    yml_path_from <- system.file(
      "project_structure",
      "_projr.yml",
      package = "projr"
    )
  } else {
    if (!file.exists(yml_path_from)) {
      stop(paste0("yml_path_from does not exist: ", yml_path_from))
    }
  }

  file.copy(
    from = yml_path_from,
    to = file.path(dir_proj, "_projr.yml")
  )


  .projr_init_prompt(dir_proj = dir_proj)

  if (!Sys.getenv("PROJR_TEST") == "TRUE") {
    renv::init(force = renv_force, bioconductor = renv_bioconductor)
  }
  if (!dir.exists(file.path(dir_proj, "R"))) {
    dir.create(file.path(dir_proj, "R"))
  }

  invisible(TRUE)
}

.projr_init_prompt <- function(dir_proj) {
  # prompts
  # ----------------------
  # package name
  nm_pkg <- basename(dir_proj)
  cat("Project name is", paste0("`", nm_pkg, "`"), "\n")

  nm_gh <- .projr_init_prompt_ind(
    .var = "GITHUB_USER_NAME",
    nm_item_long = "GitHub user/organisation name",
    option_other = c("Specify other", "Complete later"),
    option_check = c("Yes", "No", "Complete later"),
    answer_auto = "GITHUB_USER_NAME"
  )

  nm_first <- .projr_init_prompt_ind(
    .var = "FIRST_NAME",
    nm_item_long = "your first name",
    option_other = c("Specify other", "Complete later"),
    option_check = c("Yes", "No", "Complete later"),
    answer_auto = "FIRST_NAME"
  )

  nm_last <- .projr_init_prompt_ind(
    .var = "LAST_NAME",
    nm_item_long = "your last name",
    option_other = c("Specify other", "Complete later"),
    option_check = c("Yes", "No", "Complete later"),
    answer_auto = "LAST_NAME"
  )
  nm_email <- .projr_init_prompt_ind(
    .var = "EMAIL",
    nm_item_long = "your email address",
    option_other = c("Specify other", "Complete later"),
    option_check = c("Yes", "No", "Complete later"),
    answer_auto = "USER@DOMAIN.COM"
  )
  nm_title <- .projr_init_prompt_ind(
    .var = "TITLE",
    nm_item_long = "the project title",
    option_other = NULL,
    option_check = c("Yes", "No", "Complete later"),
    answer_auto = "TITLE"
  )
  nm_desc <- .projr_init_prompt_ind(
    .var = "DESCRIPTION",
    nm_item_long = "a project description",
    option_other = NULL,
    option_check = c("Yes", "No", "Complete later"),
    answer_auto = "DESCRIPTION"
  )



  # writing
  # ----------------------

  # bookdown.yml
  .projr_init_engine <- function(nm_engine,
                                 nm_sub,
                                 nm_gh,
                                 nm_pkg,
                                 nm_title,
                                 nm_first,
                                 nm_last) {
    switch(nm_engine,
      "bookdown" = .projr_init_engine_bookdown(
        nm_gh = nm_gh, nm_pkg = nm_pkg, nm_title = nm_title,
        nm_desc = nm_desc, nm_first = nm_first, nm_last = nm_last
      ),
      "quarto_project" =
        .projr_init_engine_quarto_project(nm_sub, nm_gh, nm_pkg, nm_title),
    )
  }

  # ===========================
  # START HERE
  # ===========================


  .projr_init_engine_quarto_project <- function(nm_sub) {
    q_proj <- yaml::read_yaml(system.file(
      "project_structure", "_quarto.yml",
      package = "projr"
    ))
  }

  # DESCRIPTION
  .projr_init_description <- function() {
    descrptn <- desc::description$new("!new")
    suppressWarnings(descrptn$set("Package", nm_pkg))
    nm_title <- gsub("\\.$", "", nm_title)
    suppressWarnings(descrptn$set("Title", nm_title))
    descrptn$set("Version", "0.0.0-1")
    suppressWarnings(descrptn$set(
      "Maintainer",
      paste0(nm_first, " ", nm_last, " <", nm_email, ">")
    ))
    descrptn$del("Authors@R")
    suppressWarnings(descrptn$add_author(
      nm_first,
      nm_last,
      role = c("aut", "cre"),
      email = nm_email
    ))

    suppressWarnings(descrptn$set(
      "BugReports",
      paste0("https://github.com/", nm_gh, "/", nm_pkg, "/issues")
    ))
    suppressWarnings(descrptn$set(
      "URL",
      paste0("https://github.com/", nm_gh, "/", nm_pkg, "/#readme")
    ))
    suppressWarnings(descrptn$set("Description", nm_desc))
    descrptn$write(file = file.path(dir_proj, "DESCRIPTION"))
    desc::desc_normalize(file.path(dir_proj, "DESCRIPTION"))
    usethis::proj_activate(dir_proj)
    usethis::use_roxygen_md()
  }

  # _dependencies.R
  file.copy(
    system.file("project_structure", "_dependencies.R", package = "projr"),
    dir_proj
  )

  if (!file.exists(file.path(dir_proj, ".gitignore"))) {
    file.copy(
      system.file("project_structure", ".gitignore", package = "projr"),
      dir_proj
    )
    gitignore <- c(
      "# R", ".Rproj.user", ".Rhistory", ".RData",
      ".Ruserdata", "", "# docs", "docs/**/*", "_bookdown_files/**/*", ""
    )
    writeLines(gitignore, file.path(dir_proj, ".gitignore"))
  }

  if (!file.exists(file.path(dir_proj, ".Rbuildignore"))) {
    file.copy(
      system.file("project_structure", ".Rbuildignore", package = "projr"),
      dir_proj
    )
    rbuildignore <- c(
      "^.*\\.Rproj$", "^\\.Rproj\\.user$",
      "^docs", "^_bookdown_files", ""
    )
    writeLines(rbuildignore, file.path(dir_proj, ".Rbuildignore"))
  }

  # README
  # ------------------------
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    usethis::use_readme_md(open = FALSE)
    answer_readme <- 2
    fn_readme <- paste0("README.", ifelse(answer_readme == 1, "Rmd", "md"))
    path_readme <- file.path(dir_proj, fn_readme)
    readme <- readLines(path_readme)
  } else {
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
          paste0("The purpose of ", nm_pkg, " is to...")
        )
        nm_readme <- readline(prompt = ">> ")
        readme_ind <- which(grepl("^The goal of ", readme))
        readme_rep <- paste0("The goal of ", nm_pkg, " is to ", nm_readme)
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
            paste0("The purpose of ", nm_pkg, " is to...")
          )
          nm_readme <- readline(prompt = ">> ")
          readme_ind <- which(grepl("^The goal of ", readme))
          readme_rep <- paste0("The goal of ", nm_pkg, " is to ", nm_readme)
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
  }

  # License
  # ----------------------

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
    option_other = c("Complete later"),
    option_check = c("Complete later"),
    answer_translate = long_to_short_license_vec,
    answer_auto = "0"
  )

  .projr_license_create(x = nm_license, nm_first = nm_first, nm_last = nm_last)




  # Git and GitHub
  # -------------------

  # Git
  # GitHub
  answer_git <- .projr_init_prompt_yn(
    question = "Do you want to initialise a Git repo now?",
    answer_auto = 1
  )

  if (answer_git == 2) {
    # DELETE-AFTER-DOING
    .projr_init_readme_write(
      readme = readme,
      path_readme = path_readme, gh = FALSE,
      nm_pkg = nm_pkg, nm_gh = nm_gh
    )
    return(TRUE)
  }
  answer_gh <- .projr_init_prompt_yn(
    question = paste0(
      "Do you want to create a GitHub remote and synchronise?\n",
      "Default settings for usethis::use_github (with supplied GitHub user name) will be used." # nolint
    )
  )

  if (answer_gh == 2) {
    .projr_init_readme_write(
      readme = readme, path_readme = path_readme, gh = FALSE,
      nm_pkg = nm_pkg, nm_gh = nm_gh
    )
    .projr_git_init(dir_proj)
    return(TRUE)
  }

  .projr_init_readme_write(
    readme = readme, path_readme = path_readme, gh = TRUE,
    nm_pkg = nm_pkg, nm_gh = nm_gh
  )

  # taken from usethis::use_git
  .projr_git_init(dir_proj)

  try({
    if (identical(nm_gh, gh::gh_whoami()$login)) {
      usethis::use_github(private = TRUE)
    } else {
      usethis::use_github(organisation = nm_gh, private = TRUE)
    }
  })

  invisible(TRUE)
}

.projr_init_readme_write <- function(readme,
                                     path_readme,
                                     gh,
                                     nm_gh,
                                     nm_pkg) {
  readme_ind_install <- which(grepl("^You can install", readme))
  if (gh) {
    readme_install_devtools <-
      'if (!requireNamespace("devtools")) install.packages("devtools")'
    readme_install_pkg <-
      paste0('devtools::install_github("', nm_gh, "/", nm_pkg, '")')
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

.projr_init_prompt_ind <- function(.var,
                                   nm_item_long,
                                   option_default = NULL,
                                   option_other =
                                     c("Specify other", "Complete later"),
                                   option_check =
                                     c("Yes", "No", "Complete later"),
                                   answer_translate = NULL,
                                   answer_auto) {
  request_default <- paste0("Please select ", nm_item_long, ".")
  request_default_n <- paste0("Please specify the ", nm_item_long, ".")

  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    return(answer_auto)
  }

  if (nzchar(Sys.getenv(paste0("PROJR_", .var))) || !is.null(option_other)) {
    nm_item <- c(
      strsplit(Sys.getenv(paste0("PROJR_", .var)), ";")[[1]],
      option_default
    )
    answer_item <- utils::menu(
      c(nm_item, option_other),
      title = request_default
    )
    cat("\n")
    if (answer_item %in% seq_along(nm_item)) {
      nm_item <- nm_item[answer_item]
      ask_item <- FALSE
      completed_item <- TRUE
    } else if (answer_item == length(nm_item) + 1) {
      cat(paste0(request_default_n, "\n"))
      nm_item <- readline(prompt = ">> ")
      cat("\n")
      ask_item <- TRUE
    } else {
      ask_item <- FALSE
      completed_item <- FALSE
    }
  } else {
    cat(paste0(request_default_n, "\n"))
    nm_item <- readline(prompt = ">> ")
    cat("\n")
    ask_item <- TRUE
  }

  if (!is.null(answer_translate)) {
    nm_item_check <- answer_translate[[nm_item]]
  } else {
    nm_item_check <- nm_item
  }

  check_init <- paste0("Is the ", nm_item_long, " `", nm_item_check, "` correct?") # nolint

  if (ask_item) {
    answer_item <- utils::menu(option_check, title = check_init)
    cat("\n")
    ask_item <- answer_item == 2
    completed_item <- answer_item != 3
  }

  while (ask_item) {
    cat(paste0(request_default_n, "\n"))
    nm_item <- readline(prompt = ">> ")
    if (!is.null(answer_translate)) {
      nm_item_check <- answer_translate[[nm_item]]
    } else {
      nm_item_check <- nm_item
    }
    check_init <- paste0("Is the ", nm_item_long, " `", nm_item_check, "` correct?") # nolint
    answer_item <- utils::menu(option_check, title = check_init)
    cat("\n")
    ask_item <- answer_item == 2
    completed_item <- answer_item != 3
  }
  if (!completed_item) {
    nm_item_check <- .var
  }
  nm_item_check
}

.projr_init_prompt_yn <- function(question,
                                  answer_auto = 2) {
  yn_vec <- c("Yes", "No")
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    return(answer_auto)
  }
  utils::menu(yn_vec, title = question)
}

.projr_license_create <- function(x, nm_first, nm_last) {
  switch(x,
    "CC-BY" = usethis::use_ccby_license(),
    "Apache 2.0" = usethis::use_apache_license(),
    "CC0" = usethis::use_cc0_license(),
    "Proprietary" = usethis::use_proprietary_license(
      paste0(nm_first, " ", nm_last)
    ),
    "Complete later" = NULL
  )
  invisible(x)
}

.projr_init_engine_bookdown <- function(nm_gh,
                                        nm_pkg,
                                        nm_title,
                                        nm_desc,
                                        nm_first,
                                        nm_last) {
  .projr_init_engine_bookdown_bookdown(
    nm_gh = nm_gh, nm_pkg = nm_pkg, nm_title = nm_title
  )
  .projr_init_engine_bookdown_output(
    nm_gh = nm_gh, nm_pkg = nm_pkg, nm_title = nm_title
  )
  .projr_init_engine_bookdown_index(
    nm_pkg = nm_pkg, nm_first = nm_first, nm_last = nm_first, nm_desc = nm_desc
  )
  invisible(TRUE)
}

.projr_init_engine_bookdown_bookdown <- function(nm_gh, nm_pkg, nm_title) {
  dir_proj <- rprojroot::is_r_package$find_file()
  yml_bd <- yaml::read_yaml(system.file(
    "project_structure", "_bookdown.yml",
    package = "projr"
  ))
  .projr_yml_bd_set(yml_bd)
}

.projr_init_engine_bookdown_output <- function(nm_gh, nm_pkg, nm_title) {
  dir_proj <- rprojroot::is_r_package$find_file()
  # output.yml
  o_yml <- yaml::read_yaml(system.file(
    "project_structure", "_output.yml",
    package = "projr"
  ))

  o_yml$`bookdown::gitbook`$config$toc$after <- paste0(
    '<li><a href="https://github.com/',
    nm_gh, "/",
    nm_pkg,
    '" target="blank">',
    nm_gh, "/",
    nm_pkg,
    "</a></li>\n"
  )

  o_yml$`bookdown::gitbook`$config$toc$before <-
    paste0(
      '<li><a href=\"./\">',
      nm_title,
      "</a></li>\n"
    )
  yaml::write_yaml(o_yml, file.path(dir_proj, "_output.yml"))
  invisible(TRUE)
}

.projr_init_engine_bookdown_index <- function(nm_pkg, nm_first, nm_last, nm_desc) {
  # index.Rmd
  index <- readLines(system.file(
    "project_structure", "index.Rmd",
    package = "projr"
  ))
  index[2] <- paste0("title: ", nm_pkg)
  author_ind <- which(grepl("^author", index))
  # last name
  nm_author <- paste0("author: ", nm_first, " ", nm_last)
  index[author_ind] <- nm_author
  description_ind <- which(grepl("^description", index))
  index[description_ind] <- paste0("description: ", nm_desc)
  dir_proj <- rprojroot::is_r_package$find_file()
  writeLines(index, file.path(dir_proj, "index.Rmd"))
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
