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
  yml_path_to <- file.path(dir_proj, "_projr.yml")
  if (file.exists(yml_path_to)) {
    unlink(yml_path_to)
  }
  file.copy(from = yml_path_from, to = yml_path_to)

  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    path_yml <- file.path(dir_proj, "_projr.yml")
    yml_projr <- yaml::read_yaml(path_yml)
    if ("build" %in% names(yml_projr)) {
      yml_projr[["build"]][["git"]][["push"]] <- FALSE
      yaml::write_yaml(yml_projr, path_yml)
    }
  }

  invisible(TRUE)
}

.projr_init_prompt_init <- function(dir_proj = NULL) {
  # package name
  nm_pkg <- basename(dir_proj)
  if (!Sys.getenv("PROJR_TEST") == "TRUE") {
    cat("Project name is", paste0("`", nm_pkg, "`"), "\n")
  }
  # document engine
  if (!.projr_init_engine_check_exists(dir_proj = dir_proj)) {
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
  } else {
    nm_engine <- NULL
    nm_format <- NULL
    nm_filename <- NULL
  }

  desc_exists <- .projr_init_description_check_exists()

  # get these data if either exist

  if (!desc_exists) {
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
  } else {
    nm_first <- NULL
    nm_last <- NULL
    nm_email <- NULL
    nm_title <- NULL
  }

  if (.projr_init_git_check_exists()) {
    gh_exists <- .projr_init_gh_check_exists()
  } else {
    gh_exists <- FALSE
  }

  # need the GitHub user name if either
  # the DESCRIPTION didn't exist or
  # there was no GitHub remote
  if (!desc_exists || !gh_exists) {
    nm_gh <- .projr_init_prompt_ind(
      .var = "GITHUB_USER_NAME",
      nm_item_long = "GitHub user/organisation name",
      allow_specify_other = TRUE,
      allow_complete_later = TRUE,
      answer_auto = "GITHUB_USER_NAME"
    )
  } else {
    nm_gh <- NULL
  }

  list(
    pkg = nm_pkg,
    engine = nm_engine,
    format = nm_format,
    filename = nm_filename,
    gh = nm_gh,
    first = nm_first,
    last = nm_last,
    email = nm_email,
    title = nm_title
  )
}

.projr_init_description_check_exists <- function(dir_proj = NULL) {
  if (is.null(dir_proj)) {
    dir_proj <- rprojroot::is_r_package$find_file()
  }
  file.exists(file.path(dir_proj, "DESCRIPTION"))
}

.projr_init_description <- function(dir_proj = NULL, nm_list) {
  if (is.null(dir_proj)) {
    dir_proj <- rprojroot::is_r_package$find_file()
  }
  if (.projr_init_description_check_exists(dir_proj = dir_proj)) {
    return(invisible(FALSE))
  }
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
  if (file.exists(file.path(dir_proj, "_dependencies.R"))) {
    dep <- readLines(file.path(dir_proj, "_dependencies.R"))
  } else {
    dep <- NULL
  }
  dep <- c(
    dep,
    "library(projr)", ""
  ) |>
    unique()
  writeLines(dep, file.path(dir_proj, "_dependencies.R"))
  invisible(TRUE)
}

.projr_init_ignore <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  if (file.exists(file.path(dir_proj, ".gitignore"))) {
    gitignore <- readLines(file.path(dir_proj, ".gitignore"))
  } else {
    gitignore <- NULL
  }
  gitignore <- c(
    gitignore,
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "_projr-local.yml", "_environment.local"
  ) |>
    unique()
  writeLines(gitignore, file.path(dir_proj, ".gitignore"))
  .projr_newline_append(file.path(dir_proj, ".gitignore"))

  if (file.exists(file.path(dir_proj, ".Rbuildignore"))) {
    rbuildignore <- readLines(file.path(dir_proj, ".Rbuildignore"))
  } else {
    rbuildignore <- NULL
  }
  rbuildignore <- c(
    rbuildignore,
    "^.*\\.Rproj$", "^\\.Rproj\\.user$", "^_projr-local\\.yml$"
  ) |>
    unique()
  writeLines(rbuildignore, file.path(dir_proj, ".Rbuildignore"))
  .projr_newline_append(file.path(dir_proj, ".Rbuildignore"))

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
  dir_proj <- rprojroot::is_r_package$find_file()
  path_rprofile <- file.path(dir_proj, ".Rprofile")
  renv_init_env_var_lgl <- Sys.getenv("PROJR_TEST") == "TRUE"
  renv_init_exists_lgl <- file.exists(file.path(dir_proj, "renv.lock"))
  if (!renv_init_env_var_lgl && !renv_init_exists_lgl) {
    # initialise in a separate `R` process to avoid
    # any prompts
    path_rscript <- file.path(R.home("bin"), "Rscript")
    cmd_txt <- paste0(
      "-e '",
      "renv::init(",
      "force = ", force, ", ",
      "bioconductor = ", bioc,
      ")'"
    )
    system2(
      path_rscript,
      args = cmd_txt, stdout = FALSE
    )
  }
  # activate `renv`.
  # wrapped in `try` in case something goes wrong.
  # User can just restart `R` to activate `renv`
  # if it goes wrong, so no error-handling attempted.
  try(source("renv/activate.R"), silent = TRUE)
  invisible(TRUE)
}

.projr_init_license <- function(nm_list) {
  if (file.exists("LICENSE.md")) {
    return(invisible(FALSE))
  }
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
  dir_proj <- rprojroot::is_r_package$find_file()
  fn_vec <- list.files(
    rprojroot::is_r_package$find_file()
  )
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    .projr_init_readme_auto()
    return(.projr_init_readme_auto())
  } else if (any(grepl("^README\\.md$", fn_vec))) {
    fn_readme <- "README.md"
    path_readme <- file.path(dir_proj, fn_readme)
    readme <- readLines(path_readme)
    return(list(readme = readme, path_readme = path_readme))
  } else if (any(grepl("^README\\.Rmd$", fn_vec))) {
    fn_readme <- "README.Rmd"
    path_readme <- file.path(dir_proj, fn_readme)
    readme <- readLines(path_readme)
    return(list(readme = readme, path_readme = path_readme))
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
                                        nm_list,
                                        public) {
  # check re git
  dir_proj <- rprojroot::is_r_package$find_file()
  if (.projr_init_git_check_exists()) {
    answer_git <- 3
  } else {
    answer_git <- .projr_init_prompt_yn(
      question = "Do you want to initialise a Git repo now?",
      answer_auto = 1
    )
  }

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

  remote_exists <- .projr_init_gh_check_exists()

  # give option to create a GitHub remote if
  # there is no remote already
  if (!remote_exists) {
    answer_gh <- .projr_init_prompt_yn(
      question = paste0(
        "Do you want to create a GitHub remote and synchronise?\n",
        "Default settings for usethis::use_github (with supplied GitHub user name) will be used." # nolint
      )
    )
  } else {
    answer_gh <- 3
  }

  # initialise the README
  .projr_init_readme_finalise(
    readme = readme,
    path_readme = path_readme,
    nm_list = nm_list,
    gh = FALSE
  )

  # finalise readme and git if no github
  if (answer_git == 1) {
    git_success <- .projr_git_init(dir_proj)
  } else {
    git_success <- TRUE
  }

  # create github remote
  if (answer_gh == 1 && git_success) {
    if (.projr_git_gh_check_auth(nm_list[["gh"]])) {
      try({
        if (identical(nm_list[["gh"]], gh::gh_whoami()$login)) {
          usethis::use_github(private = !public)
        } else {
          usethis::use_github(organisation = nm_list[["gh"]], private = !public)
        }
      })
    }
  }
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

.projr_git_init <- function(dir_proj = NULL) {
  git_available <- .projr_git_init_check()
  if (!git_available) {
    return(invisible(FALSE))
  }
  if (is.null(dir_proj)) {
    dir_proj <- rprojroot::is_r_package$find_file()
  }
  gert::git_init(path = dir_proj)
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    gert::git_config_set("user.name", "Darth Vader")
    gert::git_config_set("user.email", "number_one_fan@tellytubbies.com")
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

.projr_git_init_check <- function() {
  git_version_try <- try(system2("git", args = "--version"), silent = TRUE)
  git_available <- inherits(git_version_try, "try-error")
  if (git_available) {
    return(invisible(TRUE))
  }
  warning(
    "
    Git not found.
    To allow setting up a Git repo, please install Git.
    It's easy - instructions here: https://happygitwithr.com/install-git
    After doing that:
    1. In R, rerun projr::projr_init()
    It will skip what's been done already and try set up Git again."
  )
  invisible(FALSE)
}

.projr_init_gh_check_exists <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  if (file.exists(file.path(dir_proj, ".git"))) {
    remotes <- system2("git", args = "remote", stdout = TRUE)
    if (length(remotes) > 0) {
      return(TRUE)
    }
  }
  invisible(FALSE)
}

.projr_init_git_check_exists <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  dir.exists(file.path(dir_proj, ".git"))
}

.projr_init_gh_check_exists <- function() {
  length(system2("git", args = "remote", stdout = TRUE)) == 0L
}

.projr_git_gh_check_auth <- function() {
  if (!nzchar(.projr_auth_get_github_pat())) {
    return(invisible(TRUE))
  }
  warning(
    paste0(
      "
      GITHUB_PAT environment variable not found.
      To allow creating a GitHub repository, please set it.
      To easily set it in less than two minutes, do the following:
      1. If you do not have a GitHub account, create one here: https://github.com
      2. In R, run usethis::create_github_token()
      3. In R, run gitcreds::gitcreds_set()
      4. Paste the token from step 1 into the R command line (terminal), and press enter
      For more details, see https://happygitwithr.com/https-pat#tldr
      After doing the above:
      1. In R, rerun projr::projr_init()
      It will skip what's been done already and try set up GitHub again."
    )
  )
  invisible(FALSE)
}
