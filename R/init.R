#' @title Initialise project
#'
#' @description Initialise project
#'
#' @param renv_force. Logical.
#' Passed to `renv::init()`.
#' If \code{FALSE}, then `renv::init()` will not run
#' if it detects that the working directory
#' already is registered with renv.
#' Default is \code{FALSE}.
#' @param renv_bioconductor. Logical.
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

  renv::init(
    force = renv_force,
    bioconductor = renv_bioconductor
  )



  invisible(TRUE)
}

.projr_init_prompt <- function(dir_proj) {

  # prompts
  # ----------------------

  # package name
  nm_pkg <- basename(dir_proj)
  cat("Project name is", paste0("`", nm_pkg, "`"), "\n")

  # please provide the GitHub user name
  if (nzchar(Sys.getenv("PROJR_GITHUB_USERNAME"))) {
    nm_gh <- strsplit(Sys.getenv("PROJR_GITHUB_USERNAME"), ";")[[1]]
    answer_gh <- menu(
      c(nm_gh, "Specify other", "Complete later"),
      title = "Please select GitHub user/organisation name for this project"
    )
    cat("\n")
    if (answer_gh %in% seq_along(nm_gh)) {
      nm_gh <- nm_gh[answer_gh]
      ask_gh <- FALSE
      completed_gh <- TRUE
    } else if (answer_gh == length(nm_gh) + 1) {
      cat("Please provide the GitHub user/organisation name for this project.\n") # nolint
      nm_gh <- readline(prompt = ">> ")
      cat("\n")
      ask_gh <- TRUE
    } else {
      ask_gh <- FALSE
      completed_gh <- FALSE
    }
  } else {
    cat("Please provide the GitHub user/organisation name for this project.\n")
    nm_gh <- readline(prompt = ">> ")
    cat("\n")
    ask_gh <- TRUE
  }

  if (ask_gh) {
    answer_gh <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0(
        "Is the GitHub user/organisation name `", nm_gh, "` correct?"
      )
    )
    cat("\n")
    ask_gh <- answer_gh == 2
    completed_gh <- answer_gh != 3
  }

  while (ask_gh) {
    cat("Please provide the GitHub user/organisation name.\n")
    nm_gh <- readline(prompt = ">> ")
    answer_gh <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0(
        "Is the GitHub user/organisation name `", nm_gh, "` correct?"
      )
    )
    cat("\n")
    ask_gh <- answer_gh == 2
    completed_gh <- answer_gh != 3
  }
  if (!completed_gh) {
    nm_gh <- "{{ GitHub user/organisation name }}"
  }

  # first name
  if (nzchar(Sys.getenv("PROJR_FIRST_NAME"))) {
    nm_first <- strsplit(Sys.getenv("PROJR_FIRST_NAME"), ";")[[1]]
    answer_first <- menu(
      c(nm_first, "Specify other", "Complete later"),
      title = "Please select your first name."
    )
    cat("\n")
    if (answer_first %in% seq_along(nm_first)) {
      nm_first <- nm_first[answer_first]
      ask_first <- FALSE
      completed_first <- TRUE
    } else if (answer_first == length(nm_first) + 1) {
      cat("Please provide your first name.\n") # nolint
      nm_first <- readline(prompt = ">> ")
      cat("\n")
      ask_first <- TRUE
    } else {
      ask_first <- FALSE
      completed_first <- FALSE
    }
  } else {
    cat("Please provide your first name.\n")
    nm_first <- readline(prompt = ">> ")
    cat("\n")
    ask_first <- TRUE
  }

  if (ask_first) {
    answer_first <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the first name `", nm_first, "` correct?")
    )
    cat("\n")
    ask_first <- answer_first == 2
    completed_first <- answer_first != 3
  }

  while (ask_first) {
    cat("Please provide your first name.\n")
    nm_first <- readline(prompt = ">> ")
    answer_first <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the first name `", nm_first, "` correct?")
    )
    cat("\n")
    ask_first <- answer_first == 2
    completed_first <- answer_first != 3
  }

  if (!completed_first) {
    nm_first <- "{{ First name }}"
  }

  # last name
  if (nzchar(Sys.getenv("PROJR_LAST_NAME"))) {
    nm_last <- strsplit(Sys.getenv("PROJR_LAST_NAME"), ";")[[1]]
    answer_last <- menu(
      c(nm_last, "Specify other", "Complete later"),
      title = "Please select your surname (last/family name)."
    )
    cat("\n")
    if (answer_last %in% seq_along(nm_last)) {
      nm_last <- nm_last[answer_last]
      ask_last <- FALSE
      completed_last <- TRUE
    } else if (answer_last == length(nm_last) + 1) {
      cat("Please provide your surname.\n") # nolint
      nm_last <- readline(prompt = ">> ")
      cat("\n")
      ask_last <- TRUE
    } else {
      ask_last <- FALSE
      completed_last <- FALSE
    }
  } else {
    cat("Please provide your surname (last/family name).\n")
    nm_last <- readline(prompt = ">> ")
    cat("\n")
    ask_last <- TRUE
  }

  if (ask_last) {
    answer_last <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the surname `", nm_last, "` correct?")
    )
    cat("\n")
    ask_last <- answer_last == 2
    completed_last <- answer_last != 3
  }

  while (ask_last) {
    cat("Please provide your surname.\n")
    nm_last <- readline(prompt = ">> ")
    answer_last <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the surname `", nm_last, "` correct?")
    )
    cat("\n")
    ask_last <- answer_last == 2
    completed_last <- answer_last != 3
  }

  if (!completed_last) {
    nm_last <- "{{ Surname }}"
  }

  # email
  if (nzchar(Sys.getenv("PROJR_EMAIL"))) {
    nm_email <- strsplit(Sys.getenv("PROJR_EMAIL"), ";")[[1]]
    answer_email <- menu(
      c(nm_email, "Specify other", "Complete later"),
      title = "Please select your email address."
    )
    cat("\n")
    if (answer_email %in% seq_along(nm_email)) {
      nm_email <- nm_email[answer_email]
      ask_email <- FALSE
      completed_email <- TRUE
    } else if (answer_email == length(nm_email) + 1) {
      cat("Please provide your email address.\n") # nolint
      nm_email <- readline(prompt = ">> ")
      cat("\n")
      ask_email <- TRUE
    } else {
      ask_email <- FALSE
      completed_email <- FALSE
    }
  } else {
    cat("Please provide your email address.\n")
    nm_email <- readline(prompt = ">> ")
    cat("\n")
    ask_email <- TRUE
  }

  if (ask_email) {
    answer_email <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the email address `", nm_email, "` correct?")
    )
    cat("\n")
    ask_email <- answer_email == 2
    completed_email <- answer_email != 3
  }

  while (ask_email) {
    cat("Please provide your email address.\n")
    nm_email <- readline(prompt = ">> ")
    answer_email <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the email address `", nm_email, "` correct?")
    )
    cat("\n")
    ask_email <- answer_email == 2
    completed_email <- answer_email != 3
  }

  if (!completed_email) {
    nm_email <- "{{ Email address }}"
  }

  # project title
  cat("Please provide a short project title (<30 characters, initial capital and no full stop).\n") # nolint
  nm_title <- readline(prompt = ">> ")
  answer_title <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the project title `", nm_title, "` correct?")
  )
  while (answer_title == 2) {
    cat("Please provide a short project title (<30 characters, initial capital and no full stop).\n") # nolint
    nm_title <- readline(prompt = ">> ")
    answer_title <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the project title `", nm_title, "` correct?")
    )
  }
  if (answer_title == 3) {
    nm_title <- "{{ Project title }}"
  }

  # project title
  cat("Please provide a sentence or two describing the project (initial capital and a full stop).\n")
  nm_desc <- readline(prompt = ">> ")
  answer_desc <- menu(
    c("Yes", "No", "Complete later"),
    title =
      paste0(
        "Is the following project description correct:\n",
        nm_desc
      )
  )
  while (answer_desc == 2) {
    cat("Please provide a sentence or two describing the project (initial capital and a full stop).\n")
    nm_desc <- readline(prompt = ">> ")
    answer_desc <- menu(
      c("Yes", "No", "Complete later"),
      title =
        paste0(
          "Is the following project description correct:\n",
          nm_desc,
          "`"
        )
    )
  }
  if (answer_desc == 3) {
    nm_desc <- "{{ Description }}"
  }

  # writing
  # ----------------------
  if (!dir.exists(dir_proj)) {
    dir.create(dir_proj, recursive = TRUE)
  }

  # bookdown.yml
  bd <- yaml::read_yaml(system.file(
    "project_structure", "_bookdown.yml",
    package = "projr"
  ))
  version_init <- "V0.0.0-1"
  bd$book_filename <- paste0(nm_pkg, version_init)
  bd$output_dir <- paste0("docs/", nm_pkg, version_init)
  yaml::write_yaml(bd, file.path(dir_proj, "_bookdown.yml"))

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

  # DESCRIPTION
  descrptn <- desc::description$new("!new")
  descrptn$set("Package", nm_pkg)
  descrptn$set("Title", nm_title)
  descrptn$set("Version", "0.0.0-1")
  descrptn$set(
    "Maintainer",
    paste0(nm_first, " ", nm_last, " <", nm_email, ">")
  )
  descrptn$del("Authors@R")
  descrptn$add_author(
    nm_first,
    nm_last,
    email = nm_email
  )

  descrptn$set(
    "BugReports",
    paste0("https://github.com/", nm_gh, "/", nm_pkg, "/issues")
  )
  descrptn$set(
    "URL",
    paste0("https://github.com/", nm_gh, "/", nm_pkg, "/#readme")
  )
  descrptn$set("Description", nm_desc)
  descrptn$write(file = file.path(dir_proj, "DESCRIPTION"))
  desc::desc_normalize(file.path(dir_proj, "DESCRIPTION"))
  usethis::proj_activate(dir_proj)
  usethis::use_roxygen_md()



  # index.Rmd
  index <- readLines(system.file(
    "project_structure", "index.Rmd",
    package = "projr"
  ))
  index[2] <- paste0("title: ", nm_pkg)
  author_ind <- which(grepl("^author", index))
  index[author_ind] <- paste0("author: ", nm_first, " ", nm_last)
  description_ind <- which(grepl("^description", index))
  index[description_ind] <- paste0("description: ", nm_desc)
  writeLines(index, file.path(dir_proj, "index.Rmd"))

  # appendix.Rmd
  file.copy(
    system.file(
      "project_structure",
      "appendix.Rmd",
      package = "projr"
    ),
    dir_proj
  )

  if (!file.exists(file.path(dir_proj, ".gitignore"))) {
    file.copy(
      system.file(
        "project_structure",
        ".gitignore",
        package = "projr"
      ),
      dir_proj
    )
  }

  if (!file.exists(file.path(dir_proj, ".Rbuildignore"))) {
    file.copy(
      system.file(
        "project_structure",
        ".Rbuildignore",
        package = "projr"
      ),
      dir_proj
    )
  }

  # README
  # ------------------------

  #
  answer_readme <- menu(
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
      answer_goal <- menu(
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
        answer_goal <- menu(
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

  # License
  # ----------------------
  # project description
  nm_license <- menu(
    c(
      "CC-BY (good for data and analysis projects - permissive, but requires attribution)", # nolint
      "Apache 2.0 (good for function packages - permissive with patent protection)", # nolint
      "CC0 (dedicates to public domain)",
      "Proprietary (private code, i.e. no-one may use, share or change it)",
      "Complete later"
    ),
    title = paste0("Please select a license.")
  )
  if (!nm_license == 5) {
    answer_license <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0(
        "Is the licence `",
        switch(as.character(nm_license),
          "1" = "CC-BY",
          "2" = "Apache 2.0",
          "3" = "CC0",
          "4" = "Proprietary"
        ),
        "` correct?"
      )
    )
    if (answer_license == 3) {
      nm_license == 5
    }
    while (answer_license == 2) {
      nm_license <- menu(
        c(
          "CC-BY (good for data and analysis projects - permissive, but requires attribution)", # nolint
          "Apache 2.0 (good for function packages - permissive with patent protection)", # nolint
          "CC0 (dedicates to public domain)",
          "Proprietary (private code, i.e. no-one may use, share or change it)",
          "Complete later"
        ),
        title = paste0("Please select a license.")
      )
      if (nm_license == 5) {
        answer_license <- 3
      } else {
        answer_license <- menu(
          c("Yes", "No", "Complete later"),
          title = paste0(
            "Is the licence `",
            switch(as.character(nm_license),
              "1" = "CC-BY",
              "2" = "Apache 2.0",
              "3" = "CC0",
              "4" = "Proprietary"
            ),
            "` correct?"
          )
        )
      }
    }
    if (answer_license == 1) {
      switch(as.character(nm_license),
        "1" = usethis::use_ccby_license(),
        "2" = usethis::use_apache_license(),
        "3" = usethis::use_cc0_license(),
        "4" = usethis::use_proprietary_license(paste0(nm_first, " ", nm_last))
      )
    }
  }

  # Git and GitHub
  # -------------------

  # Git
  answer_git <- menu(
    c("Yes", "No"),
    title =
      paste0(
        "Do you want to initialise a Git repo now?"
      )
  )
  if (answer_git == 2) {
    file.copy(
      system.file(
        "project_structure",
        "DELETE-AFTER-DOING.md",
        package = "projr"
      ),
      dir_proj
    )
    cat("\n")
    cat("\n")
    message("Follow steps in DELETE-AFTER-DOING.md")

    if (answer_readme %in% c(1, 2)) {
      if (file.exists(path_readme)) unlink(path_readme)
      if (!identical(readme[length(readme)], "")) readme <- c(readme, "")
      writeLines(text = readme, con = path_readme)
      if (answer_readme == 1) {
        try(rmarkdown::render(
          file.path(dir_proj, "README.Rmd"),
          output_format = "md_document",
          quiet = TRUE
        ))
      }
    }
    return(TRUE)
  }

  # GitHub
  answer_gh <- menu(
    c("Yes", "No"),
    title =
      paste0(
        "Do you want to create a GitHub remote and synchronise?\n",
        "Default settings for usethis::use_github (with supplied GitHub user name) will be used." # nolint
      )
  )
  if (answer_gh == 2) {
    file.copy(
      system.file(
        "project_structure",
        "DELETE-AFTER-DOING.md",
        package = "projr"
      ),
      dir_proj
    )
    cat("\n")
    cat("\n")
    message("Follow steps in DELETE-AFTER-DOING.md")

    if (answer_readme %in% c(1, 2)) {
      if (file.exists(path_readme)) unlink(path_readme)
      if (!identical(readme[length(readme)], "")) readme <- c(readme, "")
      writeLines(text = readme, con = path_readme)
      if (answer_readme == 1) {
        try(
          rmarkdown::render(
            file.path(dir_proj, "README.Rmd"),
            output_format = "md_document",
            quiet = TRUE
          )
        )
      }
    }

    # taken from usethis::use_git
    gert::git_init(path = dir_proj)
    gert::git_add(list.files(dir_proj))
    gert::git_commit_all(message = "Initial commit")
    return(TRUE)
  }

  if (answer_readme %in% c(1, 2)) {
    if (file.exists(path_readme)) unlink(path_readme)
    readme_ind_install <- which(grepl("^You can install", readme))
    readme_install_devtools <-
      'if (!requireNamespace("devtools")) install.packages("devtools")'
    readme_install_pkg <-
      paste0('devtools::install_github("', nm_gh, "/", nm_pkg, '")')

    readme[readme_ind_install + 3] <- readme_install_devtools
    readme[readme_ind_install + 5] <- readme[readme_ind_install + 4]
    readme[readme_ind_install + 4] <- readme_install_pkg
    if (!identical(readme[length(readme)], "")) readme <- c(readme, "")
    writeLines(text = readme, con = path_readme)

    if (answer_readme == 1) {
      try(
        rmarkdown::render(
          file.path(dir_proj, "README.Rmd"),
          output_format = "md_document",
          quiet = TRUE
        )
      )
    }
  }


  # taken from usethis::use_git
  gert::git_init(path = dir_proj)
  gert::git_add(setdiff(list.files(dir_proj, all.files = TRUE), c(".", "..")))
  gert::git_commit_all(message = "Initial commit")

  try(usethis::use_github(
    organisation = nm_gh,
    private = TRUE
  ))

  invisible(TRUE)
}
