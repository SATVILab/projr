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
    to = "_projr.yml"
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
  cat("Please provide a name for the project.\n")
  nm_pkg <- readline(prompt = ">> ")
  answer_pkg <- menu(
    c("Yes", "No"),
    title = paste0("Is the project name `", nm_pkg, "` correct?")
  )
  while (answer_pkg == 2) {
    cat("Please provide a name for the project.\n")
    nm_pkg <- readline(prompt = ">> ")
    answer_pkg <- menu(
      c("Yes", "No"),
      title = paste0("Is the project name `", nm_pkg, "` correct?")
    )
  }

  # please provide the GitHub user name
  cat("Please provide the GitHub user/organisation name for this project.\n")
  nm_gh <- readline(prompt = ">> ")
  answer_gh <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the GitHub user/organisation name `", nm_gh, "` correct?")
  )
  while (answer_gh == 2) {
    cat("Please provide the GitHub user/organisation name.\n")
    nm_gh <- readline(prompt = ">> ")
    answer_gh <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the GitHub user/organisation name `", nm_gh, "` correct?")
    )
  }
  if (answer_gh == 3) {
    nm_gh <- "{{ GitHub user/organisation name }}"
  }

  # personal details: first name
  cat("Please provide your first name.\n")
  nm_first <- readline(prompt = ">> ")
  answer_first <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the first name `", nm_first, "` correct?")
  )
  while (answer_first == 2) {
    cat("Please provide your first name.\n")
    nm_first <- readline(prompt = ">> ")
    answer_first <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the first name `", nm_first, "` correct?")
    )
  }
  if (answer_first == 3) {
    nm_first <- "{{ First name }}"
  }

  # personal details: surname
  cat("Please provide your surname (last/family name).\n")
  nm_last <- readline(prompt = ">> ")
  answer_last <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the surname `", nm_last, "` correct?")
  )
  while (answer_last == 2) {
    cat("Please provide your surname (last/family name).\n")
    nm_last <- readline(prompt = ">> ")
    answer_last <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the surname `", nm_last, "` correct?")
    )
  }
  if (answer_last == 3) {
    nm_last <- "{{ Surname }}"
  }

  # personal details: email address
  cat("Please provide your email address.\n")
  nm_email <- readline(prompt = ">> ")
  answer_email <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the email address `", nm_email, "` correct?")
  )
  while (answer_email == 2) {
    cat("Please provide your email address.\n")
    nm_email <- readline(prompt = ">> ")
    answer_email <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the email address `", nm_email, "` correct?")
    )
  }
  if (answer_email == 3) {
    nm_email <- "{{ Surname }}"
  }


  # project title
  cat("Please provide a short project title (<30 characters).\n")
  nm_title <- readline(prompt = ">> ")
  answer_title <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the project title `", nm_title, "` correct?")
  )
  while (answer_title == 2) {
    cat("Please provide a short project title (<30 characters).\n")
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
  cat("Please provide a sentence or two describing the project.\n")
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
    cat("Please provide a sentence or two describing the project.\n")
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
    c("Yes - RMarkdown format (can run R code)", "Yes - Markdown format (cannot run R code)", "No"),
    title =
      paste0(
        "Do you want to create a README now?"
      )
  )
  switch(as.character(answer_readme),
    "1" = {
      usethis::use_readme_rmd()
      readme <- readLines("README.Rmd")
    },
    "2" = {
      usethis::use_readme_md()
      readme <- readLines("README.md")
    }
  )

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
      answer_readme <- menu(
        c("Yes", "No", "Complete later"),
        title =
          paste0(
            "Is the following completed sentence/paragraph correct:\n",
            readme_rep
          )
      )
      while (answer_readme == 2) {
        cat(
          "Please finish the following sentence/paragraph:\n",
          paste0("The purpose of ", nm_pkg, " is to...")
        )
        nm_readme <- readline(prompt = ">> ")
        readme_ind <- which(grepl("^The goal of ", readme))
        readme_rep <- paste0("The goal of ", nm_pkg, " is to ", nm_readme)
        answer_readme <- menu(
          c("Yes", "No", "Complete later"),
          title =
            paste0(
              "Is the following completed sentence/paragraph correct:\n",
              readme_rep
            )
        )
      }
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
      writeLines(
        readme,
        file.path(
          dir_proj, "README.", ifelse(answer_readme == 1, "Rmd", "md")
        )
      )
    }
    return(TRUE)
  }

  usethis::use_git()

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
      writeLines(
        readme,
        file.path(
          dir_proj, "README.", ifelse(answer_readme == 1, "Rmd", "md")
        )
      )
    }
    return(TRUE)
  }

  if (answer_readme %in% c(1, 2)) {
    readme_ind_install <- which(grepl("^You can install", readme))
    readme_install_devtools <-
      'if (!requireNamespace("devtools")) install.packages("devtools"))'
    readme_install_pkg <-
      paste0('devtools::install_github("', nm_gh, "/", nm_pkg, '")')

    readme[readme_ind_install + 3] <- readme_install_devtools
    readme[readme_ind_install + 5] <- readme[readme_ind_install + 4]
    readme[readme_ind_install + 4] <- readme_install_pkg
    writeLines(
      readme,
      file.path(
        dir_proj, "README.", ifelse(answer_readme == 1, "Rmd", "md")
      )
    )
  }

  usethis::use_github(
    organisation = nm_gh,
    private = TRUE
  )




  invisible(TRUE)
}
