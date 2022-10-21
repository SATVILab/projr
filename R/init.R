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
  usethis::proj_activate(getwd())

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
  cat("Please provide the GitHub user name.\n")
  nm_gh <- readline(prompt = ">> ")
  answer_gh <- menu(
    c("Yes", "No", "Complete later"),
    title = paste0("Is the  GitHub user name `", nm_gh, "` correct?")
  )
  while (answer_gh == 2) {
    cat("Please provide the GitHub user name.\n")
    nm_gh <- readline(prompt = ">> ")
    answer_gh <- menu(
      c("Yes", "No", "Complete later"),
      title = paste0("Is the  GitHub user name `", nm_gh, "` correct?")
    )
  }
  if (answer_gh == 3) {
    nm_gh <- "{{ GitHub user name }}"
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
        "Is the following project description correct:\n`",
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
          "Is the following project description correct:\n`",
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
  usethis::use_roxygen_md()

  # README
  readme <- readLines(system.file(
    "project_structure", "README.md",
    package = "projr"
  ))

  readme[1] <- paste0("# ", nm_pkg)
  writeLines(readme, file.path(dir_proj, "README.md"))

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
        "Is the lisence `",
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
        "Do you want to initialise a Git repo now?\n`",
        nm_desc
      )
  )
  if (answer_git == "no") {
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
    return(TRUE)
  }

  # GitHub
  answer_gh <- menu(
    c("Yes", "No"),
    title =
      paste0(
        "Do you want to create a GitHub remote and synchronise?\n`",
        nm_desc
      )
  )
  if (answer_gh == "no") {
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
    return(TRUE)
  }

  usethis::use_github(
    organisation = nm_gh,
    private = TRUE
  )


  invisible(TRUE)
}
