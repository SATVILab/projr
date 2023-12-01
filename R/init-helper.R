# non-engine config files
# -----------------------
.projr_init_yml <- function(yml_path_from) {
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
  yml_path_to <- .projr_dir_proj_get("_projr.yml")
  if (file.exists(yml_path_to)) {
    unlink(yml_path_to)
  }
  file.copy(from = yml_path_from, to = yml_path_to)

  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    path_yml <- .projr_dir_proj_get("_projr.yml")
    yml_projr <- yaml::read_yaml(path_yml)
    if ("build" %in% names(yml_projr)) {
      yml_projr[["build"]][["git"]][["push"]] <- FALSE
      yaml::write_yaml(yml_projr, path_yml)
    }
  }

  invisible(TRUE)
}

.projr_init_prompt_init <- function() {
  # package name
  # document engine and related
  nm_list_engine <- .projr_init_prompt_engine()

  # get metadata
  nm_list_metadata <- .projr_init_prompt_metadata()

  # get README requirements
  nm_list_readme <- .projr_init_prompt_readme(nm_list_metadata)

  # get git and gh settings
  nm_list_git_gh <- .projr_init_prompt_git_gh()

  nm_list_engine |>
    append(nm_list_metadata) |>
    append(nm_list_readme) |>
    append(nm_list_git_gh)
}

.projr_init_description_check_exists <- function() {
  file.exists(.projr_dir_proj_get("DESCRIPTION"))
}

.projr_init_description <- function(nm_list) {
  if (.projr_init_description_check_exists()) {
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
  descrptn$write(file = .projr_dir_proj_get("DESCRIPTION"))
  desc::desc_normalize(.projr_dir_proj_get("DESCRIPTION"))
  .projr_dep_install_only("usethis")
  usethis::proj_activate(.projr_dir_proj_get())
  usethis::use_roxygen_md()
  invisible(TRUE)
}

.projr_init_dep <- function() {
  if (file.exists(.projr_dir_proj_get("_dependencies.R"))) {
    dep <- readLines(.projr_dir_proj_get("_dependencies.R"))
  } else {
    dep <- NULL
  }
  dep <- c(
    dep,
    "library(projr)", ""
  ) |>
    unique()
  writeLines(dep, .projr_dir_proj_get("_dependencies.R"))
  invisible(TRUE)
}

.projr_init_ignore <- function() {
  if (file.exists(.projr_dir_proj_get(".gitignore"))) {
    gitignore <- readLines(.projr_dir_proj_get(".gitignore"))
  } else {
    gitignore <- NULL
  }
  gitignore <- c(
    gitignore,
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "_projr-local.yml", "_environment.local"
  ) |>
    unique()
  writeLines(gitignore, .projr_dir_proj_get(".gitignore"))
  .projr_newline_append(.projr_dir_proj_get(".gitignore"))

  if (file.exists(.projr_dir_proj_get(".Rbuildignore"))) {
    rbuildignore <- readLines(.projr_dir_proj_get(".Rbuildignore"))
  } else {
    rbuildignore <- NULL
  }
  rbuildignore <- c(
    rbuildignore,
    "^.*\\.Rproj$", "^\\.Rproj\\.user$", "^_projr-local\\.yml$"
  ) |>
    unique()
  writeLines(rbuildignore, .projr_dir_proj_get(".Rbuildignore"))
  .projr_newline_append(.projr_dir_proj_get(".Rbuildignore"))

  invisible(TRUE)
}

.projr_init_r <- function() {
  if (!dir.exists(.projr_dir_proj_get("R"))) {
    dir.create(.projr_dir_proj_get("R"))
  }
  invisible(TRUE)
}

.projr_init_renv <- function(force, bioc) {
  renv_init_env_var_lgl <- Sys.getenv("PROJR_TEST") == "TRUE"
  renv_init_exists_lgl <- file.exists(.projr_dir_proj_get("renv.lock"))
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

# engine
# ----------------------
.projr_init_prompt_engine <- function() {
  nm_engine <- .projr_init_prompt_ind_engine()
  nm_format <- .projr_init_prompt_ind_format(nm_engine)
  nm_filename <- .projr_init_prompt_ind_filename(nm_engine)
  list(
    engine = nm_engine,
    format = nm_format,
    filename = nm_filename
  )
}

.projr_init_prompt_ind_engine <- function() {
  if (.projr_init_engine_check_exists()) {
    return(NULL)
  }
  .projr_init_prompt_ind(
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
}

.projr_init_prompt_ind_format <- function(nm_engine) {
  if (.projr_init_engine_check_exists()) {
    return(NULL)
  }
  switch(nm_engine,
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
}

.projr_init_prompt_ind_filename <- function(nm_engine) {
  if (.projr_init_engine_check_exists()) {
    return(NULL)
  }
  switch(nm_engine,
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
}

# metadata
.projr_init_prompt_metadata <- function() {
  nm_pkg <- basename(.projr_dir_proj_get())
  if (!Sys.getenv("PROJR_TEST") == "TRUE") {
    cat("Project name is", paste0("`", nm_pkg, "`"), "\n")
  }
  if (!.projr_init_description_check_exists()) {
    nm_first <- .projr_init_prompt_ind_first()
    nm_last <- .projr_init_prompt_ind_last()
    nm_email <- .projr_init_prompt_ind_email()
    nm_title <- .projr_init_prompt_ind_title()
    nm_license <- .projr_init_prompt_ind_license()
  } else {
    nm_license <- .projr_init_prompt_ind_license(.projr_dir_proj_get())
    nm_license_detail <- !is.null(nm_license) && nm_license == "Proprietary"
    if (nm_license_detail) {
      # if extra details neede
      nm_first <- .projr_init_prompt_ind_first() # nolint
      nm_last <- .projr_init_prompt_ind_last() # nolint
    } else {
      nm_first <- NULL
      nm_last <- NULL
    }
    nm_email <- NULL
    nm_title <- NULL
  }
  list(
    pkg = nm_pkg,
    first = nm_first,
    last = nm_last,
    email = nm_email,
    title = nm_title,
    license = nm_license
  )
}

.projr_init_prompt_ind_first <- function() {
  .projr_init_prompt_ind(
    .var = "FIRST_NAME",
    nm_item_long = "your first name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "FIRST_NAME"
  )
}

.projr_init_prompt_ind_last <- function() {
  .projr_init_prompt_ind(
    .var = "LAST_NAME",
    nm_item_long = "your last name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "LAST_NAME"
  )
}

.projr_init_prompt_ind_email <- function() {
  .projr_init_prompt_ind(
    .var = "EMAIL",
    nm_item_long = "your email address",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "USER@DOMAIN.COM"
  )
}

.projr_init_prompt_ind_title <- function() {
  .projr_init_prompt_ind(
    .var = "TITLE",
    nm_item_long = "the project title",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "TITLE"
  )
}

.projr_init_prompt_ind_license <- function() {
  if (file.exists(.projr_dir_proj_get("LICENSE.md"))) {
    return(NULL)
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

  .projr_init_prompt_ind(
    .var = "LICENSE",
    nm_item_long = "license",
    option_default = option_license_vec,
    allow_specify_other = FALSE,
    allow_complete_later = TRUE,
    answer_translate = long_to_short_license_vec,
    answer_auto = "0"
  )
}

# readme
.projr_init_prompt_readme <- function(nm_list_metadata) {
  answer_readme <- .projr_init_prompt_readme_create()
  readme <- .projr_init_prompt_readme_description(
    answer_readme,
    nm_list_metadata
  )
  list(
    answer_readme = answer_readme,
    readme = readme
  )
}

.projr_init_prompt_readme_create <- function() {
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    return(list("answer_readme" = 2))
  }
  if (.projr_init_prompt_readme_check_exists()) {
    return(list("answer_readme" = 3))
  }
  list(
    "answer_readme" = utils::menu(
      c("Yes (can run R code)", "No (cannot run R code)"),
      title = "Do you want use RMarkdown to create the README?"
    )
  )
}

.projr_init_prompt_readme_check_exists <- function() {
  any(grepl("README\\.md|README\\.Rmd", list.files()))
}

.projr_init_prompt_readme_description <- function(answer_readme,
                                                  nm_list_metadata) {
  # no changes if README already exists
  if (answer_readme == 3) {
    return(.projr_init_prompt_readme_finalise())
  }

  # get fn_readme and path_readme
  fn_readme <- paste0("README.", ifelse(answer_readme == 1, "Rmd", "md"))
  path_readme <- .projr_dir_proj_get(fn_readme)
  readme <- readLines(path_readme)

  # get where to add to
  readme_ind_example <- which(grepl("^## Example", readme))
  # exclude the example section
  readme <- readme[seq_len(readme_ind_example - 1)]

  readme_rep <- .projr_init_prompt_readme_description_get(
    nm_list_metadata[["pkg"]], answer_readme
  )
  answer_readme_correct <-
    .projr_init_prompt_readme_description_check(readme_rep)

  while (answer_readme_correct == 2) {
    readme_rep <- .projr_init_prompt_readme_description_get(
      nm_list_metadata[["pkg"]], answer_readme
    )
    answer_readme_correct <-
      .projr_init_prompt_readme_description_check(readme_rep)
  }

  if (answer_readme_correct == 3) {
    return(.projr_init_prompt_readme_finalise())
  }

  readme[readme_ind_example - 1] <- readme_rep
  readme <- c(readme, "")
  writeLines(readme, path_readme)

  .projr_init_readme_finalise()

  readme
}

.projr_init_prompt_readme_description_get <- function(pkg, answer_readme) {
  cat(
    "Please finish the following sentence:\n",
    paste0("The purpose of ", pkg, " is to...")
  )
  readme_add <- readline(prompt = ">> ")
  paste0("The purpose of ", pkg, " is to ", readme_add)
}

.projr_init_prompt_readme_description_check <- function(readme_rep) {
  utils::menu(
    c("Yes", "No", "Complete later"),
    title =
      paste0(
        "Is the following completed sentence/paragraph correct:\n",
        readme_rep
      )
  )
}
.projr_init_prompt_readme_finalise <- function() {
  if (!file.exists(.projr_dir_proj_get("README.Rmd"))) {
    return(invisible(FALSE))
  }
  try(rmarkdown::render(
    .projr_dir_proj_get("README.Rmd"),
    output_format = "md_document", quiet = TRUE
  ))
  invisible(TRUE)
}


# git and github
.projr_init_prompt_git_gh <- function() {
  # whether to include Git
  answer_git <- .projr_init_prompt_git_gh_answer_git()
  # whether a Git remote is defined already
  gh_exists <- .projr_init_projt_git_gh_gh_exists(answer_git)
  # get username
  nm_gh <- .projr_init_prompt_git_gh_username(answer_git, gh_exists)
  list(
    answer_git = answer_git, gh = nm_gh
  )
}

.projr_init_prompt_git_gh_answer_git <- function() {
  if (.projr_init_git_check_exists()) {
    return(3)
  }
  .projr_init_prompt_yn(
    question = "Do you want to initialise a Git repo now?",
    answer_auto = 1
  )
}

.projr_init_projt_git_gh_gh_exists <- function(answer_git) {
  if (answer_git == 2) {
    return(invisible(FALSE))
  }
  if (.projr_init_git_check_exists()) {
    gh_exists <- .projr_init_gh_check_exists()
  } else {
    gh_exists <- FALSE
  }
  gh_exists
}

.projr_init_prompt_git_gh_username <- function(answer_git, gh_exists) {
  ask <- !gh_exists & answer_git %in% c(1, 3)
  if (!ask) {
    return(NULL)
  }
  answer_gh <- .projr_init_prompt_yn(
    question = paste0(
      "Do you want to create a GitHub remote and synchronise?\n",
      "Default settings for usethis::use_github will be used." # nolint
    )
  )
  if (answer_gh == 2) {
    return(NULL)
  }
  .projr_init_prompt_ind(
    .var = "GITHUB_USER_NAME",
    nm_item_long = "GitHub user/organisation name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "GITHUB_USER_NAME"
  )
}

# ============================
# initialisation
# ============================

.projr_init_license <- function(nm_list) {
  .projr_init_license_create(
    x = nm_list[["nm_license"]],
    nm_first = nm_list[["first"]],
    nm_last = nm_list[["last"]]
  )

  invisible(TRUE)
}

.projr_init_readme <- function(nm_list) {
  # automatic for testing
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    .projr_init_readme_auto()
    return(.projr_init_readme_auto())
  }
  # pre-existing README
  readme_list <- .projr_init_readme_pre_existing()
  if (!is.null(readme_list[["readme"]])) {
    return(readme_list)
  }
  # create afresh
  .projr_init_readme_create()
}

.projr_init_readme_auto <- function() {
  .projr_dep_install_only("usethis")
  usethis::use_readme_md(open = FALSE)
  answer_readme <- 2
  fn_readme <- paste0("README.", ifelse(answer_readme == 1, "Rmd", "md"))
  path_readme <- .projr_dir_proj_get(fn_readme)
  readme <- readLines(path_readme)
  list(readme = readme, path_readme = path_readme)
}

.projr_init_readme_pre_existing <- function() {
  fn_vec <- list.files(.projr_dir_proj_get())
  if (any(grepl("^README\\.md$", fn_vec))) {
    fn_readme <- "README.md"
    path_readme <- .projr_dir_proj_get(fn_readme)
    readme <- readLines(path_readme)
    return(list(readme = readme, path_readme = path_readme))
  } else if (any(grepl("^README\\.Rmd$", fn_vec))) {
    fn_readme <- "README.Rmd"
    path_readme <- .projr_dir_proj_get(fn_readme)
    readme <- readLines(path_readme)
    return(list(readme = readme, path_readme = path_readme))
  }
  list(readme = NULL, path_readme = NULL)
}

.projr_init_readme_create <- function(answer_readme) {
  if (answer_readme == 1) {
    .projr_dep_install_only("usethis")
    usethis::use_readme_rmd(open = FALSE)
  } else if (answer_readme == 2) {
    .projr_dep_install_only("usethis")
    usethis::use_readme_md(open = FALSE)
  }
}

.projr_init_license_create <- function(x, nm_first, nm_last) {
  if (!is.null(x)) {
    .projr_dep_install_only("usethis")
  }
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



# git
.projr_init_git_init <- function(answer_git) {
  if (answer_git %in% c(2, 3)) {
    .projr_init_git_suggest_git()
    return(invisible(FALSE))
  }
  .projr_git_system_setup()
  .projr_git_init()
  .projr_init_git_commit()
  .projr_init_git_suggest_git()
  invisible(TRUE)
}

.projr_init_git_commit <- function() {
  fn_vec <- .projr_init_git_file_get()
  if (length(fn_vec) == 0L) {
    return(invisible(FALSE))
  }
  .projr_git_commit_file(fn_vec, msg = "Initial projr commit")
}

.projr_init_git_file_get <- function() {
  fn_vec <- c(
    "DESCRIPTION",
    ".Rbuildignore",
    ".gitignore",
    "_dependencies.R",
    list.files("renv", recursive = TRUE),
    "renv.lock",
    ".Rprofile",
    "_projr.yml",
    "_quarto.yml",
    "_bookdown.yml",
    "README.md",
    "README.Rmd"
  )
  fn_vec <- fn_vec[file.exists(
    vapply(fn_vec, .projr_dir_proj_get, FUN.VALUE = logical(1))
  )]
  fn_vec[
    fn_vec %in% c(.projr_git_modified_get(), .projr_git_new_get())
  ]
}

.projr_init_git_suggest_git <- function() {
  if (.projr_git_system_get() == "git") {
    return(invisible(FALSE))
  }
  message("
    You can use `projr` without the program git
    (and setup is going fine!),
    but RStudio (and VS Code) like it.
    It's easy to install, instructions here:
    https://happygitwithr.com/install-git")
}
