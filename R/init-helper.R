# _projr.yml
# -----------------------

# set _projr.yml
.projr_init_yml <- function(yml_path_from = NULL) {
  .projr_init_yml_copy(.projr_init_yml_get_path_from(yml_path_from))
  .projr_init_yml_test_unset_push()
  invisible(TRUE)
}

.projr_init_yml_get_path_from <- function(yml_path_from) {
  # get path
  yml_path_from <- yml_path_from %||%
    .projr_init_yml_get_path_from_auto()
  # check path
  .projr_init_yml_check_path_from(yml_path_from)
  yml_path_from
}

.projr_init_yml_get_path_from_auto <- function() {
  if (nzchar(Sys.getenv("PROJR_PATH_YML"))) {
    return(Sys.getenv("PROJR_PATH_YML"))
  }
  system.file("project_structure", "_projr.yml", package = "projr")
}

.projr_init_yml_check_path_from <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("yml_path_from does not exist: ", path))
  }
  if (inherits(try(yaml::read_yaml(path), silent = TRUE), "try-error")) {
    stop(paste0("yml_path_from is not a valid YAML file: ", path))
  }
  invisible(TRUE)
}

.projr_init_yml_copy <- function(path) {
  if (file.exists(.dir_proj_get("_projr.yml"))) {
    return(invisible(FALSE))
  }
  file.copy(from = path, to = .dir_proj_get("_projr.yml"))
  invisible(TRUE)
}

.projr_init_yml_test_unset_push <- function() {
  # HERE
  if (!.is_test()) {
    return(invisible(FALSE))
  }
  path_yml <- .dir_proj_get("_projr.yml")
  yml_projr <- yaml::read_yaml(path_yml)
  if (!"build" %in% names(yml_projr)) {
    return(invisible(FALSE))
  }
  .projr_yml_git_set_push(
    FALSE,
    simplify_default = TRUE, profile = "default"
  )
  invisible(TRUE)
}

# gather metadata and tool selection
# ----------------------------
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



.projr_init_description <- function(nm_list) {
  if (!.projr_init_description_check()) {
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
  descrptn$write(file = .dir_proj_get("DESCRIPTION"))
  desc::desc_normalize(.dir_proj_get("DESCRIPTION"))
  .projr_dep_install_only("usethis")
  usethis::proj_activate(.dir_proj_get())
  usethis::use_roxygen_md()
  invisible(TRUE)
}

.projr_init_description_check <- function() {
  !.is_file_exists_description()
}

.projr_init_dep <- function() {
  if (file.exists(.dir_proj_get("_dependencies.R"))) {
    dep <- readLines(.dir_proj_get("_dependencies.R"))
  } else {
    dep <- NULL
  }
  dep <- c(
    dep,
    "library(projr)", ""
  ) |>
    unique()
  writeLines(dep, .dir_proj_get("_dependencies.R"))
  invisible(TRUE)
}

.projr_init_ignore <- function() {
  if (file.exists(.dir_proj_get(".gitignore"))) {
    gitignore <- readLines(.dir_proj_get(".gitignore"))
  } else {
    gitignore <- NULL
  }
  gitignore <- c(
    gitignore,
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "_projr-local.yml", "_environment.local"
  ) |>
    unique()
  writeLines(gitignore, .dir_proj_get(".gitignore"))
  .projr_newline_append(.dir_proj_get(".gitignore"))

  if (file.exists(.dir_proj_get(".Rbuildignore"))) {
    rbuildignore <- readLines(.dir_proj_get(".Rbuildignore"))
  } else {
    rbuildignore <- NULL
  }
  rbuildignore <- c(
    rbuildignore,
    "^.*\\.Rproj$", "^\\.Rproj\\.user$", "^_projr-local\\.yml$"
  ) |>
    unique()
  writeLines(rbuildignore, .dir_proj_get(".Rbuildignore"))
  .projr_newline_append(.dir_proj_get(".Rbuildignore"))

  invisible(TRUE)
}

.projr_init_r <- function() {
  if (dir.exists(.dir_proj_get("R"))) {
    return(invisible(FALSE))
  }
  dir.create(.dir_proj_get("R"))
  invisible(TRUE)
}

.projr_init_renv <- function(force, bioc, skip_init = .is_test()) {
  if (skip_init || .projr_renv_detect()) {
    return(invisible(TRUE))
  }
  .projr_renv_init_rscript_actual(force, bioc)
  try(source("renv/activate.R"), silent = TRUE)
  # activate `renv`.
  # wrapped in `try` in case something goes wrong.
  # User can just restart `R` to activate `renv`
  # if it goes wrong, so no error-handling attempted.
  invisible(TRUE)
}

.projr_renv_init_rscript_actual <- function(force, bioc) {
  cmd_txt <- paste0(
    "-e '",
    "renv::init(",
    'settings = list(snapshot.type = "implicit"), ',
    "force = ", force, ", ",
    "bioconductor = ", bioc,
    ")'"
  )
  system2(
    .projr_path_rscript_get(),
    args = cmd_txt, stdout = FALSE
  )
  invisible(TRUE)
}

.projr_renv_detect <- function() {
  .projr_renv_lockfile_path_get() |> file.exists()
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
  answer_auto <- Sys.getenv(
    "PROJR_TEST_ENGINE",
    unset = "Bookdown project"
  )
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
    answer_auto = answer_auto
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
  nm_pkg <- basename(.dir_proj_get())
  if (!.is_test()) {
    cat("Project name is", paste0("`", nm_pkg, "`"), "\n")
  }
  if (!.is_file_exists_description()) {
    nm_first <- .projr_init_prompt_ind_first()
    nm_last <- .projr_init_prompt_ind_last()
    nm_email <- .projr_init_prompt_ind_email()
    nm_title <- .projr_init_prompt_ind_title()
    nm_license <- .projr_init_prompt_ind_license()
  } else {
    nm_license <- .projr_init_prompt_ind_license()
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
    nm_title <- "TITLE"
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
  if (file.exists(.dir_proj_get("LICENSE.md"))) {
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
    answer_auto = "1"
  )
}

# readme
.projr_init_prompt_readme <- function(nm_list_metadata) {
  answer_readme <- .projr_init_prompt_readme_create()
  desc_exists_pre <- .projr_desc_tmp_create()
  .projr_init_readme_create(answer_readme)
  .projr_desc_tmp_remove(desc_exists_pre)
  readme <- .projr_init_prompt_readme_description(
    answer_readme,
    nm_list_metadata
  )
  list(
    answer_readme = answer_readme,
    readme = readme
  )
}

.projr_desc_tmp_create <- function() {
  path_desc <- .dir_proj_get("DESCRIPTION")
  desc_exists_pre <- file.exists(path_desc)
  if (!desc_exists_pre) {
    desc::description$new("!new")$write(file = path_desc)
  }
  desc_exists_pre
}

.projr_desc_tmp_remove <- function(desc_exists_pre) {
  if (!desc_exists_pre) {
    .file_rm(.dir_proj_get("DESCRIPTION"))
  }
}

.projr_init_prompt_readme_create <- function() {
  if (.is_test()) {
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
                                                  nm_list) {
  # no changes if README already exists
  if (answer_readme == 3) {
    return(.projr_readme_render())
  }

  # get fn_readme and path_readme
  readme <- .projr_readme_read() |>
    .projr_readme_filter_example() |>
    .projr_readme_add_description(answer_readme, nm_list[["pkg"]])
  readme |> .projr_readme_write()
  .projr_readme_render()
  readme
}

.projr_readme_filter_example <- function(readme) {
  # get where to add to
  readme_ind_example <- which(grepl("^## Example", readme))
  if (.is_len_0(readme_ind_example)) {
    return(readme)
  }
  # exclude the example section
  readme[seq_len(readme_ind_example - 1)]
}

.projr_readme_read <- function() {
  .projr_readme_get_path() |>
    readLines()
}

.projr_readme_get_path <- function() {
  switch(.projr_readme_get_type(),
    "md" = .dir_proj_get("README.md"),
    "Rmd" = .dir_proj_get("README.Rmd")
  )
}

.projr_readme_add_description <- function(readme, answer_readme, pkg) {
  readme_rep <- .projr_init_prompt_readme_description_get(pkg, answer_readme)
  answer_readme_correct <-
    .projr_init_prompt_readme_description_check(readme_rep)

  while (answer_readme_correct == 2) {
    readme_rep <- .projr_init_prompt_readme_description_get(
      pkg, answer_readme
    )
    answer_readme_correct <-
      .projr_init_prompt_readme_description_check(readme_rep)
  }

  if (answer_readme_correct != 3) {
    readme[length(readme)] <- readme_rep
    readme <- c(readme, "")
  }
  readme
}

.projr_readme_get_type <- function() {
  c("md", "Rmd")[.projr_readme_detect_type() + 1]
}

.projr_readme_detect_type <- function() {
  grepl("README\\.Rmd", list.files(.dir_proj_get())) |>
    any()
}
.projr_readme_write <- function(readme) {
  readme |> writeLines(.projr_readme_get_path())
}

.projr_init_prompt_readme_description_get <- function(pkg, answer_readme) {
  cat(
    "Please finish the following sentence:\n",
    paste0("The purpose of ", pkg, " is to...")
  )
  if (.is_test()) {
    return("The purpose of projr is to facilitate reproducible and archived projects") # nolint
  }
  readme_add <- readline(prompt = ">> ")
  paste0("The purpose of ", pkg, " is to ", readme_add)
}

.projr_init_prompt_readme_description_check <- function(readme_rep) {
  if (.is_test()) {
    return(1)
  }
  utils::menu(
    c("Yes", "No", "Complete later"),
    title =
      paste0(
        "Is the following completed sentence/paragraph correct:\n",
        readme_rep
      )
  )
}
.projr_readme_render <- function() {
  if (!file.exists(.dir_proj_get("README.Rmd"))) {
    return(invisible(FALSE))
  }
  try(rmarkdown::render(
    .dir_proj_get("README.Rmd"),
    output_format = "md_document", quiet = TRUE
  ))
  invisible(TRUE)
}


# git and github
.projr_init_prompt_git_gh <- function() {
  # whether to include Git
  answer_git <- .projr_init_prompt_git_gh_answer_git()
  # whether a Git remote is defined already
  gh_exists <- .projr_git_remote_check_exists()
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
# convenience environment variables
# ============================

#' @title Set environment variables for projr_init
#'
#' @description
#' Set environment variables for `projr_init.`
#' When set, `projr_init` will use these variables
#' to provide options to populate the metadata.
#' This function creates the `.Renviron` file
#' for the user if it does not exist.
#' It then adds the variables to the `.Renviron` file
#' without any values set.
#' @export
projr_init_renviron <- function() {
  .projr_init_renviron_create() |>
    .projr_init_renviron_add()
}

.projr_init_renviron_create <- function() {
  .projr_dep_install_only("usethis")
  path <- .projr_usethis_use_scoped_path_r(
    scope = "user", ".Renviron", envvar = "R_ENVIRON_USER"
  )
  if (!file.exists(path)) {
    file.create(path)
  }
  invisible(path)
}

.projr_usethis_use_scoped_path_r <- function(scope = c("user", "projr"),
                                             ...,
                                             envvar = NULL) {
  # rewritten because use_scoped_path_r is not exported from
  # usethis and R CMD CHECK complained about
  # usethis use_scoped_path_r
  scope <- match.arg(scope)
  if (scope == "user" && !is.null(envvar)) {
    env <- Sys.getenv(envvar, unset = "")
    if (!identical(env, "")) {
      return(fs::path_expand(env))
    }
  }
  root <- switch(scope,
    user = fs::path_home_r(),
    project = usethis::proj_get()
  )
  fs::path(root, ...)
}

.projr_init_renviron_add <- function(path) {
  renviron_txt <- readLines(path)
  renviron_txt <- .projr_init_renviron_txt_update(renviron_txt)
  writeLines(renviron_txt, path)
  .projr_newline_append(path)
  message(
    paste0(
      "Edit the .Renviron file at ", path,
      "to have default options for projr_init setup metadata.\n"
    ),
    "The following variables are availabe:\n",
    paste0(
      "  - PROJR_PATH_YML\n",
      "  - PROJR_FIRST_NAME\n",
      "  - PROJR_LAST_NAME\n",
      "  - PROJR_EMAIL\n",
      "  - PROJR_GITHUB_USER_NAME\n"
    ),
    "There must be no white space in the line.\n",
    "For example, `PROJR_FIRST_NAME=Mogley` is valid,\n",
    "but `PROJR_FIRST_NAME = Mogley` is not.\n",
    "Restart R once they are set to use them.\n",
    "They should be available for all new projects for this user."
  )
}

.projr_init_renviron_txt_update <- function(txt) {
  nm_vec <- c(
    "PROJR_PATH_YML", "PROJR_FIRST_NAME", "PROJR_LAST_NAME",
    "PROJR_EMAIL", "PROJR_GITHUB_USER_NAME"
  )
  for (x in nm_vec) {
    txt <- .projr_init_renviron_add_ind(x, txt)
  }
  txt
}

.projr_init_renviron_add_ind <- function(nm, txt) {
  if (!any(grepl(paste0("^", nm), txt))) {
    txt <- c(txt, paste0(nm, "="))
  }
  txt
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
  if (.is_test()) {
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
  path_readme <- .dir_proj_get(fn_readme)
  readme <- readLines(path_readme)
  list(readme = readme, path_readme = path_readme)
}

.projr_init_readme_pre_existing <- function() {
  fn_vec <- list.files(.dir_proj_get())
  if (any(grepl("^README\\.md$", fn_vec))) {
    fn_readme <- "README.md"
    path_readme <- .dir_proj_get(fn_readme)
    readme <- readLines(path_readme)
    return(list(readme = readme, path_readme = path_readme))
  } else if (any(grepl("^README\\.Rmd$", fn_vec))) {
    fn_readme <- "README.Rmd"
    path_readme <- .dir_proj_get(fn_readme)
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
  if (is.null(x)) {
    return(invisible(FALSE))
  }
  .projr_dep_install_only("usethis")
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
    if (answer_git == 2) {
      .projr_yml_git_set(FALSE, "default")
    } else if (answer_git == 3) {
      answer_ignore <- .projr_init_prompt_yn(
        question = "Do you want to apply ignore settings as per `_projr.yml`?",
        answer_auto = 1
      )
      if (answer_ignore == 1) {
        projr_dir_ignore()
      }
    }
    return(invisible(FALSE))
  }
  .projr_git_system_setup()
  .projr_git_init()
  .projr_init_git_commit()
  .projr_init_git_suggest_git()
  invisible(TRUE)
}

.projr_init_git_check_exists <- function() {
  dir.exists(.dir_proj_get(".git"))
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
    list.files("renv", recursive = TRUE, full.names = TRUE),
    "renv.lock",
    ".Rprofile",
    "_projr.yml",
    "_quarto.yml",
    "_bookdown.yml",
    "README.md",
    "README.Rmd"
  )
  fn_vec <- .file_filter_exists(
    vapply(fn_vec, .dir_proj_get, character(1L))
  ) |>
    stats::setNames(NULL) |>
    fs::path_rel(start = .dir_proj_get())
  fn_vec[
    fn_vec %in% c(.projr_git_modified_get(), .projr_git_new_get())
  ]
}

.projr_init_git_suggest_git <- function() {
  if (.projr_git_system_get() == "git") {
    return(invisible(FALSE))
  }
  message(
    "You can use `projr` without the program git\n",
    "(and setup is going fine!),\n",
    "but RStudio (and VS Code) like it.\n",
    "It's easy to install, instructions here:\n",
    "https://happygitwithr.com/install-git"
  )
}

# github
# --------------------------
.projr_init_github <- function(username,
                               public) {
  if (!.projr_git_repo_check_exists() || is.null(username)) {
    .projr_yml_unset_github_dest()
    return(invisible(FALSE))
  }
  if (.projr_git_remote_check_exists()) {
    return(invisible(FALSE))
  }
  .projr_init_github_actual(username, public)
}

.projr_init_github_actual <- function(username, public) {
  .projr_dep_install_only("usethis")
  .projr_dep_install_only("gh")
  if (identical(username, gh::gh_whoami()$login)) {
    tryCatch(
      usethis::use_github(private = !public),
      error = function(e) {
        print("Failed to create GitHub remote")
        print("Can try again later with:")
        print(
          paste0("usethis::use_github(private = ", !public, ")")
        )
      }
    )
  } else {
    tryCatch(
      usethis::use_github(
        username = username,
        private = !public
      ),
      error = function(e) {
        print("Failed to create GitHub remote")
        print("Can try again later with:")
        print(
          paste0(
            "usethis::use_github(username = ", username,
            ", private = ", !public, ")"
          )
        )
      }
    )
  }
  invisible(TRUE)
}

.projr_git_gh_check_auth <- function() {
  if (nzchar(.projr_auth_get_github_pat())) {
    return(invisible(TRUE))
  }
  warning(
    "GITHUB_PAT environment variable not found.\n",
    "\n",
    "To allow creating a GitHub repository, please set it.\n",
    "\n",
    "To easily set it in less than two minutes, do the following:\n",
    "1. If you do not have a GitHub account, create one here: https://github.com\n", # nolint: line_length_linter.
    "2. In R, run usethis::create_github_token()\n",
    "3. In R, run gitcreds::gitcreds_set()\n",
    "4. Paste the token from step 1 into the R command line (terminal), and press enter\n", # nolint: line_length_linter.
    "For more details, see https://happygitwithr.com/https-pat#tldr\n",
    "\n",
    "After doing the above:\n",
    "1. In R, rerun projr::projr_init()\n",
    "It will skip what's been done already and try set up GitHub again.\n",
    call. = FALSE
  )
  invisible(FALSE)
}


# citations
# --------------------------
.projr_init_cite <- function(answer_readme) {
  if (.is_test()) {
    return(invisible(FALSE))
  }
  .projr_init_cite_citation(answer_readme)
  .projr_init_cite_cff()
  .projr_init_cite_codemeta()
}

.projr_init_cite_citation <- function(answer_readme) {
  .projr_init_cite_inst_citation()
  .projr_init_cite_citation_readme(answer_readme)
}

.projr_init_cite_inst_citation <- function() {
  if (file.exists(.dir_proj_get("inst", "CITATION"))) {
    return(invisible(FALSE))
  }
  projr_yml_cite_set(inst_citation = TRUE)
  .projr_dep_install("cffr")
  .projr_cite_citation_set()
}


.projr_init_cite_citation_readme <- function(answer_readme) {
  switch(as.character(answer_readme),
    "1" = .projr_init_cite_citation_readme_add_file(
      .dir_proj_get("README.Rmd")
    ),
    "2" = .projr_init_cite_citation_readme_add_file(
      .dir_proj_get("README.md")
    ),
    "3" = invisible(FALSE)
  )
}

.projr_init_cite_citation_readme_add_rmd <- function() {
  .projr_dep_add("cffr")
  path_readme <- .dir_proj_get("README.Rmd")
  readme_vec <- readLines(path_readme)
  writeLines(
    c(
      readme_vec,
      .projr_init_cite_citation_readme_add_rmd_get_txt()
    ),
    path_readme
  )
}

.projr_init_cite_citation_readme_add_rmd_get_txt <- function() {
  c(
    "",
    "## Citation",
    "```{r, warning = FALSE}",
    paste0("citation(\"", .projr_pkg_nm_get(), "\")"),
    "```"
  )
}

.projr_init_cite_citation_readme_add_file <- function(path_readme) {
  readme_vec <- readLines(path_readme)
  writeLines(
    c(readme_vec, "", "## Citation", "", .projr_cite_bibtex_get()),
    path_readme
  )
  invisible(TRUE)
}

.projr_init_cite_cff <- function() {
  path_cff <- .dir_proj_get("CITATION")
  if (file.exists(path_cff)) {
    return(invisible(FALSE))
  }
  .projr_dep_install("cffr")
  projr_yml_cite_set(cff = TRUE)
  .projr_cite_cff_set()
}

.projr_init_cite_codemeta <- function() {
  path_codemeta <- .dir_proj_get("codemeta.json")
  if (file.exists(path_codemeta)) {
    return(invisible(FALSE))
  }
  projr_yml_cite_set(codemeta = TRUE)
  .projr_dep_install("cboettig/codemeta")
  .projr_cite_codemeta_set()
}
