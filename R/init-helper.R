# _projr.yml
# -----------------------

# set _projr.yml
.init_yml <- function(yml_path_from = NULL) {
  .init_yml_copy(.init_yml_get_path_from(yml_path_from))
  .init_yml_test_unset_push()
  invisible(TRUE)
}

.init_yml_get_path_from <- function(yml_path_from) {
  # get path
  yml_path_from <- yml_path_from %||%
    .init_yml_get_path_from_auto()
  # check path
  .init_yml_check_path_from(yml_path_from)
  yml_path_from
}

.init_yml_get_path_from_auto <- function() {
  if (nzchar(Sys.getenv("PROJR_PATH_YML"))) {
    return(Sys.getenv("PROJR_PATH_YML"))
  }
  system.file("project_structure", "_projr.yml", package = "projr")
}

.init_yml_check_path_from <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("yml_path_from does not exist: ", path))
  }
  if (inherits(try(yaml::read_yaml(path), silent = TRUE), "try-error")) {
    stop(paste0("yml_path_from is not a valid YAML file: ", path))
  }
  invisible(TRUE)
}

.init_yml_copy <- function(path) {
  if (file.exists(.path_get("_projr.yml"))) {
    return(invisible(FALSE))
  }
  fs::file_copy(from = path, to = .path_get("_projr.yml"))
  invisible(TRUE)
}

.init_yml_test_unset_push <- function() {
  # HERE
  if (!.is_test()) {
    return(invisible(FALSE))
  }
  path_yml <- .path_get("_projr.yml")
  yml_projr <- yaml::read_yaml(path_yml)
  if (!"build" %in% names(yml_projr)) {
    return(invisible(FALSE))
  }
  .yml_git_set_push(
    FALSE,
    simplify_default = TRUE, profile = "default"
  )
  invisible(TRUE)
}

# gather metadata and tool selection
# ----------------------------
.init_prompt_init <- function() {
  # package name
  # document engine and related
  nm_list_engine <- .init_prompt_engine()

  # get metadata
  nm_list_metadata <- .init_prompt_metadata()

  # get README requirements
  nm_list_readme <- .init_prompt_readme(nm_list_metadata)

  # get git and gh settings
  nm_list_git_gh <- .init_prompt_git_gh()

  nm_list_engine |>
    append(nm_list_metadata) |>
    append(nm_list_readme) |>
    append(nm_list_git_gh)
}


.init_description <- function(nm_list) {
  if (!.init_description_check()) {
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
  descrptn$write(file = .path_get("DESCRIPTION"))
  desc::desc_normalize(.path_get("DESCRIPTION"))
  .dep_install_only("usethis")
  usethis::proj_set(.path_get())
  usethis::use_roxygen_md()
  invisible(TRUE)
}

.init_description_check <- function() {
  !.is_file_exists_description()
}

.init_dep <- function() {
  if (file.exists(.path_get("_dependencies.R"))) {
    dep <- readLines(.path_get("_dependencies.R"), warn = FALSE)
  } else {
    dep <- NULL
  }
  dep <- c(
    dep,
    "library(projr)", ""
  ) |>
    unique()
  writeLines(dep, .path_get("_dependencies.R"))
  invisible(TRUE)
}

.init_ignore <- function() {
  if (file.exists(.path_get(".gitignore"))) {
    gitignore <- readLines(.path_get(".gitignore"), warn = FALSE)
  } else {
    gitignore <- NULL
  }
  gitignore <- c(
    gitignore,
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "_projr-local.yml", "_environment.local"
  ) |>
    unique()
  writeLines(gitignore, .path_get(".gitignore"))
  .newline_append(.path_get(".gitignore"))

  if (file.exists(.path_get(".Rbuildignore"))) {
    rbuildignore <- readLines(.path_get(".Rbuildignore"), warn = FALSE)
  } else {
    rbuildignore <- NULL
  }
  rbuildignore <- c(
    rbuildignore,
    "^.*\\.Rproj$", "^\\.Rproj\\.user$", "^_projr-local\\.yml$"
  ) |>
    unique()
  writeLines(rbuildignore, .path_get(".Rbuildignore"))
  .newline_append(.path_get(".Rbuildignore"))

  projr_ignore_auto()

  invisible(TRUE)
}

.init_r <- function() {
  if (dir.exists(.path_get("R"))) {
    return(invisible(FALSE))
  }
  dir.create(.path_get("R"))
  invisible(TRUE)
}

.init_renv <- function(force, bioc, skip_init = .is_test()) {
  if (skip_init || .renv_detect()) {
    return(invisible(TRUE))
  }
  .renv_init_rscript_impl(bioc)
  try(source("renv/activate.R"), silent = TRUE)
  # activate `renv`.
  # wrapped in `try` in case something goes wrong.
  # User can just restart `R` to activate `renv`
  # if it goes wrong, so no error-handling attempted.
  invisible(TRUE)
}

.renv_init_rscript_impl <- function(bioc) {
  cmd_txt <- paste0(
    "-e \"renv::init(settings = list(snapshot.type = 'implicit'), bioconductor = ", # nolint
    bioc,
    ")\""
  )

  system2(
    .path_rscript_get(),
    args = cmd_txt, stdout = FALSE
  )
  invisible(TRUE)
}

.renv_detect <- function() {
  .renv_lockfile_path_get() |> file.exists()
}

# engine
# ----------------------
.init_prompt_engine <- function() {
  nm_engine <- .init_prompt_ind_engine()
  nm_format <- .init_prompt_ind_format(nm_engine)
  nm_filename <- .init_prompt_ind_filename(nm_engine)
  list(
    engine = nm_engine,
    format = nm_format,
    filename = nm_filename
  )
}

.init_prompt_ind_engine <- function() {
  if (.init_engine_check_exists()) {
    return(NULL)
  }
  answer_auto <- Sys.getenv(
    "PROJR_TEST_ENGINE",
    unset = "Bookdown project"
  )
  nm_engine <- .init_prompt_ind(
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

.init_prompt_ind_format <- function(nm_engine) {
  if (.init_engine_check_exists()) {
    return(NULL)
  }
  switch(nm_engine,
    "quarto_project" = .init_prompt_ind(
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
    "quarto_document" = .init_prompt_ind(
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
    "rmd" = .init_prompt_ind(
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

.init_prompt_ind_filename <- function(nm_engine) {
  if (.init_engine_check_exists()) {
    return(NULL)
  }
  switch(nm_engine,
    "quarto_document" = .init_prompt_ind(
      .var = NULL,
      nm_item_long = "initial qmd file name",
      option_default = .init_prompt_ind(
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
    "rmd" = .init_prompt_ind(
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
.init_prompt_metadata <- function() {
  nm_pkg <- basename(.path_get())
  if (!.is_test()) {
    .cli_info("Project name is `{nm_pkg}`")
  }
  if (!.is_file_exists_description()) {
    nm_first <- .init_prompt_ind_first()
    nm_last <- .init_prompt_ind_last()
    nm_email <- .init_prompt_ind_email()
    nm_title <- .init_prompt_ind_title()
    nm_license <- .init_prompt_ind_license()
  } else {
    nm_license <- .init_prompt_ind_license()
    nm_license_detail <- !is.null(nm_license) && nm_license == "Proprietary"
    if (nm_license_detail) {
      # if extra details neede
      nm_first <- .init_prompt_ind_first() # nolint
      nm_last <- .init_prompt_ind_last() # nolint
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


.init_prompt_ind_first <- function() {
  .init_prompt_ind(
    .var = "FIRST_NAME",
    nm_item_long = "your first name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "FIRST_NAME"
  )
}

.init_prompt_ind_last <- function() {
  .init_prompt_ind(
    .var = "LAST_NAME",
    nm_item_long = "your last name",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "LAST_NAME"
  )
}

.init_prompt_ind_email <- function() {
  .init_prompt_ind(
    .var = "EMAIL",
    nm_item_long = "your email address",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "USER@DOMAIN.COM"
  )
}

.init_prompt_ind_title <- function() {
  .init_prompt_ind(
    .var = "TITLE",
    nm_item_long = "the project title",
    allow_specify_other = TRUE,
    allow_complete_later = TRUE,
    answer_auto = "TITLE"
  )
}

.init_prompt_ind_license <- function() {
  if (file.exists(.path_get("LICENSE.md"))) {
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

  .init_prompt_ind(
    .var = "LICENSE",
    nm_item_long = "license",
    option_default = option_license_vec,
    allow_specify_other = FALSE,
    allow_complete_later = TRUE,
    answer_translate = long_to_short_license_vec,
    answer_auto = "ccby"
  )
}

# readme
.init_prompt_readme <- function(nm_list_metadata) {
  answer_readme <- .init_prompt_readme_create()
  desc_exists_pre <- .desc_tmp_create()
  .init_readme_create(answer_readme)
  .desc_tmp_remove(desc_exists_pre)
  readme <- .init_prompt_readme_description(
    answer_readme,
    nm_list_metadata
  )
  list(
    answer_readme = answer_readme,
    readme = readme
  )
}

.desc_tmp_create <- function() {
  path_desc <- .path_get("DESCRIPTION")
  desc_exists_pre <- file.exists(path_desc)
  if (!desc_exists_pre) {
    desc::description$new("!new")$write(file = path_desc)
  }
  desc_exists_pre
}

.desc_tmp_remove <- function(desc_exists_pre) {
  if (!desc_exists_pre) {
    .file_rm(.path_get("DESCRIPTION"))
  }
}

.init_prompt_readme_create <- function() {
  if (.is_test()) {
    return(2)
  }
  if (.init_prompt_readme_check_exists()) {
    return(3)
  }
  utils::menu(
    c("Yes (can run R code)", "No (cannot run R code)"),
    title = "Do you want use RMarkdown to create the README?"
  )
}

.init_prompt_readme_check_exists <- function() {
  any(grepl("README\\.md|README\\.Rmd", list.files()))
}

.init_prompt_readme_description <- function(answer_readme,
                                            nm_list) {
  # no changes if README already exists
  if (answer_readme == 3) {
    return(.readme_render())
  }

  # get fn_readme and path_readme
  readme <- .readme_read() |>
    .readme_filter_example() |>
    .readme_add_description(answer_readme, nm_list[["pkg"]])
  readme |> .readme_write()
  .readme_render()
  readme
}

.readme_filter_example <- function(readme) {
  # get where to add to
  readme_ind_example <- which(grepl("^## Example", readme))
  if (.is_len_0(readme_ind_example)) {
    return(readme)
  }
  # exclude the example section
  readme[seq_len(readme_ind_example - 1)]
}

.readme_read <- function() {
  .readme_get_path() |>
    readLines(warn = FALSE)
}

.readme_get_path <- function() {
  switch(.readme_get_type(),
    "md" = .path_get("README.md"),
    "Rmd" = .path_get("README.Rmd")
  )
}

.readme_add_description <- function(readme, answer_readme, pkg) {
  # Replace {{ Package }} placeholders with actual package name
  readme <- gsub("\\{\\{ Package \\}\\}", pkg, readme)

  readme_rep <- .init_prompt_readme_description_get(pkg, answer_readme)
  answer_readme_correct <-
    .init_prompt_readme_description_check(readme_rep)

  while (answer_readme_correct == 2) {
    readme_rep <- .init_prompt_readme_description_get(
      pkg, answer_readme
    )
    answer_readme_correct <-
      .init_prompt_readme_description_check(readme_rep)
  }

  if (answer_readme_correct != 3) {
    readme[length(readme)] <- readme_rep
    readme <- c(readme, "")
  }
  readme
}

.readme_get_type <- function() {
  c("md", "Rmd")[.readme_detect_type() + 1]
}

.readme_detect_type <- function() {
  grepl("README\\.Rmd", list.files(.path_get())) |>
    any()
}
.readme_write <- function(readme) {
  readme |> writeLines(.readme_get_path())
}

.init_prompt_readme_description_get <- function(pkg, answer_readme) {
  cat(
    "Please finish the following sentence:\n",
    paste0("The purpose of ", pkg, " is to...")
  )
  if (.is_test()) {
    return(paste0("The purpose of ", pkg, " is to facilitate reproducible and archived projects")) # nolint
  }
  readme_add <- readline(prompt = ">> ")
  paste0("The purpose of ", pkg, " is to ", readme_add)
}

.init_prompt_readme_description_check <- function(readme_rep) {
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
.readme_render <- function() {
  if (!file.exists(.path_get("README.Rmd"))) {
    return(invisible(FALSE))
  }
  try(rmarkdown::render(
    .path_get("README.Rmd"),
    output_format = "md_document", quiet = TRUE
  ))
  invisible(TRUE)
}


# git and github
.init_prompt_git_gh <- function() {
  # whether to include Git
  answer_git <- .init_prompt_git_gh_answer_git()
  # whether a Git remote is defined already
  gh_exists <- .git_remote_check_exists()
  # get username
  nm_gh <- .init_prompt_git_gh_username(answer_git, gh_exists)
  list(
    answer_git = answer_git, gh = nm_gh
  )
}

.init_prompt_git_gh_answer_git <- function() {
  if (.init_git_check_exists()) {
    return(3)
  }
  .init_prompt_yn(
    question = "Do you want to initialise a Git repo now?",
    answer_auto = 1
  )
}

.init_prompt_git_gh_username <- function(answer_git, gh_exists) {
  ask <- !gh_exists & answer_git %in% c(1, 3)
  if (!ask) {
    return(NULL)
  }
  answer_gh <- .init_prompt_yn(
    question = paste0(
      "Do you want to create a GitHub remote and synchronise?\n",
      "Default settings for usethis::use_github will be used." # nolint
    )
  )
  if (answer_gh == 2) {
    return(NULL)
  }
  .init_prompt_ind(
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
  .init_renviron_create() |>
    .init_renviron_add()
}

.init_renviron_create <- function() {
  .dep_install_only("usethis")
  path <- .usethis_use_scoped_path_r(
    scope = "user", ".Renviron", envvar = "R_ENVIRON_USER"
  )
  if (!file.exists(path)) {
    file.create(path)
  }
  invisible(path)
}

.usethis_use_scoped_path_r <- function(scope = c("user", "projr"),
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
    project = invisible(usethis::proj_get())
  )
  fs::path(root, ...)
}

.init_renviron_add <- function(path) {
  renviron_txt <- readLines(path, warn = FALSE)
  renviron_txt <- .init_renviron_txt_update(renviron_txt)
  writeLines(renviron_txt, path)
  .newline_append(path)
  .cli_info(
    "Edit the .Renviron file at {path} to have default options for projr_init setup metadata.\nThe following variables are availabe:\n",
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

.init_renviron_txt_update <- function(txt) {
  nm_vec <- c(
    "PROJR_PATH_YML", "PROJR_FIRST_NAME", "PROJR_LAST_NAME",
    "PROJR_EMAIL", "PROJR_GITHUB_USER_NAME"
  )
  for (x in nm_vec) {
    txt <- .init_renviron_add_ind(x, txt)
  }
  txt
}

.init_renviron_add_ind <- function(nm, txt) {
  if (!any(grepl(paste0("^", nm), txt))) {
    txt <- c(txt, paste0(nm, "="))
  }
  txt
}

# ============================
# initialisation
# ============================

.init_license <- function(nm_list) {
  .init_license_create(
    x = nm_list[["license"]],
    nm_first = nm_list[["first"]],
    nm_last = nm_list[["last"]]
  )

  invisible(TRUE)
}

.init_readme <- function(nm_list) {
  # automatic for testing
  if (.is_test()) {
    .init_readme_auto()
    return(.init_readme_auto())
  }
  # pre-existing README
  readme_list <- .init_readme_pre_existing()
  if (!is.null(readme_list[["readme"]])) {
    return(readme_list)
  }
  # create afresh
  .init_readme_create()
}

.init_readme_auto <- function() {
  .dep_install_only("usethis")
  usethis::use_readme_md(open = FALSE)
  answer_readme <- 2
  fn_readme <- paste0("README.", ifelse(answer_readme == 1, "Rmd", "md"))
  path_readme <- .path_get(fn_readme)
  readme <- readLines(path_readme, warn = FALSE)
  list(readme = readme, path_readme = path_readme)
}

.init_readme_pre_existing <- function() {
  fn_vec <- list.files(.path_get())
  if (any(grepl("^README\\.md$", fn_vec))) {
    fn_readme <- "README.md"
    path_readme <- .path_get(fn_readme)
    readme <- readLines(path_readme, warn = FALSE)
    return(list(readme = readme, path_readme = path_readme))
  } else if (any(grepl("^README\\.Rmd$", fn_vec))) {
    fn_readme <- "README.Rmd"
    path_readme <- .path_get(fn_readme)
    readme <- readLines(path_readme, warn = FALSE)
    return(list(readme = readme, path_readme = path_readme))
  }
  list(readme = NULL, path_readme = NULL)
}

.init_readme_create <- function(answer_readme) {
  if (answer_readme == 1) {
    .dep_install_only("usethis")
    usethis::use_readme_rmd(open = FALSE)
  } else if (answer_readme == 2) {
    .dep_install_only("usethis")
    usethis::use_readme_md(open = FALSE)
  }
}

.init_license_create <- function(x, nm_first, nm_last) {
  if (is.null(x)) {
    return(invisible(FALSE))
  }
  .dep_install_only("usethis")
  .init_license_create_impl(x, nm_first, nm_last)
  invisible(x)
}

.init_license_create_impl <- function(x, nm_first, nm_last) {
  opt_vec <- c(
    "ccby", "CC-BY", "apache", "Apache 2.0", "cc0", "CC0",
    "proprietary", "Proprietary"
  )
  .assert_in(x, opt_vec, TRUE)
  .dep_install_only("usethis")
  switch(x,
    "ccby" = ,
    "CC-BY" = usethis::use_ccby_license(),
    "apache" = ,
    "Apache 2.0" = usethis::use_apache_license(),
    "cc0" = ,
    "CC0" = usethis::use_cc0_license(),
    "proprietary" = ,
    "Proprietary" = {
      usethis::use_proprietary_license(paste0(nm_first, " ", nm_last))
    }
  )
}

# git
.init_git_init <- function(answer_git,
                           commit = TRUE) {
  if (answer_git %in% c(2, 3)) {
    .init_git_suggest_git()
    if (answer_git == 2) {
      .yml_git_set(FALSE, "default")
    } else if (answer_git == 3) {
      projr_ignore_auto()
    }
    return(invisible(FALSE))
  }
  .init_std_git(TRUE, commit)
  invisible(TRUE)
}

.init_git_check_exists <- function() {
  dir.exists(.path_get(".git"))
}

.init_git_commit <- function() {
  fn_vec <- .init_git_file_get()
  if (.is_len_0(fn_vec)) {
    return(invisible(FALSE))
  }
  .git_commit_file(fn_vec, msg = "Initial projr commit")
}

.init_git_file_get <- function() {
  fn_vec_root <- .file_ls(
    .path_get(),
    recursive = FALSE, full.names = TRUE
  )
  fn_vec_citation <- .init_git_file_get_citation()
  fn_vec_renv <- .init_git_file_get_renv()
  fn_vec <- c(fn_vec_root, fn_vec_citation, fn_vec_renv)
  if (.is_len_0(fn_vec)) {
    return(character(0))
  }
  .git_changed_filter(fn_vec)
}

.init_git_file_get_citation <- function() { # nolint
  if (dir.exists("inst")) {
    list.files("inst", pattern = "^CITATION$", full.names = TRUE)
  } else {
    character(0)
  }
}

.init_git_file_get_renv <- function() {
  if (dir.exists("renv")) {
    fn_vec_root <- .file_ls(
      "renv",
      recursive = FALSE, full.names = TRUE
    )
    dir_vec <- .dir_ls("renv", recursive = FALSE, full.names = FALSE)
    if (.is_len_0(dir_vec)) {
      return(fn_vec_root)
    }
    ignore_vec <- if (file.exists(.path_get("renv", ".gitignore"))) {
      readLines(.path_get("renv", ".gitignore"), warn = FALSE) |> (\(x) gsub("/$", "", x))()
    } else {
      character(0)
    }

    dir_vec <- dir_vec[!basename(dir_vec) %in% ignore_vec]
    if (.is_len_0(dir_vec)) {
      return(fn_vec_root)
    }
    lapply(dir_vec, .file_ls, recursive = TRUE, full.names = TRUE) |>
      unlist() |>
      c(fn_vec_root)
  } else {
    character(0)
  }
}

.init_git_suggest_git <- function() {
  if (.git_system_get() == "git") {
    return(invisible(FALSE))
  }
  .cli_info("You can use `projr` without the program git\n(and setup is going fine!),\nbut RStudio (and VS Code) like it.\nIt's easy to install, instructions here:\nhttps://happygitwithr.com/install-git")
}


# github
# --------------------------
.init_github <- function(username,
                         public) {
  # Check if git repo exists
  if (!.git_repo_check_exists()) {
    .yml_unset_github_dest()
    return(invisible(FALSE))
  }

  # Check if username is NULL or a placeholder value
  if (is.null(username) || identical(username, "GITHUB_USER_NAME")) {
    .yml_unset_github_dest()
    return(invisible(FALSE))
  }

  # Check if remote already exists
  if (.git_remote_check_exists()) {
    return(invisible(FALSE))
  }

  .init_github_impl(username, public)
}

.init_github_impl <- function(username, public) {
  .dep_install_only("usethis")
  .dep_install_only("gh")
  .auth_check_github("creating GitHub repository")
  current_user <- tryCatch(
    {
      gh::gh_whoami()$login
    },
    error = function(e) {
      NULL
    }
  )
  if (!.is_string(current_user)) {
    stop("Failed to get GitHub user information. Please check your GitHub authentication.")
  }
  if (is.null(username) || identical(username, current_user)) {
    .init_github_actual_user(public, current_user)
  } else {
    .init_github_actual_org(public, username)
  }
  invisible(TRUE)
}

.init_github_actual_user <- function(public, username) {
  .cli_info("Creating GitHub remote for user {username}")
  result <- tryCatch(
    usethis::use_github(private = !public),
    error = function(e) {
      .init_github_actual_user_error(public)
      NULL
    }
  )
  if (!is.null(result)) {
    # Do something if the call was successful.
    .cli_info("GitHub remote created successfully!")
  }
  invisible(result)
}

.init_github_actual_user_error <- function(public) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(private = {!public})")
}

.init_github_actual_org <- function(public, username) {
  .cli_info("Creating GitHub remote for organisation {username}")
  if ("username" %in% names(formals(usethis::use_github))) {
    .init_github_actual_org_old(public, username)
  } else {
    .init_github_actual_org_new(public, username)
  }
}

.init_github_actual_org_new <- function(public, username) {
  result <- tryCatch(
    usethis::use_github(
      organisation = username,
      private = !public
    ),
    error = function(e) {
      .init_github_actual_org_new_error(public, username)
      NULL
    }
  )
  if (!is.null(result)) {
    .cli_info("GitHub remote for organisation created successfully!")
  }
  invisible(result)
}

.init_github_actual_org_old <- function(public, username) {
  result <- tryCatch(
    usethis::use_github(
      username = username,
      private = !public
    ),
    error = function(e) {
      .init_github_actual_org_old_error(public, username)
      NULL
    }
  )
  if (!is.null(result)) {
    .cli_info("GitHub remote for user created successfully!")
  }
  invisible(result)
}

.init_github_actual_org_old_error <- function(public, username) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(username = '{username}', private = {!public})")
}
.init_github_actual_org_new_error <- function(public, username) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(organisation = '{username}', private = {!public})")
}

.git_gh_check_auth <- function(use_gh_if_available = TRUE,
                               use_gitcreds_if_needed = TRUE) {
  if (nzchar(.auth_get_github_pat(
    use_gh_if_available = use_gh_if_available,
    use_gitcreds_if_needed = use_gitcreds_if_needed
  ))) {
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
.init_cite <- function(answer_readme) {
  .init_cite_citation(answer_readme)
  .init_cite_cff()
  .init_cite_codemeta()
  invisible(TRUE)
}

.init_cite_citation <- function(answer_readme) {
  .init_cite_inst_citation()
  .init_cite_citation_readme(answer_readme)
}

.init_cite_inst_citation <- function() {
  if (file.exists(.path_get("inst", "CITATION"))) {
    return(invisible(FALSE))
  }
  projr_yml_cite_set(inst_citation = TRUE)
  .dep_install("cffr")
  .cite_citation_set()
}


.init_cite_citation_readme <- function(answer_readme) {
  switch(as.character(answer_readme),
    "1" = .init_cite_citation_readme_add_file(
      .path_get("README.Rmd")
    ),
    "2" = .init_cite_citation_readme_add_file(
      .path_get("README.md")
    ),
    "3" = invisible(FALSE)
  )
}

.init_cite_citation_readme_add_rmd <- function() {
  .dep_add("cffr")
  path_readme <- .path_get("README.Rmd")
  readme_vec <- readLines(path_readme, warn = FALSE)
  writeLines(
    c(
      readme_vec,
      .init_cite_citation_readme_add_rmd_get_txt()
    ),
    path_readme
  )
}

.init_cite_citation_readme_add_rmd_get_txt <- function() {
  c(
    "",
    "## Citation",
    "```{r, warning = FALSE}",
    paste0("citation(\"", .pkg_nm_get(), "\")"),
    "```"
  )
}

.init_cite_citation_readme_add_file <- function(path_readme) {
  readme_vec <- readLines(path_readme, warn = FALSE)
  writeLines(
    c(readme_vec, "", "## Citation", "", .cite_bibtex_get()),
    path_readme
  )
  invisible(TRUE)
}

.init_cite_cff <- function() {
  path_cff <- .path_get("CITATION")
  if (file.exists(path_cff)) {
    return(invisible(FALSE))
  }
  .dep_install("cffr")
  projr_yml_cite_set(cff = TRUE)
  .cite_cff_set()
}

.init_cite_codemeta <- function() {
  path_codemeta <- .path_get("codemeta.json")
  if (file.exists(path_codemeta)) {
    return(invisible(FALSE))
  }
  projr_yml_cite_set(codemeta = TRUE)

  # Try to install and create codemeta.json
  # May fail in CI environments without GitHub authentication
  result <- tryCatch(
    {
      .dep_install("cboettig/codemeta")
      .cite_codemeta_set()
      TRUE
    },
    error = function(e) {
      .cli_info("Note: Could not install 'codemeta' package. Skipping codemeta.json creation.")
      .cli_info("Error: {e$message}")
      FALSE
    }
  )

  invisible(result)
}
