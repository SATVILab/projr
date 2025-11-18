.test_setup_project <- function(git = TRUE,
                                github = FALSE,
                                set_env_var = TRUE,
                                base_name = "test_projr",
                                env = rlang::caller_env(),
                                rm_engine = FALSE) {
  force(env)
  path_dir_test <- .test_setup_project_dir(base_name, env)
  .test_setup_project_env_var(set_env_var, env)
  .test_setup_project_github(github, path_dir_test, env)
  .test_setup_project_files_copy(path_dir_test)
  .test_setup_project_files_create_ignore(path_dir_test)
  .test_setup_project_files_git(git && !github, path_dir_test)
  .test_rm_engine(rm_engine, path_dir_test)
  if (!github) {
    .test_setup_project_github_unset(path_dir_test)
    .test_setup_project_git_unset_push(path_dir_test)
  }
  invisible(path_dir_test)
}

.test_should_run_renv <- function() {
  val <- tolower(Sys.getenv("PROJR_TEST_RENV", "false"))
  val %in% c("true", "1", "yes", "y")
}


.test_rm_engine <- function(rm_engine, path_dir_test) {
  if (rm_engine) {
    .file_rm(file.path(path_dir_test, "_bookdown.yml"))
    .file_rm(file.path(path_dir_test, "index.Rmd"))
    return(invisible(TRUE))
  }
  invisible(FALSE)
}



.test_setup_project_dir <- function(base_name, env) {
  # set up directory
  base_name <- .test_setup_project_basename(base_name)
  path_dir_test <- file.path(tempdir(), paste0(base_name))
  if (dir.exists(path_dir_test)) {
    unlink(path_dir_test, recursive = TRUE)
  }
  withr::defer(unlink(path_dir_test, recursive = TRUE), envir = env)
  .dir_create(path_dir_test)
  path_dir_test
}

.test_setup_project_basename <- function(base_name = NULL) {
  if (is.null(base_name)) {
    base_name <- "Auto"
  }
  paste0("ProjrGitHubTest", base_name, signif(stats::rnorm(1), 6))
}

.test_setup_project_env_var <- function(set_env_var, env) {
  if (set_env_var) {
    .test_set()
    withr::defer(.test_unset(), envir = env)
  }
  invisible(TRUE)
}

.test_setup_project_lit_docs <- function(engine, path_dir_test = NULL) {
  if (engine == "bookdown") {
    return(invisible(FALSE))
  }
  wd <- path_dir_test %||% .path_get()
  fn_vec <- file.path(wd, c("_output.yml", "_bookdown.yml"))
  for (x in fn_vec) {
    if (file.exists(x)) {
      invisible(file.remove(x))
    }
  }
  switch(engine,
    "quarto_project" = .test_setup_project_lit_docs_quarto_project(
      wd
    ),
    "quarto" = .test_setup_project_lit_docs_quarto(wd),
    "rmarkdown" = NULL
  )
}

.test_setup_project_lit_docs_quarto_project <- function(path_dir_test) {
  .test_setup_project_lit_docs_quarto(path_dir_test)
  .test_setup_project_lit_docs_quarto_yml(path_dir_test)
}

.test_setup_project_lit_docs_quarto <- function(path_dir_test) {
  fn <- file.path(path_dir_test, "index.Rmd")
  if (file.exists(fn)) {
    invisible(file.remove(fn))
  }
  writeLines(
    c(
      "---",
      "title: \"My first Quarto doc\"",
      "format: html",
      "---",
      "",
      "# introduction",
      "",
      "This file was generated from R with `writeLines()`."
    ),
    file.path(path_dir_test, "index.qmd")
  )
}

.test_setup_project_lit_docs_quarto_yml <- function(path_dir_test) {
  writeLines(
    c(
      "project:",
      "  type: website",
      "",
      "website:",
      "  title: \"demo site\"",
      "  navbar:",
      "    left:",
      "      - index.qmd"
    ),
    file.path(path_dir_test, "_quarto.yml")
  )
}

.test_setup_project_git_config <- function(global_only = FALSE) {
  env <- parent.frame()
  .test_git_identity_env_set(env)

  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    gert_config_global <- gert::git_config_global()[
      gert::git_config_global()[["level"]] == "global", ,
      drop = FALSE
    ]
    nm <- gert_config_global[["value"]][
      gert_config_global[["name"]] == "user.name"
    ]
    if (!.is_string(nm)) {
      gert::git_config_global_set(
        "user.name", "DarthVader"
      )
      system2(
        "git",
        c("config", "--global", "user.name", "DarthVader"),
        stdout = NULL, stderr = NULL
      )
    }
    email <- gert_config_global[["value"]][
      gert_config_global[["name"]] == "user.email"
    ]
    if (!.is_string(email)) {
      gert::git_config_global_set(
        "user.email", "number_one_fan@tellytubbies.com"
      )
      system2(
        "git",
        c("config", "--global", "user.email", "number_one_fan@tellytubbies.com"),
        stdout = NULL, stderr = NULL
      )
    }
  }

  if (global_only) {
    return(invisible(TRUE))
  }

  .test_git_identity_local_set()
  invisible(TRUE)
}

.test_git_identity_env_set <- function(env) {
  env_vars <- c(
    "GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL",
    "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL"
  )
  old_values <- Sys.getenv(env_vars, unset = NA_character_)
  withr::defer({
    to_restore <- !is.na(old_values)
    if (any(to_restore)) {
      restore_list <- as.list(stats::setNames(old_values[to_restore], env_vars[to_restore]))
      do.call(Sys.setenv, restore_list)
    }
    to_clear <- is.na(old_values)
    if (any(to_clear)) {
      Sys.unsetenv(env_vars[to_clear])
    }
  }, envir = env)

  new_vals <- as.list(stats::setNames(
    c(
      "DarthVader", "number_one_fan@tellytubbies.com",
      "DarthVader", "number_one_fan@tellytubbies.com"
    ),
    env_vars
  ))
  do.call(Sys.setenv, new_vals)
  invisible(TRUE)
}

.test_git_identity_local_set <- function() {
  cfg <- list(
    list(name = "user.name", value = "DarthVader"),
    list(name = "user.email", value = "number_one_fan@tellytubbies.com")
  )

  for (item in cfg) {
    tryCatch(
      gert::git_config_set(item$name, item$value),
      error = function(e) invisible(NULL)
    )
    tryCatch(
      system2(
        "git",
        c("config", "--local", item$name, item$value),
        stdout = NULL, stderr = NULL
      ),
      warning = function(w) invisible(NULL),
      error = function(e) invisible(NULL)
    )
  }
  invisible(TRUE)
}

.test_setup_project_github <- function(github,
                                       path_dir,
                                       env,
                                       debug = FALSE) {
  if (!github) {
    .test_setup_project_github_unset(path_dir)
    return(invisible(FALSE))
  }
  if (!nzchar(.auth_get_github_pat_find())) {
    testthat::skip("Cannot detect GitHub token - skipping GitHub remote tests")
  }
  if (debug) {
    print("Beginning GitHub setup")
    print("Running config function")
  }
  .test_setup_project_git_config(TRUE)
  if (debug) {
    print("Ended running config function")
  }
  # .dir_rm(path_dir)
  # create github repo if required
  with_dir(
    dirname(path_dir),
    .test_github_repo_create(repo = basename(path_dir), env = env)
  )
  invisible(TRUE)
}

.test_setup_project_github_unset <- function(path_dir) {
  path_yml <- file.path(path_dir, "_projr.yml")
  if (!file.exists(path_yml)) {
    return(invisible(FALSE))
  }
  yml_projr <- yaml::read_yaml(path_yml)
  yml_projr[["build"]][["github"]] <- NULL
  yaml::write_yaml(yml_projr, path_yml)
  invisible(TRUE)
}

.test_setup_project_git_unset_push <- function(path_dir_test) {
  path_yml <- file.path(path_dir_test, "_projr.yml")
  if (!file.exists(path_yml)) {
    return(invisible(FALSE))
  }
  yml_projr <- yaml::read_yaml(path_yml)
  yml_git <- yml_projr[["build"]][["git"]]
  if (is.null(yml_git) || isFALSE(yml_git)) {
    return(invisible(FALSE))
  }
  if (isTRUE(yml_git)) {
    yml_git <- list(
      "commit" = TRUE,
      "push" = FALSE
    )
  } else {
    yml_git[["push"]] <- FALSE
  }
  yml_projr[["build"]][["git"]] <- yml_git
  yaml::write_yaml(yml_projr, path_yml)
  invisible(TRUE)
}

.test_setup_project_files_copy <- function(path_dir) {
  for (x in list.files(testthat::test_path("./project_structure"))) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(path_dir, x),
      overwrite = TRUE
    )
  }
  invisible(TRUE)
}

.test_setup_project_files_create_ignore <- function(path_dir) {
  # create .gitignore and .Rbuildignore
  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(path_dir, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(path_dir, ".Rbuildignore"))
  invisible(TRUE)
}

.test_setup_project_files_git <- function(git, path_dir) {
  if (!git) {
    path_yml <- file.path(path_dir, "_projr.yml")
    if (!file.exists(path_yml)) {
      return(invisible(FALSE))
    }
    yml_projr <- yaml::read_yaml(path_yml)
    yml_projr[["build"]][["git"]] <- FALSE
    yaml::write_yaml(yml_projr, path_yml)
    return(invisible(TRUE))
  }
  # Package should be installed via Suggests
  .dep_install_only("gert")
  .test_setup_project_git_config(TRUE)
  gert::git_init(path_dir)
  with_dir(path_dir, .test_setup_project_git_config(FALSE))
  gert::git_add(".", repo = path_dir)
  gert::git_commit(
    message = "Initial commit", repo = path_dir
  )
  invisible(TRUE)
}

.test_setup_content <- function(label,
                                safe = FALSE,
                                dir_sub_lvl = 2,
                                dir_sub_prefix = "subdir") {
  for (x in label) {
    # create directories and files
    base_dir <- projr_path_get_dir(x, safe = safe, create = TRUE)
    file.create(file.path(base_dir, "abc.txt"))
    
    if (dir_sub_lvl > 0) {
      subdir1 <- projr_path_get_dir(
        x, paste0(dir_sub_prefix, "1"),
        safe = safe, create = TRUE
      )
      file.create(file.path(subdir1, "def.txt"))
    }
    if (dir_sub_lvl > 1) {
      subdir2 <- projr_path_get_dir(
        x, paste0(dir_sub_prefix, "1"),
        paste0(dir_sub_prefix, "2"),
        safe = safe, create = TRUE
      )
      file.create(file.path(subdir2, "ghi.txt"))
    }
  }
  vapply(label, projr_path_get_dir, character(1), safe = safe) |> invisible()
}

.test_setup_content_dir <- function(path_dir = NULL,
                                    safe = FALSE,
                                    dir_sub_lvl = 2,
                                    dir_sub_prefix = "subdir") {
  if (is.null(path_dir)) {
    path_dir <- file.path(tempdir(), signif(stats::rnorm(1), 6))
  }
  if (dir.exists(path_dir)) {
    unlink(path_dir, recursive = TRUE)
  }
  dir.create(path_dir, recursive = TRUE)
  # create files
  file.create(file.path(path_dir, "abc.txt"))
  file.create(file.path(path_dir, ".hidden.txt"))
  if (dir_sub_lvl > 0) {
    path_dir_sub1 <- file.path(path_dir, paste0(dir_sub_prefix, "1"))
    dir.create(path_dir_sub1)
    file.create(file.path(path_dir_sub1, "def.txt"))
  }
  if (dir_sub_lvl > 1) {
    path_dir_sub2 <- file.path(path_dir_sub1, paste0(dir_sub_prefix, "2"))
    dir.create(path_dir_sub2)
    file.create(file.path(path_dir_sub2, "ghi.txt"))
  }
  path_dir |> invisible()
}

content_vec_test_file <- c(
  ".hidden.txt", "abc.txt",
  "subdir1/def.txt", "subdir1/subdir2/ghi.txt"
)

content_vec_test_dir <- c(
  "subdir1", "subdir1/subdir2"
)

content_vec <- c(content_vec_test_file, content_vec_test_dir)

.test_manifest_create <- function(pre = TRUE,
                                  post = TRUE,
                                  write = TRUE,
                                  output_run = TRUE) {
  if (pre && !post) {
    manifest <- .build_manifest_pre(output_run)
  } else if (!pre && post) {
    manifest <- .build_manifest_post(output_run)
  } else {
    manifest <- .build_manifest_pre(output_run) |>
      rbind(.build_manifest_post(output_run))
  }
  if (file.exists("manifest.csv")) {
    manifest_old <- utils::read.csv("manifest.csv")
    if (nrow(manifest_old) > 0L) {
      manifest <- manifest_old |>
        rbind(manifest)
    }
  }

  .manifest_write(manifest, output_run = output_run)
  manifest
}

.test_manifest_create_pre <- function(output_run = TRUE) {
  manifest <- .build_manifest_pre(output_run)
  .manifest_write(manifest, output_run = output_run)
  manifest
}

.test_dir_create_random <- function(base = tempdir(), create = TRUE) {
  suffix <- .test_random_string_get()
  path_dir <- file.path(base, "test", suffix)
  if (dir.exists(path_dir)) {
    unlink(path_dir, recursive = TRUE)
  }
  if (create) {
    dir.create(path_dir, recursive = TRUE)
  }
  path_dir
}

.test_random_string_get <- function(prefix = "ProjrOSFTest") {
  random_chr <- signif(stats::rnorm(1))
  if (is.null(prefix)) random_chr else paste0(prefix, random_chr)
}

.test_yml_dest_remote_rm <- function() {
  yml_projr <- projr_yml_get()
  type_vec <- c("local", "github", "osf")
  type_vec <- type_vec[type_vec %in% names(yml_projr[["build"]])]
  for (i in seq_along(type_vec)) {
    yml_projr[["build"]] <- yml_projr[["build"]][
      -which(names(yml_projr[["build"]]) == type_vec[i])
    ]
  }
  .yml_set(yml_projr)
}

.test_coverage <- function() {
  devtools::load_all()
  # Packages should be installed via Suggests
  .dep_install_only("covr")
  .dep_install_only("DT")
  .dep_install_only("htmltools")
  .dir_rm("_tmp/coverage")
  .dir_create("_tmp/coverage")
  .test_unset_select()
  .test_unset_fast()
  withr::with_envvar(
    c(NOT_CRAN = "true"),
    {
      covr::report(
        covr::package_coverage(),
        file = "_tmp/coverage/report.html",
        browse = FALSE
      )
    }
  )
  .file_rm("_tmp/coverage.zip")
  .zip_dir(
    .path_get("_tmp/coverage"),
    .path_get("_tmp/coverage.zip")
  )
}

.test_yml_unset_remote <- function() {
  yml_projr_build <- .yml_build_get(NULL)
  yml_projr_build <- yml_projr_build[setdiff(
    names(yml_projr_build), c("github", "osf", "local")
  )]
  .yml_build_set(yml_projr_build, NULL)
}

# ============================
# test initialization helper functions
# ============================

# Minimal initialization for tests
# Used in tests that need a basic projr setup
.init <- function() {
  # Set up usethis project
  .init_usethis_std()
  
  # Create initial VERSION if needed
  if (!file.exists(.path_get("VERSION")) &&
        !file.exists(.path_get("DESCRIPTION"))) {
    projr_version_set("0.0.0-1")
  }
  
  # Initialize directories
  .init_dir_std(TRUE)
  
  # Copy _projr.yml if not exists
  .init_yml_std(TRUE)
  
  invisible(TRUE)
}

# Full initialization for tests
# Used in tests that need a complete projr setup with all features
.init_full <- function() {
  # Set up usethis project
  .init_usethis_std()
  
  # Create initial VERSION if needed
  if (!file.exists(.path_get("VERSION")) &&
        !file.exists(.path_get("DESCRIPTION"))) {
    projr_version_set("0.0.0-1")
  }
  
  # Initialize directories
  .init_dir_std(TRUE)
  
  # Copy _projr.yml if not exists
  .init_yml_std(TRUE)
  
  invisible(TRUE)
}

.test_setup_project()
