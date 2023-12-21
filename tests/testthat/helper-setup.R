.projr_test_setup_project <- function(git = TRUE,
                                      github = FALSE,
                                      set_env_var = TRUE,
                                      base_name = "test_projr",
                                      env = rlang::caller_env()) {
  force(env)

  path_dir_test <- .projr_test_setup_project_dir(base_name, env)
  .projr_test_setup_project_env_var(set_env_var, env)
  .projr_test_setup_project_files_copy(path_dir_test)
  .projr_test_setup_project_files_create_ignore(path_dir_test)
  .projr_test_setup_project_files_git(git || github, path_dir_test)
  .projr_test_setup_project_github(github, path_dir_test, env)
  invisible(path_dir_test)
}

.projr_test_setup_project_dir <- function(base_name, env) {
  # set up directory
  path_dir_test <- file.path(tempdir(), paste0(base_name))
  if (dir.exists(path_dir_test)) {
    unlink(path_dir_test, recursive = TRUE)
  }
  withr::defer(unlink(path_dir_test, recursive = TRUE), envir = env)
  .dir_create(path_dir_test)
  path_dir_test
}

.projr_test_setup_project_env_var <- function(set_env_var, env) {
  if (set_env_var) {
    .test_set()
    withr::defer(.test_unset(), envir = env)
  }
  invisible(TRUE)
}

.projr_test_setup_project_files_copy <- function(path_dir) {
  for (x in list.files(testthat::test_path("./project_structure"))) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(path_dir, x),
      overwrite = TRUE
    )
  }
  invisible(TRUE)
}

.projr_test_setup_project_files_create_ignore <- function(path_dir) {
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

.projr_test_setup_project_files_git <- function(git, path_dir) {
  if (!git) {
    return(invisible(TRUE))
  }
  if (!requireNamespace("gert", quietly = TRUE)) {
    utils::install.packages("gert")
  }
  gert::git_init(path_dir)
  gert::git_add(".", repo = path_dir)
  gert::git_config_set(
    "user.name", "Darth Vader",
    repo = path_dir
  )
  gert::git_config_set(
    "user.email", "number_one_fan@tellytubbies.com",
    repo = path_dir
  )
  gert::git_commit(
    message = "Initial commit", repo = path_dir,
  )
  invisible(TRUE)
}


.projr_test_setup_project_github <- function(github, path_dir, env) {
  if (!github) {
    return(invisible(TRUE))
  }
  # create github repo if required
  with_dir(path_dir, .projr_test_setup_project_github_actual(path_dir, env))
  invisible(TRUE)
}

.projr_test_setup_project_github_actual <- function(path_dir, env) {
  remote_vec <- .projr_test_git_remote_get()
  if (.is_len_0(remote_vec)) {
    # creating a repo works fine
    repo <- .projr_test_github_repo_create(
      repo = basename(path_dir), env = env
    ) |>
      basename()
    # adding remotes works
    .projr_test_github_repo_remote_add(repo = repo)
    remote_vec <- .projr_test_git_remote_get()
    if (length(remote_vec) == 0L) {
      stop("No remotes found")
    }
    print(.string_create(remote_vec))
    # we must not then have upstream set:
    invisible(.projr_test_git_set_upstream_and_force_push())
  } else {
    # should really check that the remote exists
  }
  invisible(TRUE)
}

.projr_test_setup_content <- function(label,
                                      safe = FALSE,
                                      dir_sub_lvl = 2,
                                      dir_sub_prefix = "subdir") {
  for (x in label) {
    # create files
    file.create(
      projr_path_get_file(x, "abc.txt", safe = safe)
    )
    if (dir_sub_lvl > 0) {
      file.create(
        projr_path_get_file(
          x, paste0(dir_sub_prefix, "1"), "def.txt",
          safe = safe
        )
      )
    }
    if (dir_sub_lvl > 1) {
      file.create(
        projr_path_get_file(
          x, paste0(dir_sub_prefix, "1"),
          paste0(dir_sub_prefix, "2"), "ghi.txt",
          safe = safe
        )
      )
    }
  }
  vapply(x, projr_path_get_dir, character(1), safe = safe) |> invisible()
}

.projr_test_setup_content_dir <- function(path_dir = NULL,
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

.projr_test_manifest_create <- function(pre = TRUE,
                                        post = TRUE,
                                        write = TRUE,
                                        output_run = TRUE) {
  if (pre && !post) {
    manifest <- .projr_build_manifest_pre(output_run)
  } else if (!pre && post) {
    manifest <- .projr_build_manifest_post(output_run)
  } else {
    manifest <- .projr_build_manifest_pre(output_run) |>
      rbind(.projr_build_manifest_post(output_run))
  }
  if (file.exists("manifest.csv")) {
    manifest_old <- utils::read.csv("manifest.csv")
    if (nrow(manifest_old) > 0L) {
      manifest <- manifest_old |>
        rbind(manifest)
    }
  }

  .projr_manifest_write(manifest, output_run = output_run)
  manifest
}

.projr_test_manifest_create_pre <- function(output_run = TRUE) {
  manifest <- .projr_build_manifest_pre(output_run)
  .projr_manifest_write(manifest, output_run = output_run)
  manifest
}

.projr_test_dir_create_random <- function(base = tempdir(), create = TRUE) {
  suffix <- .projr_test_random_string_get()
  path_dir <- file.path(base, "test", suffix)
  if (dir.exists(path_dir)) {
    unlink(path_dir, recursive = TRUE)
  }
  if (create) {
    dir.create(path_dir, recursive = TRUE)
  }
  path_dir
}

.projr_test_random_string_get <- function(prefix = "ProjrOSFTest") {
  random_chr <- signif(stats::rnorm(1))
  if (is.null(prefix)) random_chr else paste0(prefix, random_chr)
}

.projr_test_yml_dest_remote_rm <- function() {
  yml_projr <- .projr_yml_get()
  type_vec <- c("local", "github", "osf")
  type_vec <- type_vec[type_vec %in% names(yml_projr[["build"]])]
  for (i in seq_along(type_vec)) {
    yml_projr[["build"]] <- yml_projr[["build"]][
      -which(names(yml_projr[["build"]]) == type_vec[i])
    ]
  }
  .projr_yml_set(yml_projr)
}
