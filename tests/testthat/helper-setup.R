.projr_test_setup_project <- function(git = TRUE,
                                      github = FALSE,
                                      set_env_var = TRUE,
                                      base_name = "test_projr",
                                      env = rlang::caller_env()) {
  # set up directory
  dir_test <- file.path(tempdir(), paste0(base_name))
  if (dir.exists(dir_test)) {
    unlink(dir_test, recursive = TRUE)
  }
  withr::defer(unlink(dir_test, recursive = TRUE), envir = env)
  dir.create(dir_test, recursive = TRUE)

  if (set_env_var) {
    Sys.setenv("PROJR_TEST" = "TRUE")
    withr::defer(Sys.unsetenv("PROJR_TEST"), envir = env)
  }

  # copy files
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  # create .gitignore and .Rbuildignore
  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  # create git repo
  git <- if (github) TRUE else git
  if (git) {
    browser()
    gert::git_init(dir_test)
    gert::git_add(".")
    gert::git_commit(message = "Initial commit")
    gert::git_commit_all(message = "Initial commit")
  }
  # create github repo if required
  if (github) {
    with_dir(dir_test, {
      try(usethis::use_github(private = TRUE))
    })
  }

  dir_test
}

.projr_test_setup_content <- function(label,
                                      output_safe = FALSE,
                                      dir_sub_lvl = 2,
                                      dir_sub_prefix = "subdir") {
  for (x in label) {
    # create files
    file.create(
      projr_path_get(label, "abc.txt", output_safe = output_safe)
    )
    if (dir_sub_lvl > 0) {
      file.create(
        projr_path_get(
          label, paste0(dir_sub_prefix, "1"), "def.txt",
          output_safe = output_safe
        )
      )
    }
    if (dir_sub_lvl > 1) {
      file.create(
        projr_path_get(
          label, paste0(dir_sub_prefix, "1"),
          paste0(dir_sub_prefix, "2"), "ghi.txt",
          output_safe = FALSE
        )
      )
    }
  }
}

.projr_test_manifest_create <- function(pre = TRUE,
                                        post = TRUE,
                                        write = TRUE,
                                        output_run = TRUE) {
  if (pre && !post) {
    manifest <- .projr_build_manifest_hash_pre(output_run)
  } else if (!pre && post) {
    manifest <- .projr_build_manifest_hash_post(output_run)
  } else {
    manifest <- .projr_build_manifest_hash_pre(output_run) |>
      rbind(.projr_build_manifest_hash_post(output_run))
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
  manifest <- .projr_build_manifest_hash_pre(output_run)
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

.projr_test_random_string_get <- function(prefix = "TestProjr") {
  random_chr <- signif(rnorm(1))
  if (is.null(prefix)) random_chr else paste0(prefix, random_chr)
}
