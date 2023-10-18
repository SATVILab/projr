.projr_test_setup_project <- function(git = TRUE,
                                      set_env_var = TRUE,
                                      base_name = "test_projr") {
  # set up directory
  dir_test <- file.path(tempdir(), paste0(base_name))
  if (dir.exists(dir_test)) {
    unlink(dir_test, recursive = TRUE)
  }
  withr::defer(unlink(dir_test, recursive = TRUE))
  dir.create(dir_test)

  if (set_env_var) {
    Sys.setenv("PROJR_TEST" = "TRUE")
    withr::defer(Sys.unsetenv("PROJR_TEST"))
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
  if (git) {
    gert::git_init(dir_test)
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
  .projr_manifest_write(manifest, output_run = output_run)
  manifest
}

.projr_test_manifest_create_pre <- function(output_run = TRUE) {
  manifest <- .projr_build_manifest_hash_pre(output_run)
  .projr_manifest_write(manifest, output_run = output_run)
  manifest
}
