projr_dir_create <- function(yml_active,
                             version_current) {
  dir_proj <- rprojroot::is_r_package$find_file()
  gitignore <- suppressWarnings(readLines(
    file.path(dir_proj, ".gitignore")
  ))
  rbuildignore <- suppressWarnings(readLines(
    file.path(dir_proj, ".Rbuildignore")
  ))
  yml_active_dir <- yml_active[["directories"]]
  for (i in seq_along(yml_active_dir)) {
    yml_curr_orig <- yml_active_dir[i]
    # create one where the output and archive
    # directories are versioned.
    # separate, original one kept for
    # git versioning
    yml_curr_versioned <- yml_curr_orig
    if (names(yml_curr_orig) %in% c("output", "archive")) {
      yml_curr_versioned[[1]][["path"]] <- file.path(
        yml_curr_versioned[[1]][["path"]], version_current
      )
    }
    if (!dir.exists(yml_curr_versioned[[1]][["path"]])) {
      dir.create(yml_curr_versioned[[1]][["path"]], recursive = TRUE)
    }

    within_wd <- fs::path_has_parent(
      yml_curr_orig[[1]][["path"]],
      dir_proj
    )
    if (!within_wd) next

    dir_path <- fs::path_rel(yml_curr_orig[[1]][["path"]], dir_proj)

    txt_gitignore <- paste0(gsub("/*$", "", dir_path), "/**/*")
    txt_rbuildignore <- paste0("^", Hmisc::escapeRegex(dir_path))

    if (!is.logical(yml_curr_orig[[1]][["ignore"]])) next
    if (yml_curr_orig[[1]][["ignore"]]) {
      if (!txt_gitignore %in% gitignore) {
        cat(
          "\n",
          txt_gitignore,
          "\n",
          file = file.path(dir_proj, ".gitignore"),
          sep = "",
          append = TRUE
        )
      }
      if (!txt_rbuildignore %in% rbuildignore) {
        cat(
          "\n",
          txt_rbuildignore,
          "\n",
          file = file.path(dir_proj, ".Rbuildignore"),
          sep = "",
          append = TRUE
        )
      }
    } else {
      if (txt_gitignore %in% gitignore) {
        gitignore <- gitignore[
          -which(gitignore == txt_gitignore),
        ]
        cat(
          gitignore,
          file = file.path(dir_proj, ".gitignore"),
          sep = "",
          append = FALSE
        )
      }
      if (txt_rbuildignore %in% rbuildignore) {
        rbuildignore <- rbuildignore[
          -which(rbuildignore == txt_rbuildignore),
        ]
        cat(
          rbuildignore,
          file = file.path(dir_proj, ".Rbuildignore"),
          sep = "",
          append = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}
