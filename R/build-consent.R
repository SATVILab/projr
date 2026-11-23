# Resolve and authorize the checkout before creating the starter or migrating files.
.build_prepare_project <- function(project) {
  .assert_string(project)
  start <- project %||% getwd()
  .assert_dir_exists_single(start, TRUE, nm = "project")
  start <- as.character(fs::path_real(start))
  root <- tryCatch(
    rprojroot::find_root(.projr_root_criterion, path = start),
    error = function(e) NULL
  )
  needs_config <- is.null(root)
  if (needs_config) {
    markers <- rprojroot::criteria$is_vcs_root |
      rprojroot::has_file("VERSION") |
      rprojroot::has_file("DESCRIPTION") |
      rprojroot::criteria$is_rstudio_project |
      rprojroot::criteria$is_renv_project |
      rprojroot::criteria$is_quarto_project |
      rprojroot::has_file("_bookdown.yml") |
      rprojroot::has_dir(".projr/config") |
      rprojroot::has_dir("_projr/config")
    root <- tryCatch(
      rprojroot::find_root(markers, path = start),
      error = function(e) start
    )
  }
  root <- as.character(fs::path_real(root))
  interactive <- .build_is_interactive()
  explicit <- !is.null(project)
  if (!interactive && explicit && start != root) {
    stop(
      "The project root is ",
      root,
      ". Pass project = ",
      encodeString(root, quote = '"'),
      " to authorize a build there.",
      call. = FALSE
    )
  }
  if (interactive || !explicit) {
    marker <- .build_project_approval_file(root)
    if (is.null(marker) || !utils::file_test("-f", marker)) {
      if (!interactive) {
        stop(
          "This checkout has no saved build approval: ",
          root,
          ". To authorize this run, pass project = ",
          encodeString(root, quote = '"'),
          " to the build function.",
          call. = FALSE
        )
      }
      prompt <- paste0(
        "Allow projr builds in this checkout?\n",
        root,
        "\n",
        "Builds can run project scripts and hooks, write or replace configuration, ",
        "versions, logs, cache files, documents and outputs, and commit or push Git changes. ",
        "Configured directories can be outside this checkout and artifacts can be published to remote destinations."
      )
      if (!isTRUE(utils::askYesNo(prompt, default = FALSE))) {
        stop("Build cancelled.", call. = FALSE)
      }
      .build_project_approve(root)
    }
  }
  if (needs_config) {
    .build_init_config(root)
  }
  .project_state_upgrade(root)
  root
}

# Consent depends on the R session, independently of testing or project settings.
.build_is_interactive <- base::interactive

.build_project_approval_file <- function(root) {
  root <- as.character(fs::path_real(root))
  user_config <- tools::R_user_dir("projr", "config")
  key <- digest::digest(enc2utf8(root), algo = "sha256", serialize = FALSE)
  file <- file.path(user_config, "build-approvals", key)
  # A checkout cannot grant itself permission using files under its own root.
  actual <- .path_resolve_links(fs::path_abs(file))
  if (!fs::path_has_parent(actual, root)) file
}

.build_project_approve <- function(root) {
  marker <- .build_project_approval_file(root)
  if (is.null(marker)) {
    message(
      "Approval applies to this run because the user configuration directory is inside the checkout."
    )
    return(invisible(FALSE))
  }
  fs::dir_create(dirname(marker), mode = "u=rwx")
  fs::file_create(marker, mode = "u=rw")
  invisible(TRUE)
}
