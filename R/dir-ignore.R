.git_get_tracked <- function(path) {
  # Retrieve a list of tracked files within the specified path
  fn_vec <- suppressWarnings(system2(
    "git",
    args = c("ls-files", "--", path),
    stdout = TRUE,
    stderr = FALSE
  ))

  if (length(fn_vec) == 0L) {
    return(character(0L))
  }

  # Return the list of tracked file paths
  fn_vec
}

.ignore_diryml <- function(create_git = FALSE) {
  .ignore_diryml_git(create_git)
  .ignore_diryml_rbuild()
  invisible(TRUE)
}

.ignore_diryml_git <- function(create = FALSE) {
  path <- .path_get(".gitignore")
  if (!create && !file.exists(path) && !.git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  .ignore_path_write(.ignore_path_read(path), path, reset = TRUE)
  invisible(TRUE)
}

.ignore_diryml_rbuild <- function() {
  path <- .path_get(".Rbuildignore")
  if (!file.exists(path) && !file.exists(.path_get("DESCRIPTION"))) {
    return(invisible(FALSE))
  }
  .ignore_path_write(.ignore_path_read(path), path, reset = TRUE)
  invisible(TRUE)
}

.ignore_get_setting <- function(label, type, directory = .yml_dir_get_label(label, NULL)) {
  key <- paste0("ignore-", type)
  flag <- function(value) {
    if (is.character(value)) {
      .assert_in(value, c("ignore", "no-ignore", "manual"))
      return(identical(value, "ignore"))
    }
    .assert_flag(value, TRUE)
    value
  }
  general <- directory[["ignore"]]
  specific <- directory[[key]]
  if (!is.null(general) && !is.null(specific) && flag(general) != flag(specific)) {
    stop("Inconsistent ignore settings for label '", label, "': ", key,
      " = ", specific, ", ignore = ", general,
      call. = FALSE
    )
  }
  flag(specific %||% general %||% TRUE)
}

.ignore_diryml_path_get <- function(label, directory = .yml_dir_get_label(label, NULL),
                                    root = .path_get()) {
  path <- directory[["path"]] %||% .yml_dir_get_path_default(label)
  if (label %in% c("project", "code")) {
    return(character())
  }
  if (label == "data") path <- "data"
  if (label == "docs" && is.null(path)) {
    path <- if (file.exists(file.path(root, "_quarto.yml"))) {
      .dir_get_docs_quarto_project_unset()
    } else if (file.exists(file.path(root, "_bookdown.yml"))) {
      .yml_bd_get_output_dir() %||% "_book"
    } else {
      "docs"
    }
  }
  .ignore_project_relative(path, root)
}

.ignore_project_relative <- function(path, root) {
  if (is.null(path)) {
    return(character())
  }

  # These fs operations are lexical, including for foreign drives and UNC paths.
  path <- fs::path_abs(path, start = root)
  if (path == root || !fs::path_has_parent(path, root)) {
    return(character())
  }
  as.character(fs::path_rel(path, start = root))
}

.ignore_diryml_rules <- function(type) {
  root <- .path_get()
  across_profiles <- type == "git" || file.exists(file.path(root, "DESCRIPTION"))
  profiles <- if (across_profiles) .yml_dir_get_profiles() else list()
  active_profiles <- projr_profile_get()
  # Multiple active profiles can supply different fields of the same directory.
  directories <- c(profiles, list(.yml_dir_get(NULL)))
  active <- c(names(profiles) %in% active_profiles, TRUE)

  rules <- character()
  for (i in seq_along(directories)) {
    labels <- names(directories[[i]])
    if (type == "git") labels <- union(labels, "docs")
    for (label in labels) {
      directory <- directories[[i]][[label]]
      path <- .ignore_diryml_path_get(label, directory, root)
      if (!length(path)) next
      keep <- directory[["ignore-even-if-inactive"]] %||% TRUE
      .assert_flag(keep, TRUE)
      if ((!active[i] && !keep) || !.ignore_get_setting(label, type, directory)) next
      rules <- c(rules, .ignore_directory_pattern(path, type))
    }
  }
  sort(unique(rules))
}

.ignore_directory_pattern <- function(path, type) {
  if (grepl("[\r\n]", path)) stop("Invalid newline in directory path.")
  if (type == "git") {
    escaped <- gsub("([][?*\\\\ ])", "\\\\\\1", path)
    return(paste0("/", escaped, "/**"))
  }
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", path)
  paste0("^", escaped, "(/|$)")
}

.ignore_diryml_update <- function(file_vec, path, reset) {
  type <- if (basename(path) == ".gitignore") "git" else "rbuild"
  rules <- .ignore_diryml_rules(type)
  indices <- .ignore_path_get_ind(file_vec, path)
  outer <- .ignore_path_get_startend(indices[["top"]], indices[["bot"]], file_vec)
  content <- .ignore_path_get_content(indices[["top"]], indices[["bot"]], file_vec)

  # The boundary lets manual edits refresh just the directory rules; a full
  # automatic refresh also regenerates every other entry in the managed block.
  marker <- "# End of configured directory rules"
  boundary <- match(marker, content)
  if (!is.na(boundary)) {
    content <- utils::tail(content, length(content) - boundary)
  }
  c(outer$start, rules, marker, if (!reset) content, outer$end)
}
