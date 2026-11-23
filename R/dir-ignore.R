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
  path <- directory[["path"]]
  .ignore_project_relative(path, root)
}

.ignore_project_relative <- function(path, root) {
  if (is.null(path)) {
    return(character())
  }

  path <- fs::path_abs(path, start = root)
  if (!fs::path_has_parent(path, root)) {
    if (!.path_can_relativise(path, root)) return(character())

    # The checkout's ancestors may use a symlink or a Windows short name.
    # Check possible checkout roots only, without opening the configured directory.
    parts <- fs::path_split(path)[[1L]]
    matches <- which(tolower(parts) == tolower(basename(root)))
    candidates <- vapply(matches, function(i) as.character(fs::path_join(parts[seq_len(i)])), character(1))
    candidates <- candidates[dir.exists(candidates)]
    aliases <- candidates[normalizePath(candidates, winslash = "/") == root]
    if (!length(aliases)) return(character())
    root <- aliases[[1L]]
  }
  if (path == root) return(character())
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
  # Escape literal text separately from placeholders so directory names match
  # exactly while one rule covers all builds using the same YAML.
  if (type == "git") {
    escape <- function(x) gsub("([][?*\\\\ ])", "\\\\\\1", x)
    pattern <- .template_substitute(path, function(token) {
      hex <- "[0-9a-f]"
      switch(token,
        "@branch" = "[a-z0-9_-]*",
        "@version" = "v[0-9]*",
        "@uuid" = paste(strrep(hex, 8), strrep(hex, 4), paste0("4", strrep(hex, 3)),
                        paste0("[89ab]", strrep(hex, 3)), strrep(hex, 12), sep = "-")
      )
    }, literal = escape)
    return(paste0("/", pattern, "/**"))
  }
  escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
  pattern <- .template_substitute(path, function(token) {
    switch(token,
      "@branch" = "[a-z0-9_-]+",
      "@version" = "v[0-9]+([.-][0-9]+)*",
      "@uuid" = "[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}"
    )
  }, literal = escape)
  paste0("^", pattern, "(/|$)")
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
  if (!reset && is.na(boundary) && length(content) > 0L) {
    # Older releases mixed directory and other rules. Regenerate the block
    # once and keep the caller's manual edits from either side of it.
    projr_ignore_auto()
    return(c(outer$start, .ignore_path_get_list(path)$content, outer$end))
  }
  if (!is.na(boundary)) {
    content <- utils::tail(content, length(content) - boundary)
  }
  c(outer$start, rules, marker, if (!reset) content, outer$end)
}
