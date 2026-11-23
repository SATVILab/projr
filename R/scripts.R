.scripts_get <- function(scripts, bump_component, profile) {
  scripts_vec_specified <- .scripts_get_specified(
    scripts,
    bump_component,
    profile
  )
  .scripts_get_final(scripts_vec_specified)
}

.scripts_get_specified <- function(scripts, bump_component, profile) {
  if (!is.null(scripts)) {
    if (!identical(scripts, character())) {
      .assert_chr_mid(scripts)
    }
    return(scripts)
  }
  if (.is_dev_build(bump_component)) {
    return(.yml_dev_get_scripts(profile) %||% .yml_scripts_get(profile))
  }
  .yml_scripts_get(profile)
}

.scripts_get_final <- function(scripts) {
  if (is.null(scripts)) {
    return(NULL)
  }
  scripts <- trimws(scripts)
  scripts <- scripts[nzchar(scripts)]
  if (length(scripts) == 0L) {
    return(character(0))
  }

  if (all(startsWith(scripts, "!"))) {
    # Only the engine planner knows which project owns the automatic selection.
    return(scripts)
  }
  scripts_vec_inc <- .scripts_get_inc_specified(scripts)
  scripts_vec_exc <- .scripts_get_exc(scripts)

  scripts_vec_inc[!scripts_vec_inc %in% scripts_vec_exc]
}

.scripts_get_exc <- function(scripts) {
  patterns <- substring(scripts[startsWith(scripts, "!")], 2L)
  .scripts_expand_patterns(patterns)
}

.scripts_get_inc_all <- function() {
  c(
    .scripts_get_inc_all_qmd(),
    .scripts_get_inc_all_rmd(),
    .scripts_get_inc_all_r()
  ) |>
    # run in alphabetical order
    sort()
}

.scripts_get_inc_all_qmd <- function() {
  list.files(
    .path_get(),
    pattern = "\\.qmd$",
    ignore.case = TRUE,
    full.names = TRUE
  )
}

.scripts_get_inc_all_rmd <- function() {
  list.files(
    .path_get(),
    pattern = "\\.rmd$",
    ignore.case = TRUE,
    full.names = TRUE
  )
}

.scripts_get_inc_all_r <- function() {
  root <- .path_get()
  scripts <- list.files(
    root,
    pattern = "\\.r$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  scripts <- scripts[
    !startsWith(basename(scripts), "_") & basename(scripts) != "renv_repos.R"
  ]
  hooks <- c(
    unlist(.yml_hooks_get(NULL), use.names = FALSE),
    unlist(.yml_dev_get_hooks(NULL), use.names = FALSE),
    unlist(lapply(.yml_script_get(NULL), `[[`, "path"), use.names = FALSE)
  )
  setdiff(
    scripts,
    as.character(fs::path_abs(as.character(hooks), start = root))
  )
}

.scripts_get_inc_specified <- function(scripts) {
  .scripts_expand_patterns(scripts[!startsWith(scripts, "!")], required = TRUE)
}

.scripts_expand_patterns <- function(patterns, required = FALSE) {
  root <- .path_get()
  paths <- as.character(fs::path_abs(path.expand(patterns), start = root))
  literal <- file.exists(paths)
  matches <- lapply(seq_along(paths), function(i) {
    matched <- if (literal[i]) paths[i] else Sys.glob(paths[i])
    matched <- matched[!dir.exists(matched)]
    if (required) {
      matched <- matched[
        tolower(tools::file_ext(matched)) %in% c("qmd", "rmd", "r")
      ]
      if (length(matched) == 0L) {
        stop(
          "Script '",
          patterns[i],
          "' does not exist or match any supported files.",
          call. = FALSE
        )
      }
    }
    sort(matched)
  })
  files <- as.character(unlist(matches, use.names = FALSE))
  keep <- !duplicated(files)
  explicit <- rep(literal, lengths(matches))
  keep[explicit] <- keep[explicit] | duplicated(files[explicit])
  files[keep]
}
