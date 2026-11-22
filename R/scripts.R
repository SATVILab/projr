.scripts_get <- function(scripts, bump_component, profile) {
  scripts_vec_specified <- .scripts_get_specified(
    scripts, bump_component, profile
  )
  .scripts_get_final(scripts_vec_specified)
}

.scripts_get_specified <- function(scripts, bump_component, profile) {
  if (!is.null(scripts)) {
    .assert_chr_mid(scripts)
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

  scripts_vec_inc <- .scripts_get_inc(scripts)
  scripts_vec_exc <- .scripts_get_exc(scripts)

  setdiff(scripts_vec_inc, scripts_vec_exc)
}

.scripts_get_exc <- function(scripts) {
  patterns <- substring(scripts[startsWith(scripts, "!")], 2L)
  .scripts_expand_patterns(patterns)
}

.scripts_get_inc <- function(scripts) {
  if (all(startsWith(scripts, "!"))) {
    # if there no explicitly-included scripts,
    # then include all available scripts that aren't later excluded
    .scripts_get_inc_all()
  } else {
    .scripts_get_inc_specified(scripts)
  }
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
  list.files(.path_get(), pattern = "\\.qmd$", ignore.case = TRUE, full.names = TRUE)
}

.scripts_get_inc_all_rmd <- function() {
  list.files(.path_get(), pattern = "\\.rmd$", ignore.case = TRUE, full.names = TRUE)
}

.scripts_get_inc_all_r <- function() {
  root <- .path_get()
  scripts <- list.files(root, pattern = "\\.r$", ignore.case = TRUE, full.names = TRUE)
  scripts <- scripts[!startsWith(basename(scripts), "_") & basename(scripts) != "renv_repos.R"]
  hooks <- c(
    unlist(.yml_hooks_get(NULL), use.names = FALSE),
    unlist(.yml_dev_get_hooks(NULL), use.names = FALSE),
    unlist(lapply(.yml_script_get(NULL), `[[`, "path"), use.names = FALSE)
  )
  setdiff(scripts, as.character(fs::path_abs(as.character(hooks), start = root)))
}

.scripts_get_inc_specified <- function(scripts) {
  .scripts_expand_patterns(scripts[!startsWith(scripts, "!")], required = TRUE)
}

.scripts_expand_patterns <- function(patterns, required = FALSE) {
  root <- .path_get()
  matches <- lapply(patterns, function(pattern) {
    path <- as.character(fs::path_abs(path.expand(pattern), start = root))
    matched <- if (file.exists(path)) path else Sys.glob(path)
    matched <- matched[!dir.exists(matched)]
    if (required) {
      matched <- matched[tolower(tools::file_ext(matched)) %in% c("qmd", "rmd", "r")]
      if (length(matched) == 0L) {
        stop("Script '", pattern, "' does not exist or match any supported files.", call. = FALSE)
      }
    }
    sort(matched)
  })
  unique(as.character(unlist(matches, use.names = FALSE)))
}
