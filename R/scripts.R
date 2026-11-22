.scripts_get <- function(scripts, bump_component, profile) {
  scripts_vec_specified <- .scripts_get_specified(
    scripts, bump_component, profile
  )
  .scripts_get_final(scripts_vec_specified)
}

.scripts_get_specified <- function(scripts, bump_component, profile) {
  if (!is.null(scripts)) {
    .assert_chr(scripts)
    return(scripts)
  }
  if (.is_dev_build(bump_component)) {
    .yml_scripts_get_dev(profile)
  } else {
    .yml_scripts_get_build(profile)
  }
}

.scripts_get_final <- function(scripts) {
  if (!is.null(scripts) && length(scripts) == 1L &&
        is.character(scripts) && nchar(trimws(scripts)) == 0L) {
    # return nothing if it's explicitly specified
    return(trimws(scripts))
  }
  # remove whitespace so we can match properly
  scripts <- trimws(scripts)

  scripts_vec_inc <- .scripts_get_inc(scripts)
  scripts_vec_exc <- .scripts_get_exc(scripts)

  # remove excluded scripts from included scripts
  scripts_vec_pre_project <- setdiff(scripts_vec_inc, scripts_vec_exc)

  # if there is a quarto or a bookdown project,
  # then we:
  # a) throw an error if the other literate document
  #    (quarto vs bookdown) is present alongside it
  # b) remove the specified literate document if it
  #    was not explicitly specified
  .scripts_get_considering_project(
    scripts, scripts_vec_pre_project
  )
}

.scripts_get_considering_project <- function(scripts, scripts_pre_project) {
  if (length(scripts) == 0L && length(scripts_pre_project) == 0L) {
    return(character(0))
  }
  is_quarto_project <- file.exists(.path_get("_quarto.yml"))
  is_bookdown_project <- file.exists(.path_get("_bookdown.yml"))
  if (is_quarto_project && is_bookdown_project) {
    stop("Both quarto and bookdown projects are present.")
  }
  if (is_quarto_project) {
    if (!any(grepl("\\.qmd$", scripts))) {
      scripts_pre_project <- scripts_pre_project[
        !grepl("\\.qmd$", scripts_pre_project)
      ]
    }
  } else if (is_bookdown_project) {
    if (!any(grepl("\\.Rmd$|\\.rmd$", scripts))) {
      scripts_pre_project <- scripts_pre_project[
        !grepl("\\.Rmd$|\\.rmd$", scripts_pre_project)
      ]
    }
  }
  scripts_pre_project
}

.scripts_get_exc <- function(scripts) {
  scripts_exc_init <- grep("^!", scripts, value = TRUE)
  if (length(scripts_exc_init) == 0L) {
    return(character(0))
  }
  # remove the leading "!" from excluded scripts
  scripts_exc_init <- sub("^!", "", scripts_exc_init)
  # glob expansion
  scripts_exc <- unlist(lapply(scripts_exc_init, function(pattern) {
    Sys.glob(file.path(.dir_get("project"), pattern))
  }))
  scripts_exc |>
    unique() |>
    sort()
}

.scripts_get_inc <- function(scripts) {
  if (any(!grepl("^!", scripts))) {
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
  list.files(.dir_get("project"), pattern = "\\.qmd$")
}

.scripts_get_inc_all_rmd <- function() {
  list.files(.dir_get("project"), pattern = "\\.Rmd$|\\.rmd$")
}

.scripts_get_inc_all_r <- function() {
  list.files(path = .dir_get("project"), pattern = "\\.R")
}

.scripts_get_inc_specified <- function(scripts) {
  scripts <- scripts[!grepl("^!", scripts)]
  if (length(scripts) == 0L) {
    return(character(0))
  }
  # glob expansion
  scripts_inc <- unlist(lapply(scripts, function(pattern) {
    Sys.glob(file.path(.dir_get("project"), pattern))
  }))
  scripts_inc |>
    unique() |>
    sort()
}
