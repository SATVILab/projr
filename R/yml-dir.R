# detailed functions
# ---------------------------

# docs
.projr_yml_dir_set_docs <- function(path, profile) {
  # this is the profile that we get it from (NULL)
  path_docs <- .projr_yml_dir_get_docs_rel_if_within_cache(path, profile = NULL)
  if (missing(profile) || is.null(profile)) {
    profile_save <- "default"
    # allowing multiple profiles, should
    # make priority match up with quartos.
    # quarto uses the first profile as the dominant one
    projr_profile_vec <- projr_profile_get() |> rev()
    for (i in seq_along(projr_profile_vec)) {
      if (!is.null(.projr_yml_dir_get_path("docs", projr_profile_vec[[i]]))) {
        profile_save <- projr_profile_vec[[i]]
      }
    }
  } else {
    .assert_string(profile, TRUE)
    profile_save <- profile
  }

  # we always save it to the base profile.
  # otherwise we're going to have to figure out
  # where the setting came from.
  # I suppose we can do that...
  # we just read in the active profile,
  # and see if it's
  .projr_yml_dir_set_path(path_docs, "docs", profile_save)
}

.projr_yml_dir_get_docs_rel_if_within_cache <- function(path, profile) {
  within_cache <- fs::path_has_parent(
    path, .projr_dir_get_cache_auto_version(profile = profile)
  )
  if (!within_cache) {
    return(path)
  }
  fs::path_rel(path, .projr_dir_get_cache_auto_version(profile = profile))
}


# hash
.projr_yml_dir_nm_set_hash <- function(hash, label, profile) {
  yml_dir <- .projr_yml_dir_get_label(label, profile)
  yml_dir[["hash"]] <- hash
  .projr_yml_dir_set_label(yml_dir, label, profile)
}


.projr_yml_dir_get_hash <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["hash"]] %@@% NULL
}

.projr_yml_dir_get_hash_complete <- function(label, profile) {
  .projr_yml_dir_get_hash(label, profile) |>
    .projr_yml_dir_complete_hash(label)
}

.projr_yml_dir_get_hash <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["hash"]] %@@% NULL
}

.projr_yml_dir_complete_hash <- function(hash, label) {
  switch(.projr_yml_dir_label_class_get(label),
    "dataraw" = .projr_yml_dir_complete_hash_data_raw(hash),
    "cache" = .projr_yml_dir_complete_hash_cache(hash),
    "output" = .projr_yml_dir_complete_hash_output(hash),
    "docs" = .projr_yml_dir_complete_hash_output(hash),
    stop("label '", label, "' not a valid label for hashing", call. = FALSE)
  )
}

.projr_yml_dir_label_class_get_match <- function(x, match) {
  .projr_yml_dir_label_class_get(x) %in% match
}

.projr_yml_dir_label_class_get <- function(x) {
  vapply(x, .projr_yml_dir_label_class_get_ind, character(1))
}

.projr_yml_dir_label_class_get_ind <- function(x) {
  if (.projr_yml_dir_label_class_detect_data_raw(x)) {
    return("dataraw")
  }
  if (.projr_yml_dir_label_class_detect_cache(x)) {
    return("cache")
  }
  if (.projr_yml_dir_label_class_detect_output(x)) {
    return("output")
  }
  if (.projr_yml_dir_label_class_detect_project(x)) {
    return("project")
  }
  if (.projr_yml_dir_label_class_detect_code(x)) {
    return("code")
  }
  if (.projr_yml_dir_label_class_detect_data(x)) {
    return("data")
  }
  if (.projr_yml_dir_label_class_detect_docs(x)) {
    return("docs")
  }
  stop("label '", x, "' not valid", call. = FALSE)
}

.projr_yml_dir_label_class_detect_data_raw <- function(x) {
  grepl("^dataraw", .projr_dir_label_strip(x))
}

.projr_yml_dir_label_class_detect_cache <- function(x) {
  grepl("^cache", .projr_dir_label_strip(x))
}

.projr_yml_dir_label_class_detect_output <- function(x) {
  grepl("^output", .projr_dir_label_strip(x))
}
.projr_yml_dir_label_class_detect_project <- function(x) {
  grepl("^project$", .projr_dir_label_strip(x))
}

.projr_yml_dir_label_class_detect_code <- function(x) {
  grepl("^code$", .projr_dir_label_strip(x))
}

.projr_yml_dir_label_class_detect_data <- function(x) {
  grepl("^data$", .projr_dir_label_strip(x))
}
.projr_yml_dir_label_class_detect_docs <- function(x) {
  grepl("^docs$", .projr_dir_label_strip(x))
}



.projr_yml_dir_label_detect <- function(x, label) {

}

.projr_yml_dir_complete_hash_output <- function(hash) {
  hash %||% TRUE
}

.projr_yml_dir_complete_hash_data_raw <- function(hash) {
  hash %||% TRUE
}

.projr_yml_dir_complete_hash_cache <- function(hash) {
  hash %||% FALSE
}

# get package values for a particular label
.projr_yml_dir_get_pkg_complete <- function(label, profile) {
  if (!label %in% .projr_yml_dir_get_label_output(profile)) {
    return(character())
  }
  .projr_yml_dir_get_pkg(label, profile) |>
    .projr_yml_dir_complete_pkg(label)
}

.projr_yml_dir_get_pkg <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["package"]]
}

.projr_yml_dir_set_pkg <- function(package, label, profile) {
  yml <- .projr_yml_dir_get_label(label, profile)
  yml[["package"]] <- package
  .projr_yml_dir_set_label(yml, label, profile)
}

.projr_yml_dir_complete_pkg <- function(pkg, label) {
  label[pkg]
}

# get output values for a particular label
.projr_yml_dir_get_output_nm_complete <- function(label, profile) {
  .projr_yml_dir_get_output_nm(label, profile) |>
    .projr_yml_dir_complete_output(label, profile)
}

.projr_yml_dir_get_output_nm <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["output"]]
}

.projr_yml_dir_complete_output <- function(output, label, profile) {
  switch(class(output),
    logical = .projr_yml_dir_complete_output_lgl(output, label, profile),
    character = .projr_yml_dir_complete_output_chr(output, label, profile),
    character()
  )
}

.projr_yml_dir_complete_output_lgl <- function(output, label, profile) {
  if (!output) {
    return(character())
  }
  .projr_yml_dir_complete_output_true(label, profile)
}

.projr_yml_dir_complete_output_true <- function(label, profile) {
  .projr_yml_dir_get_label_output(profile) |> setdiff(label)
}

.projr_yml_dir_complete_output_chr <- function(output, label, profile) {
  output[output %in% .projr_yml_dir_get_label_output(profile)] |>
    setdiff(label)
}

# all labels matching certain types
.projr_yml_dir_get_label_artefact <- function(profile) {
  .projr_yml_dir_get_label_in(profile) |>
    c(.projr_yml_dir_get_label_out(profile))
}

.projr_yml_dir_get_label_in <- function(profile) {
  .projr_yml_dir_get_label_data_raw(profile) |>
    c(.projr_yml_dir_get_label_cache(profile))
}

.projr_yml_dir_get_label_out <- function(profile) {
  .projr_yml_dir_get_label_output(profile) |>
    c(.projr_yml_dir_get_label_docs(profile)) |>
    c("data")
}

.projr_yml_dir_get_label_output <- function(profile) {
  .projr_yml_dir_get_label_nm("output", profile)
}

.projr_yml_dir_get_label_data_raw <- function(profile) {
  .projr_yml_dir_get_label_nm("dataraw", profile)
}

.projr_yml_dir_get_label_cache <- function(profile) {
  .projr_yml_dir_get_label_nm("cache", profile)
}

.projr_yml_dir_get_label_docs <- function(profile) {
  label <- .projr_yml_dir_get_label_nm("docs", profile)
  if (.is_len_0(label)) "docs" else label
}

.projr_yml_dir_get_label_nm <- function(nm, profile) {
  .projr_yml_dir_get_label_nm_raw(paste0("^", nm), profile)
}

.projr_yml_dir_get_label_nm_raw <- function(nm, profile) {
  yml_projr_dir <- .projr_yml_dir_get(profile)
  label_vec <- names(yml_projr_dir)
  label_vec[
    grepl(nm, .projr_dir_label_strip(label_vec))
  ]
}

.projr_yml_dir_get_label <- function(nm, profile) {
  .assert_in(nm, .projr_opt_dir_get_label(profile))
  .assert_given(profile)
  .projr_yml_dir_get(profile)[[nm]]
}

.projr_yml_dir_set_label <- function(yml, nm, profile) {
  .assert_in(nm, names(.projr_yml_dir_get(profile)))
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_get_path <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["path"]]
}

.projr_yml_dir_set_path <- function(path, label, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["directories"]][[label]][["path"]] <- path
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_add_label <- function(path, package = NULL, output = NULL, label, profile) {
  .assert_string(path, TRUE)
  .assert_string(label, TRUE)
  .assert_given(profile)
  yml_projr <- .projr_yml_get(profile)
  yml_list <- list(path = path)
  if (!is.null(package)) {
    yml_list[["package"]] <- package
  }
  if (!is.null(output)) {
    yml_list[["output"]] <- output
  }
  yml_projr[["directories"]][[label]] <- yml_list
  .projr_yml_set(yml_projr, profile)
}

# basic functions
# ---------------------------
.projr_yml_dir_get_label <- function(label, profile) {
  .projr_yml_dir_get(profile)[[label]] %@@% NULL
}

.projr_yml_dir_get <- function(profile) {
  .projr_yml_get(profile)[["directories"]]
}

.projr_yml_dir_set <- function(yml_dir, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["directories"]] <- yml_dir
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_label <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .projr_yml_dir_set_label_empty(nm, profile)
  } else {
    .projr_yml_dir_set_label_non_empty(yml, nm, profile)
  }
}

.projr_yml_dir_set_label_empty <- function(nm, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["directories"]] <- yml_projr[["directories"]][
    setdiff(names(yml_projr[["directories"]]), nm)
  ]
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_label_non_empty <- function(yml, nm, profile) {
  yml_projr <- .projr_yml_get(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}
