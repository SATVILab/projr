# detailed functions
# ---------------------------

# docs
.projr_yml_dir_set_docs <- function(path, profile) {
  .projr_yml_dir_get_docs_rel_if_within_cache(path, profile) |>
    .projr_yml_dir_set_path("docs", NULL)
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
  yml_dir <- .projr_yml_dir_get_nm(label, profile)
  yml_dir[["hash"]] <- hash
  .projr_yml_dir_set_nm(yml_dir, label, profile)
}


.projr_yml_dir_get_hash <- function(label, profile) {
  .projr_yml_dir_get_nm(label, profile)[["hash"]] %@@% NULL
}

.projr_yml_dir_get_hash_complete <- function(label, profile) {
  .projr_yml_dir_get_hash(label, profile) |>
    .projr_yml_dir_complete_hash(label)
}

.projr_yml_dir_get_hash <- function(label, profile) {
  .projr_yml_dir_get_nm(label, profile)[["hash"]] %@@% NULL
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
.projr_yml_dir_get_pkg_nm_complete <- function(label, profile) {
  if (!label %in% .projr_yml_dir_get_label_output(profile)) {
    return(character())
  }
  .projr_yml_dir_get_pkg_nm(label, profile) |>
    .projr_yml_dir_complete_pkg(label)
}

.projr_yml_dir_get_pkg_nm <- function(label, profile) {
  .projr_yml_dir_get_label(label, profile)[["package"]]
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
    logical = .projr_yml_dir_complete_output_lgl(output, label),
    character = .projr_yml_dir_complete_output_chr(output, label, profile),
    character()
  )
}

.projr_yml_dir_complete_output_lgl <- function(output, label) {
  if (!output) {
    return(character())
  }
  .projr_yml_dir_complete_output_true(label)
}

.projr_yml_dir_complete_output_true <- function(label) {
  .projr_yml_dir_get_label_output() |> setdiff(label)
}

.projr_yml_dir_complete_output_chr <- function(output, label, profile) {
  output[output %in% .projr_yml_dir_get_label_output(profile)] |>
    setdiff(label)
}

# all labels matching certain types
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
  .projr_yml_dir_get_label_nm("docs", profile)
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

.projr_yml_dir_get_nm <- function(nm, profile) {
  .assert_opt(
    nm, names(.projr_yml_dir_get(profile))
  )
  .projr_yml_dir_get(profile)[[nm]]
}

.projr_yml_dir_set_nm <- function(yml, nm, profile) {
  .assert_opt(nm, names(.projr_yml_dir_get(profile)))
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_get_path <- function(label, profile) {
  .projr_yml_dir_get_nm(label, profile)[["path"]]
}

.projr_yml_dir_set_path <- function(path, label, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]][[label]][["path"]] <- path
  .projr_yml_set(yml_projr, profile)
}

# basic functions
# ---------------------------

.projr_yml_dir_get_label <- function(label, profile) {
  .projr_yml_dir_get(profile)[[label]] %@@% NULL
}

.projr_yml_dir_get <- function(profile) {
  projr_yml_get_unchecked(profile)[["directories"]]
}

.projr_yml_dir_set <- function(yml_dir, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]] <- yml_dir
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_nm <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .projr_yml_dir_set_nm_empty(nm, profile)
  } else {
    .projr_yml_dir_set_nm_non_empty(yml, nm, profile)
  }
}

.projr_yml_dir_set_nm_empty <- function(nm, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]] <- yml_projr[["directories"]][
    setdiff(names(yml_projr[["directories"]]), nm)
  ]
  .projr_yml_set(yml_projr, profile)
}

.projr_yml_dir_set_nm_non_empty <- function(yml, nm, profile) {
  yml_projr <- projr_yml_get_unchecked(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .projr_yml_set(yml_projr, profile)
}
