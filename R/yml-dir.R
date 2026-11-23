# detailed functions
# ---------------------------

# hash
.yml_dir_nm_set_hash <- function(hash, label, profile) {
  yml_dir <- .yml_dir_get_label(label, profile)
  yml_dir[["hash"]] <- hash
  .yml_dir_set_label(yml_dir, label, profile)
}


.yml_dir_get_hash <- function(label, profile) {
  .yml_dir_get_label(label, profile)[["hash"]] %@@% NULL
}

# license
.yml_dir_set_license <- function(license, label, profile) {
  yml_dir <- .yml_dir_get_label(label, profile)
  yml_dir[["license"]] <- license
  .yml_dir_set_label(yml_dir, label, profile)
}

.yml_dir_get_license <- function(label, profile) {
  .yml_dir_get_label(label, profile)[["license"]] %@@% NULL
}

.yml_dir_get_hash_complete <- function(label, profile) {
  .yml_dir_get_hash(label, profile) |>
    .yml_dir_complete_hash(label)
}

.yml_dir_get_hash <- function(label, profile) {
  .yml_dir_get_label(label, profile)[["hash"]] %@@% NULL
}

.yml_dir_complete_hash <- function(hash, label) {
  switch(.yml_dir_label_class_get(label),
    "raw" = .yml_dir_complete_hash_raw(hash),
    "cache" = .yml_dir_complete_hash_cache(hash),
    "output" = .yml_dir_complete_hash_output(hash),
    "docs" = .yml_dir_complete_hash_output(hash),
    stop("label '", label, "' not a valid label for hashing", call. = FALSE)
  )
}

.yml_dir_label_class_get_match <- function(x, match) {
  .yml_dir_label_class_get(x) %in% match
}

.yml_dir_label_class_get <- function(x) {
  vapply(x, .yml_dir_label_class_get_ind, character(1))
}

.yml_dir_label_class_get_ind <- function(x) {
  if (.yml_dir_label_class_detect_raw(x)) {
    return("raw")
  }
  if (.yml_dir_label_class_detect_cache(x)) {
    return("cache")
  }
  if (.yml_dir_label_class_detect_output(x)) {
    return("output")
  }
  if (.yml_dir_label_class_detect_project(x)) {
    return("project")
  }
  if (.yml_dir_label_class_detect_code(x)) {
    return("code")
  }
  if (.yml_dir_label_class_detect_data(x)) {
    return("data")
  }
  if (.yml_dir_label_class_detect_docs(x)) {
    return("docs")
  }
  stop("label '", x, "' not valid", call. = FALSE)
}

.yml_dir_label_class_detect_raw <- function(x) {
  grepl("^raw", .dir_label_strip(x))
}

.yml_dir_label_class_detect_cache <- function(x) {
  grepl("^cache", .dir_label_strip(x))
}

.yml_dir_label_class_detect_output <- function(x) {
  grepl("^output", .dir_label_strip(x))
}
.yml_dir_label_class_detect_project <- function(x) {
  grepl("^project$", .dir_label_strip(x))
}

.yml_dir_label_class_detect_code <- function(x) {
  grepl("^code$", .dir_label_strip(x))
}

.yml_dir_label_class_detect_data <- function(x) {
  grepl("^data$", .dir_label_strip(x))
}
.yml_dir_label_class_detect_docs <- function(x) {
  grepl("^docs$", .dir_label_strip(x))
}


.yml_dir_label_detect <- function(x, label) {}

.yml_dir_complete_hash_output <- function(hash) {
  hash %||% TRUE
}

.yml_dir_complete_hash_raw <- function(hash) {
  hash %||% TRUE
}

.yml_dir_complete_hash_cache <- function(hash) {
  hash %||% FALSE
}

# get package values for a particular label
.yml_dir_get_pkg_complete <- function(label, profile) {
  if (!label %in% .yml_dir_get_label_output(profile)) {
    return(character())
  }
  .yml_dir_get_pkg(label, profile) |>
    .yml_dir_complete_pkg(label)
}

.yml_dir_get_pkg <- function(label, profile) {
  .yml_dir_get_label(label, profile)[["package"]]
}

.yml_dir_set_pkg <- function(package, label, profile) {
  yml <- .yml_dir_get_label(label, profile)
  yml[["package"]] <- package
  .yml_dir_set_label(yml, label, profile)
}

.yml_dir_complete_pkg <- function(pkg, label) {
  label[pkg]
}

# get output values for a particular label
.yml_dir_get_output_nm_complete <- function(label, profile) {
  .yml_dir_get_output_nm(label, profile) |>
    .yml_dir_complete_output(label, profile)
}

.yml_dir_get_output_nm <- function(label, profile) {
  .yml_dir_get_label(label, profile)[["output"]]
}

.yml_dir_complete_output <- function(output, label, profile) {
  switch(class(output),
    logical = .yml_dir_complete_output_lgl(output, label, profile),
    character = .yml_dir_complete_output_chr(output, label, profile),
    character()
  )
}

.yml_dir_complete_output_lgl <- function(output, label, profile) {
  if (!output) {
    return(character())
  }
  .yml_dir_complete_output_true(label, profile)
}

.yml_dir_complete_output_true <- function(label, profile) {
  .yml_dir_get_label_output(profile) |> setdiff(label)
}

.yml_dir_complete_output_chr <- function(output, label, profile) {
  output[output %in% .yml_dir_get_label_output(profile)] |>
    setdiff(label)
}

# all labels matching certain types
.yml_dir_get_label_artefact <- function(profile) {
  .yml_dir_get_label_in(profile) |>
    c(.yml_dir_get_label_out(profile))
}

.yml_dir_get_label_in <- function(profile) {
  .yml_dir_get_label_raw(profile) |>
    c(.yml_dir_get_label_cache(profile))
}

.yml_dir_get_label_out <- function(profile) {
  c(.yml_dir_get_label_output(profile), .yml_dir_get_label_docs(profile),
    intersect("data", names(.yml_dir_get(profile))))
}

.yml_dir_get_label_output <- function(profile) {
  .yml_dir_get_label_nm("output", profile)
}

.yml_dir_get_label_raw <- function(profile) {
  .yml_dir_get_label_nm("raw", profile)
}

.yml_dir_get_label_cache <- function(profile) {
  .yml_dir_get_label_nm("cache", profile)
}

.yml_dir_get_label_docs <- function(profile) {
  .yml_dir_get_label_nm("docs", profile)
}

.yml_dir_get_label_nm <- function(nm, profile) {
  .yml_dir_get_label_nm_raw(paste0("^", nm), profile)
}

.yml_dir_get_label_nm_raw <- function(nm, profile) {
  yml_projr_dir <- .yml_dir_get(profile)
  label_vec <- names(yml_projr_dir)
  label_vec[
    grepl(nm, .dir_label_strip(label_vec))
  ]
}

.yml_dir_get_label <- function(nm, profile) {
  .assert_in(nm, .opt_dir_get_label(profile))
  .assert_given(profile)
  .yml_dir_get(profile)[[nm]]
}

.yml_dir_set_label <- function(yml, nm, profile) {
  .assert_in(nm, names(.yml_dir_get(profile)))
  yml_projr <- .yml_get(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .yml_set(yml_projr, profile)
}

.yml_dir_get_path <- function(label, profile) {
  directory <- .yml_dir_get_label(label, profile)
  if (is.null(directory)) stop("label '", label, "' not found in _projr.yml", call. = FALSE)
  path <- if (is.list(directory)) directory[["path"]]
  .assert_string(path, TRUE, nm = paste0("directories.", label, ".path"))
  .template_expand(path)
}

.yml_dir_get_path_raw <- function(label, profile) {
  .yml_dir_get_label(label, profile)[["path"]]
}

.yml_dir_set_path <- function(path, label, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["directories"]][[label]][["path"]] <- path
  .yml_set(yml_projr, profile)
}

.yml_dir_add_label <- function(path,
                               package = NULL,
                               output = NULL,
                               label,
                               profile) {
  .assert_string(path, TRUE)
  .assert_string(label, TRUE)
  .assert_given(profile)
  yml_projr <- .yml_get(profile)
  yml_list <- list(path = path)
  if (!is.null(package)) {
    yml_list[["package"]] <- package
  }
  if (!is.null(output)) {
    yml_list[["output"]] <- output
  }
  yml_projr[["directories"]][[label]] <- yml_list
  .yml_set(yml_projr, profile)
}

# basic functions
# ---------------------------
.yml_dir_get_label <- function(label, profile) {
  .yml_dir_get(profile)[[label]] %@@% NULL
}

.yml_dir_get <- function(profile) {
  directories <- .yml_get(profile)[["directories"]] %||% list()
  if (!is.list(directories) || (length(directories) && is.null(names(directories)))) {
    stop("directories must be a YAML mapping of labels to settings.", call. = FALSE)
  }
  directories
}

.yml_dir_get_profiles <- function() {
  files <- setdiff(basename(.yml_config_files()), "_projr.yml")
  profiles <- sub("^_projr-(.*)\\.yml$", "\\1", files)
  profiles <- c("default", setdiff(profiles, c("default", "local")))
  default <- .yml_get_default()[["directories"]]
  local <- .yml_get_local()[["directories"]]
  # A local override with its own path belongs to the merged active view.
  # Keep each named profile's separate physical directory available for ignores.
  local <- Filter(function(directory) is.null(directory[["path"]]), local)

  lapply(profiles, function(profile) {
    directories <- if (profile != "default") {
      .yml_get_profile_ind(profile)[["directories"]]
    }
    .yml_merge(default, directories, local)
  }) |> stats::setNames(profiles)
}

.yml_dir_set <- function(yml_dir, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["directories"]] <- yml_dir
  .yml_set(yml_projr, profile)
}

.yml_dir_set_label <- function(yml, nm, profile) {
  if (is.null(yml) || .is_len_0(yml)) {
    .yml_dir_set_label_empty(nm, profile)
  } else {
    .yml_dir_set_label_non_empty(yml, nm, profile)
  }
}

.yml_dir_set_label_empty <- function(nm, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["directories"]] <- yml_projr[["directories"]][
    setdiff(names(yml_projr[["directories"]]), nm)
  ]
  .yml_set(yml_projr, profile)
}

.yml_dir_set_label_non_empty <- function(yml, nm, profile) {
  yml_projr <- .yml_get(profile)
  yml_projr[["directories"]][[nm]] <- yml
  .yml_set(yml_projr, profile)
}
