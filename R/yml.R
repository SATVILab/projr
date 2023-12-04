#' @title Get active `projr` settings and checks for validity
#'
#' @description
#' Gets active `projr` settings, which merges settings and resolves conflicts
#' between local (`_projr-local.yml`), profile (`_projr-<projr>.yml` or as
#' <key>-<profile> keys in `_projr.yml`)
#' and default (`_projr.yml`) settings.
#' Where there are conflicts, local settings has highest precedence
#' (i.e. are always preferred) and default settings have lowest precedence
#' (i.e. are never preferred).
#'
#' Note that an error is thrown if the active settings
#' are invalid.
#'
#' @seealso projr_yml_get_unchecked,projr_yml_check
#'
#' @return A named list, if the settings are valid.
#'
#' @export
projr_yml_get <- function() {
  yml_projr <- projr_yml_get_unchecked()
  projr_yml_check(yml_projr)
  yml_projr
}

#' @title Get active `projr` settings and do no check
#'
#' @description A list of the active `projr` settings,
#' without doing any error checking.
#' Gets active `projr` settings, which merges settings and resolves conflicts
#' between local (`_projr-local.yml`), profile (`_projr-<projr>.yml` or as
#' <key>-<profile> keys in `_projr.yml`)
#' and default (`_projr.yml`) settings.
#' Where there are conflicts, local settings has highest precedence
#' (i.e. are always preferred) and default settings have lowest precedence
#' (i.e. are never preferred).
#'
#' @return A named list.
#'
#' @seealso projr_yml_get,projr_yml_check
#' @export
projr_yml_get_unchecked <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_profile <- .projr_yml_get_profile()
  yml_projr_local <- .projr_yml_get_local()

  .projr_yml_merge(
    yml_projr_root, yml_projr_profile, yml_projr_local
  )
}

.projr_yml_merge <- function(yml_projr_root_default,
                             yml_projr_profile,
                             yml_projr_local) {
  nm_vec <- names(yml_projr_root_default) |>
    c(names(yml_projr_profile), names(yml_projr_local)) |>
    unique()
  lapply(nm_vec, function(nm) {
    elem_default <- yml_projr_root_default[[nm]]
    elem_profile <- yml_projr_profile[[nm]]
    elem_local <- yml_projr_local[[nm]]
    # return early if highest-precedence element
    # is not a list.
    # Otherwise, make any lower-precedence elements
    # NULL.
    # if local is a list, then make others
    # empty lists if not lists
    if (is.list(elem_local)) {
      if (!is.list(elem_profile)) {
        elem_profile <- list()
      }
      if (!is.list(elem_default)) {
        elem_default <- list()
      }
    } else {
      # if elem_local is not a list and is not NULL,
      # then simply return elem_local
      if (!is.null(elem_local)) {
        return(elem_local)
      } else {
        # if elem_local is not a list and is NULL,
        # then consider what elem_profile is
        if (is.list(elem_profile)) {
          if (!is.list(elem_default)) {
            elem_default <- list()
          }
        } else {
          # if elem_profile is not a list and is not NULL,
          # then simply return elem_profile
          if (!is.null(elem_profile)) {
            return(elem_profile)
          } else {
            # if elem_profile is not a list and is NULL,
            # then it must be that elem_default is not NULL
            # (otherwise we would not have a name entry here)
            if (!is.list(elem_default)) {
              return(elem_default)
            }
          }
        }
      }
    }
    # carry on if no return has been made
    # because the highest-precedence element was not a list
    .projr_yml_merge(elem_default, elem_profile, elem_local)
  }) |>
    stats::setNames(nm_vec)
}

.projr_yml_get_root_full <- function() {
  path_yml <- .projr_dir_proj_get("_projr.yml")
  if (!file.exists(path_yml)) {
    stop(paste0("_projr.yml not found at ", path_yml))
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_get_root_default <- function() {
  yml_projr_root_full <- .projr_yml_get_root_full()
  nm_vec <- c("directories", "build")
  if ("version-format" %in% names(yml_projr_root_full)) {
    nm_vec <- c(nm_vec, "version-format")
  }
  yml_projr_root_full[nm_vec]
}

.projr_yml_get_profile <- function() {
  profile <- projr_profile_get()
  if (profile == "default") {
    return(list())
  }
  yml_projr_root_full <- .projr_yml_get_root_full()
  key_root_dir <- paste0("directories-", profile)
  key_root_build <- paste0("build-", profile)
  root_dir_ind <- key_root_dir %in% names(yml_projr_root_full)
  root_build_ind <- key_root_build %in% names(yml_projr_root_full)
  path_yml_projr_profile <- .projr_dir_proj_get(
    paste0("_projr-", profile, ".yml")
  )
  path_projr_profile_root <- file.exists(path_yml_projr_profile)
  if ((root_build_ind || root_dir_ind) && path_projr_profile_root) {
    stop(paste0(
      "Settings for profile ", profile,
      " found in both _projr.yml and _projr-", profile,
      ".yml. Please either delete _projr-", profile,
      ".yml, or remove the profile's settings in _projr.yml"
    ))
  }
  if (root_build_ind || root_dir_ind) {
    pos_dir <- which(names(yml_projr_root_full) == key_root_dir)
    pos_build <- which(names(yml_projr_root_full) == key_root_build)
    pos_either <- c(pos_dir, pos_build)
    yml_projr_profile <- yml_projr_root_full[pos_either]
    return(yml_projr_profile)
  }
  if (!file.exists(path_yml_projr_profile)) {
    return(list())
  }
  yml_projr_init <- yaml::read_yaml(path_yml_projr_profile)
  pos_dir <- which(names(yml_projr_init) == key_root_dir)
  pos_build <- which(names(yml_projr_init) == key_root_build)
  pos_either <- c(pos_dir, pos_build)
  yml_projr_init[pos_either]
}

.projr_yml_get_local <- function() {
  path_yml <- .projr_dir_proj_get("_projr-local.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }

  yml_projr_init <- yaml::read_yaml(path_yml)
  pos_dir <- which(names(yml_projr_init) == "directories")
  pos_build <- which(names(yml_projr_init) == "build")
  pos_either <- c(pos_dir, pos_build)
  yml_projr_init[pos_either]
}

#' @title Check active `projr` settings.
#'
#' @description
#' Checks correctness of active `projr` settings.
#' @param yml_projr list.
#' Projr settings. If not supplied,
#' then the results of `projr_yml_get_unchecked` is used.
#'
#' @return
#' Returns `TRUE` if all checks pass.
#' Otherwise throws an error.
#'
#' @export
projr_yml_check <- function(yml_projr = NULL) {
  if (is.null(yml_projr)) {
    yml_projr <- projr_yml_get_unchecked()
  }
  .projr_yml_check_merged(yml_projr)
}

.projr_yml_check_merged <- function(yml_projr) {
  if (!"directories" %in% names(yml_projr)) {
    stop("default directories not set in _projr.yml.")
  }
  if (!"build" %in% names(yml_projr)) {
    stop('_projr.yml must include "build" element')
  }

  # directories section
  # ----------------------
  yml_projr_dir <- yml_projr[["directories"]]
  nm_vec_dir <- names(yml_projr_dir)[nzchar(names(yml_projr_dir))]
  if (!length(nm_vec_dir) == length(yml_projr_dir)) {
    stop("Directories must be named in projr settings")
  }
  nm_vec_dir_uni <- unique(nm_vec_dir)
  if (!length(nm_vec_dir_uni) == length(nm_vec_dir)) {
    stop("Directory names must be unique in projr settings")
  }
  if (!length(names(yml_projr_dir)) == length(yml_projr_dir)) {
    stop("no directories set in _projr.yml")
  }
  nm_vec_dir <- names(yml_projr_dir)
  # required repositories
  nm_vec_dir_match <- .projr_dir_label_strip(nm_vec_dir)
  if (!any(grepl("^dataraw", nm_vec_dir_match))) {
    stop("No data-raw directory specified in projr settings")
  }
  if (!"cache" %in% nm_vec_dir_match) {
    stop("No cache directory specified in projr settings")
  }
  if (!any(grepl("^output", nm_vec_dir_match))) {
    stop("No output directory specified in projr settings")
  }
  if (!any(grepl("^archive", nm_vec_dir_match))) {
    stop("No archive directory specified in projr settings")
  }

  .projr_yml_check_dir(yml_projr_dir)


  # build section
  # -------------------------
  yml_projr_build <- yml_projr[["build"]]

  # dev-output
  if (!"dev-output" %in% names(yml_projr_build)) {
    stop("dev-output section missing from projr settings in build key")
  }
  if (!is.logical(yml_projr_build[["dev-output"]])) {
    stop("dev-output must be logical in projr settings in build key")
  }
  nm_vec_extra <- setdiff(
    names(yml_projr_build),
    c("dev-output", "git", "github", "package", "local", "osf")
  )
  if (length(nm_vec_extra) > 0) {
    stop(paste0(
      "The following key(s) are unknown
      in the build section of projr settings: ",
      paste0(nm_vec_extra, collapse = ", ")
    ))
  }
  .projr_yml_check_build_git(yml_projr_build[["git"]])
  .projr_yml_check_build_gh_release(yml_projr_build[["github"]])

  TRUE
}

.projr_yml_check_dir <- function(yml_projr_dir) {
  for (i in seq_along(yml_projr_dir)) {
    .projr_yml_check_dir_elem(
      yml_projr_dir[[i]],
      names(yml_projr_dir)[i],
      names(yml_projr_dir)
    )
  }
  invisible(TRUE)
}

.projr_yml_check_dir_elem <- function(elem, key, keys) {
  key_match <- .projr_dir_label_strip(key)
  nm_vec_actual <- names(elem)
  nm_vec_valid <- c(
    "path", "ignore-git", "ignore-rbuild", "git-track-adjust",
    "output", "archive",
    "manifest",
    "hash",
    "osf"
  )
  nm_vec_extra <- setdiff(nm_vec_actual, nm_vec_valid)
  if (length(nm_vec_extra) > 0) {
    stop(paste0(
      "The following name(s) are invalid for
      directories as projr settings:",
      paste0(nm_vec_extra, collapse = ", ")
    ))
  }
  if (length(nm_vec_actual) > length(unique(nm_vec_actual))) {
    stop(paste0(
      "Directory settings must be unique"
    ))
  }
  # path
  # -------------------

  if (!grepl("^docs", key_match)) {
    if (!"path" %in% nm_vec_actual) {
      stop(paste0(
        "Path must be specified for directories in `projr` settings"
      ))
    }
  }
  if ("path" %in% nm_vec_actual) {
    if (!all(is.character(elem[["path"]]))) {
      stop(paste0(
        "Path must be of type character for directories in `projr` settings"
      ))
    }
    if (!length(elem[["path"]]) == 1) {
      stop(paste0(
        "Path must be of length one for directories in `projr` settings"
      ))
    }
  }

  # ignore
  # ------------
  if ("ignore-git" %in% names(elem)) {
    # must be length 1
    if (!length(elem[["ignore-git"]]) == 1) {
      stop(paste0(
        "`ignore_git` must be of length 1 for for directories in `projr` settings" # nolint
      ))
    }
    # must be logical, or else character of certain types
    ignore_logical <- is.logical(elem[["ignore-git"]])
    if (is.character(elem[["ignore-git"]])) {
      ignore_chr_correct <- elem[["ignore-git"]] %in%
        c("manual", "ignore", "no-ignore")
    } else {
      ignore_chr_correct <- FALSE
    }
    if (!(ignore_logical || ignore_chr_correct)) {
      stop(paste0(
        '`ignore_git` must be of type logical or `"manual"`, `"ignore"` or `"no-ignore"` # nolint
        for directories in `projr` settings'
      ))
    }
  }
  if ("ignore-rbuild" %in% names(elem)) {
    # must be length 1
    if (!length(elem[["ignore-rbuild"]]) == 1) {
      stop(paste0(
        "`ignore_rbuild` must be of length 1 for for directories in `projr` settings" # nolint
      ))
    }
    # must be logical, or else character of certain types
    ignore_logical <- is.logical(elem[["ignore-rbuild"]])
    if (is.character(elem[["ignore-rbuild"]])) {
      ignore_chr_correct <- elem[["ignore-rbuild"]] %in%
        c("manual", "ignore", "no-ignore")
    } else {
      ignore_chr_correct <- FALSE
    }
    if (!(ignore_logical || ignore_chr_correct)) {
      stop(paste0(
        '`ignore_rbuild` must be of type logical or `"manual"`, `"ignore"` or `"no-ignore"` # nolint
        for directories in `projr` settings'
      ))
    }
  }

  # must be logical or character
  if ("git-track-adjust" %in% names(elem)) {
    if (!is.logical(elem[["output"]])) {
      stop(paste0(
        "`git_track_adjust` must be of type character
      for directories in `projr` settings"
      ))
    }
  }

  # output
  # ------------
  # must be logical or character
  if ("output" %in% names(elem)) {
    if (!is.logical(elem[["output"]])) {
      if (!all(is.character(elem[["output"]]))) {
        stop(paste0(
          "`output` must be of type character
          for directories in `projr` settings"
        ))
      }
    }
  }

  # archive
  # ------------
  # must be logical or character
  if ("archive" %in% names(elem)) {
    if (!is.logical(elem[["archive"]])) {
      if (!is.character(elem[["archive"]])) {
        stop(paste0(
          "`archive` must be of type character
          for directories in `projr` settings"
        ))
      }
    }
  }

  # hash
  # ------------
  # must be logical or character
  if ("hash" %in% names(elem)) {
    if (!is.logical(elem[["hash"]])) {
      stop(paste0(
        "`hash` must be of type logical
        for directories in `projr` settings"
      ))
    }
  }

  # manifest
  # ------------
  # must be logical or character
  if ("manifest" %in% names(elem)) {
    if (!is.logical(elem[["manifest"]])) {
      stop(paste0(
        "`manifest` must be of type logical
        for directories in `projr` settings"
      ))
    }
  }

  # key-specific
  # ==================



  # path not invalid
  # -------------------
  dir_vec_restricted <- c(
    "data", "man", "R", "tests"
  )
  if (grepl("^dataraw|^cache|^archive|^output", key_match)) {
    within_ind <- fs::path_has_parent(
      elem[["path"]], dir_vec_restricted
    ) |>
      any()
    if (within_ind) {
      stop(paste0(
        "Paths in dataraw, cache, archive and output directories cannot
        be to the following (or sub-directories thereof): ",
        paste0(dir_vec_restricted, collapse = ", ")
      ))
    }
  }

  # sending to output
  # -------------------
  if (grepl("^dataraw|^cache", key_match)) {
    if ("output" %in% names(elem)) {
      if (is.character(elem[["output"]])) {
        if (any(!elem[["output"]] %in% keys)) {
          stop(paste0(
            "Output location for projr directory ", key, " misspecified"
          ))
        }
      }
    }
  } else if (grepl("^output|^archive", key_match)) {
    if ("output" %in% names(elem)) {
      stop(paste0(
        "Output location for projr directory ", key,
        " should not be specified"
      ))
    }
  }

  # sending to archive
  # -----------------
  if (grepl("^output|^dataraw|^cache", key_match)) {
    if ("archive" %in% names(elem)) {
      if (is.character(elem[["archive"]])) {
        if (any(!elem[["archive"]] %in% keys)) {
          stop(paste0(
            "Archive location for projr directory ", key, " misspecified"
          ))
        }
      }
    }
  } else if (grepl("^archive", key_match)) {
    if ("archive" %in% names(elem)) {
      stop(paste0(
        "Archive location for projr directory ", key,
        " should not be specified"
      ))
    }
  }

  # sending manifest to cache or data-raw
  # -----------------
  if (grepl("^dataraw|^cache", key_match)) {
    if ("manifest" %in% names(elem)) {
      stop(paste0(
        "Cannot save manifest to the following
        `projr` directory (data-raw or cache): ",
        key
      ))
    }
  }

  # hashing archive
  # -----------------
  if (grepl("^archive", key_match)) {
    if ("hash" %in% names(elem)) {
      stop(paste0(
        "Cannot hash `projr` archive directories: ", key
      ))
    }
  }


  invisible(TRUE)
}

.projr_yml_check_build_git <- function(yml_git) {
  if (is.null(yml_git)) {
    return(FALSE)
  }
  nm_vec_permitted <- c("commit", "add-untracked", "push")
  nm_vec_actual <- names(yml_git)
  nm_vec_extra <- setdiff(nm_vec_actual, nm_vec_permitted)
  if (length(nm_vec_extra) > 0) {
    stop(paste0(
      "The following are extra setting(s)
      in the Git section of the projr build key: ",
      paste0(nm_vec_extra, collapse = ", ")
    ))
  }
  if (!"commit" %in% names(yml_git)) {
    stop("The commit key is required in the
    Git section of the projr build key")
  }
  for (i in seq_along(yml_git)) {
    if (!is.logical(yml_git[[i]])) {
      stop(paste0(
        "The key ", names(yml_git)[i],
        " in the git section of the projr build key must be logical"
      ))
    }
  }
}

.projr_yml_check_build_gh_release <- function(yml_gh) {
  if (is.null(yml_gh)) {
    return(FALSE)
  }
  dir_vec <- names(projr_yml_get_unchecked()[["directories"]])
  for (i in seq_along(yml_gh)) {
    .projr_yml_check_gh_release_ind(
      tag = names(yml_gh)[i],
      elem = yml_gh[[i]],
      directories = dir_vec
    )
  }
  invisible(TRUE)
}

.projr_yml_check_gh_release_ind <- function(tag = NULL,
                                            elem = NULL,
                                            directories = NULL) {
  if (is.null(tag)) {
    stop("tag must be specified for GitHub releases in projr settings")
  }
  if (is.null(elem)) {
    stop("`content` and `body` must be specified
    for GitHub releases in projr settings")
  }
  nm_vec_permitted <- c("content", "body") |> sort()
  nm_vec_actual <- names(elem) |> sort()
  if (!all(nm_vec_permitted == nm_vec_actual)) {
    stop(
      "GitHub releases specified in projr settings
      require `content` and `body` keys"
    )
  }
  directories <- c(directories, "code")
  directories_extra <- setdiff(elem[["content"]], directories)
  if (length(directories_extra) > 0) {
    stop(paste0(
      "In GitHub release with tag ", tag,
      ", the following entries in content are not found
      in the directories key:",
      directories_extra
    ))
  }
  invisible(TRUE)
}

.projr_yml_set <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_set_root <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_bd_get <- function() {
  path_yml <- .projr_dir_proj_get("_bookdown.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_quarto_get <- function() {
  path_yml <- .projr_dir_proj_get("_quarto.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_bd_set <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_bookdown.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_quarto_set <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_quarto.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_desc_get <- function() {
  path_desc <- .projr_dir_proj_get("DESCRIPTION")
  read.dcf(path_desc)
}

# cite
# -----------------

.projr_yml_set_cite <- function(all_three = FALSE,
                                codemeta = FALSE,
                                cff = FALSE,
                                inst_citation = FALSE) {
  # Note: codemeta, cff and inst_citation
  # being FALSE does not set the corresponding yml
  # values to FALSE.
  # The function never sets such values to FALSE,
  # it's only for making such values TRUE.
  # So, FALSE just means "don't force to TRUE".

  # set all three
  set_all_three <- .projr_yml_set_cite_all_three(
    force_all_three = all_three,
    code_meta = codemeta,
    cff = cff,
    inst_citation = inst_citation
  )
  if (set_all_three) {
    return(invisible(TRUE))
  }
  .projr_yml_cite_add_ind(
    codemeta = codemeta,
    cff = cff,
    inst_citation = inst_citation
  )
}

.projr_yml_unset_cite <- function() {
  .projr_yml_set_cite(all_three = FALSE)
}

.projr_yml_cite_check_all_three <- function(force_all_three = FALSE,
                                            code_meta = FALSE,
                                            cff = FALSE,
                                            inst_citation = FALSE) {
  if (force_all_three) {
    return(TRUE)
  }
  yml_projr_cite <- .projr_yml_get_root_default()[["build"]][["cite"]]
  missing_vec <- setdiff(
    c("codemeta", "cff", "inst-citation"),
    c(
      yml_projr_cite,
      "code_meta"[code_meta],
      "cff"[cff],
      "inst_citation"[inst_citation]
    )
  )
  identical(missing_vec, character())
}

.projr_yml_set_cite_all_three <- function(force_all_three,
                                          code_meta,
                                          cff,
                                          inst_citation) {
  all_three_lgl <- .projr_yml_cite_check_all_three(
    force_all_three = force_all_three,
    code_meta = code_meta,
    cff = cff,
    inst_citation = inst_citation
  )
  if (!all_three_lgl) {
    return(TRUE)
  }
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["cite"]] <- TRUE
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}

.projr_yml_cite_add_ind <- function(codemeta = FALSE,
                                    cff = FALSE,
                                    inst_citation = FALSE) {
  # setup
  yml_projr_root <- .projr_yml_get_root_default()

  yml_cite <- .projr_yml_cite_add_ind_yml(
    yml_cite = yml_projr_root[["build"]][["cite"]],
    codemeta = codemeta,
    cff = cff,
    inst_citation = inst_citation
  )

  if (isFALSE(yml_cite)) {
    yml_projr_root[["build"]] <- yml_projr_root[["build"]][
      -which(names(yml_projr_root[["build"]]) == "cite")
    ]
  } else {
    yml_projr_root[["build"]] <- yml_cite
  }

  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}

.projr_yml_cite_add_ind_yml <- function(yml_cite = character(),
                                        codemeta = FALSE,
                                        cff = FALSE,
                                        inst_citation = FALSE) {
  if (isTRUE(yml_cite)) {
    return(invisible(TRUE))
  }
  if (isFALSE(yml_cite)) {
    yml_cite <- character()
  }
  yml_cite <- yml_cite |>
    c("codemeta"[codemeta], "cff"[cff], "inst-citation"[inst_citation]) |>
    unique()
  if (length(setdiff(c("codemeta", "cff", "inst-citation"), yml_cite)) == 0L) {
    yml_cite <- TRUE
  }
  if (length(yml_cite) == 0L) {
    yml_cite <- FALSE
  }
  yml_cite
}

.projr_yml_cite_get <- function(method) {
  yml_cite <- .projr_yml_get_root_default()[["build"]][["cite"]]
  cite_vec <- c("codemeta", "cff", "inst-citation")
  if (isTRUE(yml_cite)) {
    return(cite_vec)
  }
  if (is.null(yml_cite) || isFALSE(yml_cite)) {
    return(character())
  }
  cite_vec
}

# git
# -----------------

.projr_yml_set_git <- function(all = NULL,
                               commit = NULL,
                               add_untracked = NULL,
                               push = NULL) {
  if (!is.null(all)) {
    commit <- all
    add_untracked <- all
    push <- all
  }
  if (!is.null(commit)) {
    .projr_yml_set_git_commit(commit)
  }
  if (!is.null(add_untracked)) {
    .projr_yml_set_git_add_untracked(add_untracked)
  }
  if (!is.null(push)) {
    .projr_yml_set_git_push(push)
  }
}

.projr_yml_unset_git <- function() {
  .projr_yml_set_git(all = FALSE)
}

.projr_yml_set_git_commit <- function(commit = TRUE) {
  commit_pre <- .projr_yml_get_git_commit()
  if (identical(commit_pre, commit)) {
    return(invisible(FALSE))
  }
  push <- .projr_yml_get_git_push()
  add_untracked <- .projr_yml_get_git_add_untracked()
  combn_vec <- c(commit, push, add_untracked) |>
    stats::setNames(c("commit", "add_untracked", "push"))
  if (all(combn_vec)) {
    .projr_yml_set_git_true()
  } else if (!any(combn_vec)) {
    .projr_yml_set_git_false()
  } else {
    .projr_yml_set_git_mix(as.list(combn_vec), "commit")
  }
  invisible(TRUE)
}

.projr_yml_get_git_commit <- function() {
  yml_git <- .projr_yml_get_root_default()[["build"]][["git"]]
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  yml_git[["commit"]]
}

.projr_yml_set_git_true <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_git_orig <- yml_projr_root[["build"]][["git"]]
  if (!is.null(yml_git_orig)) {
    yml_projr_root[["build"]][["git"]] <- TRUE
    .projr_yml_set_root(yml_projr_root)
  }
  return(invisible(TRUE))
}

.projr_yml_set_git_false <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["git"]] <- FALSE
  .projr_yml_set_root(yml_projr_root)
  return(invisible(TRUE))
}

.projr_yml_set_git_mix <- function(yml_git_full, nm) {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_git_orig <- yml_projr_root[["build"]][["git"]]
  nm_vec_sel <- factor(
    c(nm, names(yml_git_orig)) |> unique(),
    levels = c("commit", "add-untracked", "push")
  ) |>
    sort() |>
    as.character()
  yml_git <- yml_git_full[nm_vec_sel]
  yml_projr_root[["build"]][["git"]] <- yml_git
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}

.projr_yml_set_git_add_untracked <- function(add_untracked = TRUE) {
  add_untracked_pre <- .projr_yml_get_git_add_untracked()
  if (identical(add_untracked_pre, add_untracked)) {
    return(invisible(FALSE))
  }
  push <- .projr_yml_get_git_push()
  commit <- .projr_yml_get_git_commit()
  combn_vec <- c(commit, push, add_untracked) |>
    stats::setNames(c("commit", "add_untracked", "push"))
  if (all(combn_vec)) {
    .projr_yml_set_git_true()
  } else if (!any(combn_vec)) {
    .projr_yml_set_git_false()
  } else {
    .projr_yml_set_git_mix(as.list(combn_vec), "add-untracked")
  }
}

.projr_yml_get_git_add_untracked <- function() {
  yml_git <- .projr_yml_get_root_default()[["build"]][["git"]]
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  yml_git[["add-untracked"]]
}

.projr_yml_set_git_push <- function(push = TRUE) {
  push_pre <- .projr_yml_get_git_push()
  if (identical(push_pre, push)) {
    return(invisible(FALSE))
  }
  commit <- .projr_yml_get_git_commit()
  add_untracked <- .projr_yml_get_git_add_untracked()
  combn_vec <- c(commit, push, add_untracked) |>
    stats::setNames(c("commit", "add_untracked", "push"))
  if (all(combn_vec)) {
    .projr_yml_set_git_true()
  } else if (!any(combn_vec)) {
    .projr_yml_set_git_false()
  } else {
    .projr_yml_set_git_mix(as.list(combn_vec), "push")
  }
  invisible(TRUE)
}

.projr_yml_get_git_push <- function() {
  yml_git <- .projr_yml_get_root_default()[["build"]][["git"]]
  if (is.null(yml_git) || isTRUE(yml_git)) {
    return(TRUE)
  }
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  yml_git[["push"]]
}

# github
# --------------------

.projr_yml_unset_github_dest <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_root[["build"]][["github"]] <- NULL
  .projr_yml_set_root(yml_projr_root)
  invisible(TRUE)
}
