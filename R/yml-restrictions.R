#' @rdname yml-restrictions
#' @title Set restrictions for production builds
#' @description Configure which branches may build and how projr checks Git
#'   and archive versions before a production build. Omit a setting or pass
#'   `NULL` to leave its current value in place.
#' @param branch `TRUE` allows any branch (default), a character vector allows
#'   the named branches, and `FALSE` prevents production builds on any branch.
#' @param not_behind Earlier spelling of `not_behind_git`.
#' @param profile Name of the profile to update. The default is `"default"`.
#' @param not_behind_git Whether to check if the local branch is behind its
#'   Git upstream. Defaults to `TRUE`.
#' @param not_behind_remote Which archives to check. `TRUE` (default) checks
#'   the destinations selected for publication by this build. `FALSE` skips
#'   archive version checks. To check specific destinations, supply a character
#'   vector of remote names such as `c("reports", "backup")`. Write a title
#'   registered under both types as `<type>:<title>`, for example
#'   `"github:reports"`. Selected archives are checked even when the build will
#'   not publish to them.
#' @param rule Which project version must be at least as recent as each
#'   selected archive's completed version. `"base_version_not_behind"` (or
#'   `"base"`, the default) compares the version before the bump.
#'   `"target_version_not_behind"` (or `"target"`) compares the version the
#'   build will create.
#' @return Invisibly returns `TRUE`.
#' @examples
#' \dontrun{
#' projr_yml_restrictions_set(branch = c("main", "dev"))
#' projr_yml_restrictions_set(not_behind_git = FALSE)
#' projr_yml_restrictions_set(
#'   not_behind_remote = c("github:reports", "backup"),
#'   rule = "target"
#' )
#' }
#' @export
projr_yml_restrictions_set <- function(
  branch = NULL,
  not_behind = NULL,
  profile = "default",
  not_behind_git = NULL,
  not_behind_remote = NULL,
  rule = NULL
) {
  profile <- profile %||% "default"
  .assert_string(profile, TRUE)
  .yml_restrictions_check_branch(branch)
  git <- .yml_restrictions_git_value(not_behind_git, not_behind)
  if (!is.null(not_behind_remote)) {
    not_behind_remote <- .yml_restrictions_remote_value(not_behind_remote)
  }
  if (!is.null(rule)) {
    rule <- .yml_restrictions_rule_value(rule)
  }

  updates <- Filter(
    Negate(is.null),
    list(
      branch = if (isFALSE(branch)) list() else branch,
      not_behind_git = git,
      not_behind_remote = not_behind_remote,
      rule = rule
    )
  )
  if (!length(updates)) {
    return(invisible(TRUE))
  }
  restrictions <- .yml_restrictions_get(profile) %||% list()
  for (key in names(updates)) {
    value <- updates[[key]]
    # A named profile may need an explicit default to override its base.
    omit <- profile == "default" &&
      (isTRUE(value) || (key == "rule" && value == "base_version_not_behind"))
    restrictions[[key]] <- if (omit) NULL else value
  }
  .yml_build_set_nm(restrictions, "restrictions", profile)
  invisible(TRUE)
}

.yml_restrictions_check_branch <- function(branch) {
  if (is.null(branch) || (is.list(branch) && !length(branch))) {
    return(invisible(TRUE))
  }
  if (!is.logical(branch) && !is.character(branch)) {
    stop(
      "build.restrictions.branch must be logical or character",
      call. = FALSE
    )
  }
  if (is.logical(branch)) {
    .assert_flag(branch)
  } else {
    .assert_chr(branch)
  }
}

.yml_restrictions_git_value <- function(git, legacy) {
  .assert_flag(git, nm = "build.restrictions.not_behind_git")
  .assert_flag(legacy, nm = "build.restrictions.not_behind")
  if (!is.null(git) && !is.null(legacy) && !identical(git, legacy)) {
    stop("Conflicting values for not_behind_git and not_behind.", call. = FALSE)
  }
  git %||% legacy
}

.yml_restrictions_remote_titles <- function(titles) {
  key <- "build.restrictions.not_behind_remote"
  if (!is.null(names(titles)) || is.null(titles)) {
    stop(key, " expects a list of archive titles.", call. = FALSE)
  }
  if (is.list(titles)) {
    for (title in titles) {
      .assert_string(title, TRUE, nm = key)
    }
    titles <- vapply(titles, identity, character(1), USE.NAMES = FALSE)
  }
  if (identical(titles, character())) {
    return(titles)
  }
  .assert_chr(titles, TRUE, nm = key)
  unique(titles)
}

.yml_restrictions_remote_value <- function(selection) {
  key <- "build.restrictions.not_behind_remote"
  if (is.logical(selection)) {
    .assert_flag(selection, TRUE, nm = key)
    return(selection)
  }
  if (is.list(selection) && length(names(selection))) {
    stop(
      key,
      " lists remote names. Write an ambiguous title as <type>:<title>, ",
      "for example github:reports.",
      call. = FALSE
    )
  }
  .yml_restrictions_remote_titles(selection)
}

.yml_restrictions_rule_value <- function(rule) {
  rules <- c("base_version_not_behind", "target_version_not_behind")
  .assert_in_single(
    rule,
    c(rules, "base", "target"),
    TRUE,
    nm = "build.restrictions.rule"
  )
  switch(rule, base = rules[[1L]], target = rules[[2L]], rule)
}

.yml_restrictions_get_branch <- function(profile) {
  branch <- .yml_restrictions_get(profile)[["branch"]] %||% TRUE
  if (is.list(branch) && !length(branch)) character(0) else branch
}

.yml_restrictions_get_not_behind_git <- function(profile) {
  .yml_restrictions_get(profile)[["not_behind_git"]] %||% TRUE
}

.yml_restrictions_get_not_behind <- .yml_restrictions_get_not_behind_git

.yml_restrictions_get_not_behind_remote <- function(profile) {
  selection <- .yml_restrictions_get(profile)[["not_behind_remote"]] %||% TRUE
  .yml_restrictions_remote_value(selection)
}

.yml_restrictions_get_rule <- function(profile) {
  rule <- .yml_restrictions_get(profile)[["rule"]] %||% "base"
  .yml_restrictions_rule_value(rule)
}

.yml_restrictions_get <- function(profile) {
  .yml_get(profile)[["build"]][["restrictions"]]
}
