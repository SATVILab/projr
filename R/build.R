#' @rdname projr_build_output
#' @title Build project to output
#'
#' @description `projr_build_output` Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' `projr_build_major`, `projr_build_minor` and `projr_build_patch`
#' are wrappers around `projr_build_output` with the version component
#' bumped set automatically, e.g. `projr_build_major()` is equivalent
#' `projr_build_output(bump_component = "major")`.
#'
#' @param bump_component "major", "minor", "patch" or missing.
#' Specifies version component to increment.
#' If missing, then is set equal to the lowest version component
#' in used version format.
#' No default (i.e. is missing by default).
#' @param msg character.
#' Message to append to Git commit messages.
#' Default is \code{NULL}, in which
#' case the user is prompted for a message or,
#' if the session is not interactive, it is
#' left empty.
#' Default is \code{NULL}.
#' Note that the Git messages in this case would not be blank -
#' they would simply consist of details as to the version
#' being bumped to and the stage in the build process
#' at which the commit was made.
#'
#' @param ... Arguments passed to \code{bookdown::render}.
#'
#' @export
projr_build_output <- function(bump_component,
                               old_output_cache = FALSE,
                               msg = NULL,
                               ...) {
  bump_component <- .projr_build_output_get_bump_component(
    bump_component
  )
  msg <- .projr_build_output_get_msg(msg)

  .projr_build(
    bump_component = bump_component,
    old_output_cache = old_output_cache,
    msg = msg, ...
  )
}

#' @rdname projr_build_output
#' @export
projr_build_major <- function(msg = NULL,
                              old_output_cache = FALSE,
                              ...) {
  projr_build_output(
    bump_component = "major",
    msg = msg,
    old_output_cache = old_output_cache,
    ...
  )
}

#' @rdname projr_build_output
#' @export
projr_build_minor <- function(msg = NULL,
                              old_output_cache = FALSE,
                              ...) {
  projr_build_output(
    bump_component = "minor",
    msg = msg,
    old_output_cache = old_output_cache,
    ...
  )
}

#' @rdname projr_build_output
#' @export
projr_build_patch <- function(msg = NULL,
                              old_output_cache = FALSE,
                              ...) {
  projr_build_output(
    bump_component = "patch",
    msg = msg,
    old_output_cache = old_output_cache,
    ...
  )
}

#' @title Build dev project
#'
#' @description Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' @param file character vector.
#' Paths to files to build.
#' Paths may be relative to project root, or absolute.
#' Default is NULL, in which case all files are built.
#' @param bump logical.
#' Whether to increment dev version for build.
#' Default is \code{FALSE}.
#' @param old_dev_remove logical.
#' If `TRUE`, then previous development builds are deleted
#' after a successful run.
#' @param ... Arguments passed to \code{bookdown::render}.
#'
#' @export
projr_build_dev <- function(file = NULL,
                            bump = FALSE,
                            old_dev_remove = TRUE, ...) {
  # NULL if FALSE and "dev" if TRUE
  bump_component <- .projr_build_dev_get_bump_component(bump)
  .projr_build(
    file = file,
    bump_component = bump_component,
    old_dev_remove = TRUE,
    ...
  )
}

.projr_build_dev_get_bump_component <- function(bump) {
  switch(bump,
    "dev"
  )
}

.projr_build <- function(file = NULL,
                         bump_component,
                         old_dev_remove = TRUE,
                         msg = "",
                         ...) {
  .projr_build_pre(bump_component, msg) |>
    .projr_build_actual(file) |>
    .projr_build_post(bump_component, msg, old_dev_remove)
}

.projr_build_get_output_run <- function(bump_component) {
  !(is.null(bump_component) || bump_component == "dev")
}

.projr_build_pre <- function(bump_component, msg) {
  # whether it's an output run  or not
  output_run <- .projr_build_get_output_run(bump_component)

  # set and check authorisation is available
  .projr_build_env_check(output_run)

  # check we are not missing upstream commits
  .projr_build_exit_if_behind_upstream()

  # get version for DESCRIPTION and bookdown from run onwards
  # snapshot if need be
  .projr_build_renv_snapshot(output_run)

  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .projr_build_ignore()

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .projr_build_doc_output_dir_update(FALSE)

  # get DESCRIPTION and build versions under all
  # build outcomes
  version_run_on_list <- .projr_version_run_onwards_get(bump_component)

  # run any scripts
  .projr_build_script_run(bump_component, "pre")

  # commit any unstaged files pre-run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "pre",
    msg = msg
  )

  # push files pre-run to notify others of build
  .projr_build_git_push(output_run)

  # set the version pre-run
  .projr_build_version_set_pre(version_run_on_list)

  # empty output directories
  # (docs, output and data)
  .projr_build_clear_pre(output_run)

  # hash cache
  .projr_build_hash_pre(output_run)

  # return version_run_on_list
  invisible(version_run_on_list)
}

.projr_build_actual <- function(version_run_on_list, file) {
  .projr_build_engine(
    file = file,
    version_run_on_list = version_run_on_list,
    ...
  )
  invisible(version_run_on_list)
}

.projr_build_post <- function(version_run_on_list,
                              bump_component,
                              msg,
                              old_dev_remove) {
  output_run <- .projr_build_get_output_run(bump_component)
  # update documentation
  .projr_build_post_docs(bump_component, version_run_on_list, msg)

  # organise files and folders (clear and copy)
  .projr_build_post_order(bump_component)

  # record (version, rmd, git)
  .projr_build_post_record(bump_component, version_run_on_list, msg)

  .projr_build_dest_send(bump_component)

  # run post-build scripts
  .projr_build_script_run(bump_component, "post")

  # initate dev version
  .projr_build_post_dev(bump_component, version_run_on_list, msg)

  # push
  .projr_build_git_push(output_run)

  invisible(TRUE)
}

.projr_build_post_docs <- function(bump_component, version_run_on_list, msg) {
  # update lock file, help files, citation files, README
  # and CHANGELOG
  output_run <- .projr_build_get_output_run(bump_component)
  .projr_build_renv_snapshot(output_run)
  .projr_build_roxygenise(output_run)
  .projr_build_cite(output_run)
  .projr_build_changelog_add(
    msg = msg,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list
  )
}

.projr_build_post_dev <- function(bump_component,
                                  version_run_on_list,
                                  msg) {
  # set version
  .projr_build_version_set_post(
    version_run_on_list = version_run_on_list,
    success = TRUE
  )

  # commit dev version
  .projr_build_git_commit(
    output_run = .projr_build_get_output_run(bump_component),
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "dev",
    msg = msg
  )
}

.projr_build_post_order <- function(bump_component) {
  output_run <- .projr_build_get_output_run(bump_component)
  .projr_build_clear_post(output_run)

  # copy outputs to (final) output and data directories
  .projr_build_copy(output_run, bump_component, version_run_on_list)
}

.projr_build_post_record <- function(bump_component,
                                     version_run_on_list,
                                     msg) {
  output_run <- .projr_build_get_output_run(bump_component)
  # hash data-raw and outputs, then save manifest table
  .projr_build_hash_post(output_run)

  # remove dev output files
  .projr_build_readme_rmd_render(output_run)

  # commit any files generated by run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )
}

.projr_build_dest_send <- function(bump_component) {
  .projr_dest_send(bump_component)
  .projr_build_clear_old(
    .projr_build_get_output_run(bump_component), old_dev_remove
  )
}
