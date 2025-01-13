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
#' @param profile character.
#' `projr` profile to use. Will set the environment variable
#' `PROJR_PROFILE` to this value at the start of the build,
#  and set it to what it was before at the end of the build.
#' @param upload_github `TRUE`,`FALSE` orcharacter vector of 
#' directory labels.
#' If `TRUE`, then all directories (`raw-data`, `output`, etc)
#' are uploaded to a GitHub release named `archive`
#' as versioned files (e.g. `output-v0.1.2.zip`).
#' If `FALSE`, then no directories are uploaded.
#' If a character vector, then only the directories
#' specified are uploaded.
#' Default is `FALSE`.
#' Ignored if there is a release named `archive`
#' already specified as a destination in
#' the `projr` configuration file.
#' @param upload_force logical.
#' If `TRUE`, then the directories are uploaded
#' regardless of whether the directory to be uploaded
#' has exactly the same contents as the latest version
#' of the directory on the GitHub release.
#' Default is `TRUE`.
#' Ignored if there is a release named `archive`
#' already specified as a destination in
#' the `projr` configuration file.
#' @param args_engine list.
#' Arguments passed to the
#' rendering engine
#' (`rmarkdown::render`, `quarto::render` or `bookdown::render_book`).
#'
#' @export
projr_build_output <- function(bump_component,
                               msg = NULL,
                               args_engine = list(),
                               profile = NULL,
                               upload_github = FALSE,
                               upload_force = TRUE) {
  bump_component <- .projr_build_output_get_bump_component(
    bump_component
  )
  msg <- .projr_build_output_get_msg(msg)

  .projr_build(
    bump_component = bump_component,
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    upload_github = upload_github,
    upload_force = upload_force
  )
}

#' @rdname projr_build_output
#' @export
projr_build_major <- function(msg = NULL,
                              args_engine = list(),
                              profile = NULL,
                              upload_github = FALSE,
                              upload_force = TRUE) {
  projr_build_output(
    bump_component = "major",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    upload_github = upload_github,
    upload_force = upload_force
  )
}

#' @rdname projr_build_output
#' @export
projr_build_minor <- function(msg = NULL,
                              args_engine = list(),
                              profile = NULL,
                              upload_github = FALSE,
                              upload_force = TRUE) {
  projr_build_output(
    bump_component = "minor",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    upload_github = upload_github,
    upload_force = upload_force
  )
}

#' @rdname projr_build_output
#' @export
projr_build_patch <- function(msg = NULL,
                              args_engine = list(),
                              profile = NULL,
                              upload_github = FALSE,
                              upload_force = TRUE) {
  projr_build_output(
    bump_component = "patch",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    upload_github = upload_github,
    upload_force = upload_force
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
#' @param args_engine list.
#' Arguments passed to the
#' rendering engine
#' (`rmarkdown::render`, `quarto::render` or `bookdown::render_book`).
#' @param profile character.
#' `projr` profile to use. Will set the environment variable
#' `PROJR_PROFILE` to this value at the start of the build,
#  and set it to what it was before at the end of the build.
#'
#' @export
projr_build_dev <- function(file = NULL,
                            bump = FALSE,
                            old_dev_remove = TRUE,
                            args_engine = list(),
                            profile = NULL) {
  # NULL if FALSE and "dev" if TRUE
  bump_component <- .projr_build_dev_get_bump_component(bump)
  .projr_build(
    file = file,
    bump_component = bump_component,
    old_dev_remove = TRUE,
    args_engine = args_engine,
    profile = profile
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
                         args_engine,
                         profile,
                         upload_github = FALSE,
                         upload_force = TRUE) {
  if (!is.null(profile)) {
    old_profile <- Sys.getenv("PROJR_PROFILE")
    Sys.setenv(PROJR_PROFILE = profile)
    on.exit(Sys.setenv(PROJR_PROFILE = old_profile))
  }
  projr_env_file_activate()
  version_run_on_list <- .projr_build_pre(bump_component, msg)
  .projr_build_actual(version_run_on_list, file, args_engine)
  .projr_build_post(
    version_run_on_list, bump_component, msg, old_dev_remove,
    upload_github, upload_force
    )
  .projr_env_file_deactivate()
}

.projr_build_get_output_run <- function(bump_component) {
  !(is.null(bump_component) || bump_component == "dev")
}

# pre
# ------------------------
.projr_build_pre <- function(bump_component, msg) {
  projr_yml_check(NULL)
  # whether it's an output run  or not
  output_run <- .projr_build_get_output_run(bump_component)

  # check required env vars are present, and
  # that we're not behind upstream remote
  .projr_build_pre_check(output_run)

  # update reng, ignore files, doc directory and version
  .projr_build_pre_document(output_run)

  # get DESCRIPTION and build versions under all
  # build outcomes
  version_run_on_list <- .projr_version_run_onwards_get(bump_component)

  # run any scripts
  .projr_build_pre_script_run(bump_component)

  # commit any unstaged files pre-run
  .projr_build_pre_commit_git(
    output_run, bump_component, version_run_on_list, msg
  )

  # clear output and docs directories, and set
  # run version to output run version if need be
  .projr_build_pre_setup_for_output_run(
    version_run_on_list, output_run
  )

  # hash cache and raw directories
  .projr_build_manifest_pre(output_run)

  # return version_run_on_list
  invisible(version_run_on_list)
}

# actual
# ------------------------
.projr_build_actual <- function(version_run_on_list, file, args_engine) {
  .projr_build_engine(
    file = file,
    version_run_on_list = version_run_on_list,
    args_engine = args_engine
  )
  invisible(version_run_on_list)
}

# post
# ------------------------
.projr_build_post <- function(version_run_on_list,
                              bump_component,
                              msg,
                              old_dev_remove,
                              upload_github,
                              upload_force) {
  output_run <- .projr_build_get_output_run(bump_component)

  # move artefacts to unsafe, final directories
  .projr_build_post_finalise_artefacts(bump_component, version_run_on_list)

  # update documentation
  .projr_build_post_document(bump_component, version_run_on_list, msg)

  # record (version, rmd, git)
  .projr_build_post_commit_git(bump_component, version_run_on_list, msg)

  # send to remotes
  .projr_build_post_send_dest(
    bump_component, old_dev_remove, upload_github, upload_force
    )

  # run post-build scripts
  .projr_build_post_script_run(bump_component)

  # initate dev version
  .projr_build_post_dev(bump_component, version_run_on_list, msg)

  # push
  .projr_build_git_push(output_run)

  invisible(TRUE)
}

# update lock file, help files, citation files, README
# and CHANGELOG
.projr_build_post_document <- function(bump_component, version_run_on_list, msg) {
  output_run <- .projr_build_get_output_run(bump_component)
  .projr_build_renv_snapshot(output_run)
  .projr_build_roxygenise(output_run)
  .projr_build_cite(output_run)
  .projr_build_changelog_add(
    msg = msg,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list
  )
  # hash raw-data and outputs, then save manifest table
  .projr_build_manifest_post(output_run)
    # update README
  .projr_build_readme_rmd_render(output_run)
}


# organise files and folders (clear and copy)
.projr_build_post_finalise_artefacts <- function(bump_component, version_run_on_list) {
  output_run <- .projr_build_get_output_run(bump_component)
  .projr_build_clear_post(output_run)

  # copy outputs to (final) output and data directories
  .projr_build_copy(output_run, bump_component, version_run_on_list)
}

# record (version, rmd, git)
.projr_build_post_commit_git <- function(bump_component,
                                         version_run_on_list,
                                         msg) {
  output_run <- .projr_build_get_output_run(bump_component)

  # commit any files generated by run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )
}

# record (version, rmd, git)
.projr_build_pre_commit_git <- function(bump_component,
                                         version_run_on_list,
                                         msg) {
  output_run <- .projr_build_get_output_run(bump_component)

  # commit any files generated by run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "pre",
    msg = msg
  )
}
# record (version, rmd, git)
.projr_build_post_commit_git <- function(bump_component,
                                         version_run_on_list,
                                         msg) {
  output_run <- .projr_build_get_output_run(bump_component)

  # commit any files generated by run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )
}

# send to remotes
.projr_build_post_send_dest <- function(bump_component,
                                        old_dev_remove,
                                        github_latest,
                                        github_archive) {
  .projr_dest_send(bump_component, github_latest, github_archive)
  .projr_build_clear_old(
    .projr_build_get_output_run(bump_component), old_dev_remove
  )
}

# bump to dev
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
