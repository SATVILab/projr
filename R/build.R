#' @rdname projr_build
#' @title Build project to output
#'
#' @description .build_output` Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' .build_major`, .build_minor` and .build_patch`
#' are wrappers around .build_output` with the version component
#' bumped set automatically, e.g. projr_build_major()` is equivalent
#' projr_build(bump_component = "major")`.
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
#' .PROFILE` to this value at the start of the build,
#  and set it to what it was before at the end of the build.
#' @param archive_github `TRUE`,`FALSE` orcharacter vector of
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
#' @param always_archive logical.
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
projr_build <- function(bump_component,
                        msg = NULL,
                        args_engine = list(),
                        profile = NULL,
                        archive_github = FALSE,
                        archive_local = FALSE,
                        always_archive = TRUE,
                        clear_output = NULL) {
  bump_component <- .build_output_get_bump_component(
    bump_component
  )
  msg <- .build_output_get_msg(msg)

  .build(
    bump_component = bump_component,
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output
  )
}

#' @rdname projr_build
#' @export
projr_build_major <- function(msg = NULL,
                              args_engine = list(),
                              profile = NULL,
                              archive_github = FALSE,
                              archive_local = FALSE,
                              always_archive = TRUE,
                              clear_output = NULL) {
  projr_build(
    bump_component = "major",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output
  )
}

#' @rdname projr_build
#' @export
projr_build_minor <- function(msg = NULL,
                              args_engine = list(),
                              profile = NULL,
                              archive_github = FALSE,
                              archive_local = FALSE,
                              always_archive = TRUE,
                              clear_output = NULL) {
  projr_build(
    bump_component = "minor",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output
  )
}

#' @rdname projr_build
#' @export
projr_build_patch <- function(msg = NULL,
                              args_engine = list(),
                              profile = NULL,
                              archive_github = FALSE,
                              archive_local = FALSE,
                              always_archive = TRUE,
                              clear_output = NULL) {
  projr_build(
    bump_component = "patch",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output
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
#' .PROFILE` to this value at the start of the build,
#  and set it to what it was before at the end of the build.
#'
#' @export
projr_build_dev <- function(file = NULL,
                            bump = FALSE,
                            old_dev_remove = TRUE,
                            args_engine = list(),
                            profile = NULL,
                            clear_output = "never") {
  # NULL if FALSE and "dev" if TRUE
  bump_component <- .build_dev_get_bump_component(bump)
  .build(
    file = file,
    bump_component = bump_component,
    old_dev_remove = TRUE,
    args_engine = args_engine,
    profile = profile,
    clear_output = clear_output
  )
}

.build_dev_get_bump_component <- function(bump) {
  switch(bump,
    "dev"
  )
}

.build <- function(file = NULL,
                   bump_component,
                   old_dev_remove = TRUE,
                   msg = "",
                   args_engine,
                   profile,
                   archive_github = FALSE,
                   archive_local = FALSE,
                   always_archive = TRUE,
                   clear_output = NULL) {
  if (!is.null(profile)) {
    old_profile <- Sys.getenv("PROJR_PROFILE")
    Sys.setenv(PROJR_PROFILE = profile)
    on.exit(Sys.setenv(PROJR_PROFILE = old_profile), add = TRUE)
  }
  # set and unset environment variables
  .env_set(unset = TRUE)
  on.exit(.env_unset(), add = TRUE)
  .build_ensure_version()
  clear_output <- .build_get_clear_output(clear_output)

  version_run_on_list <- .build_pre(
    bump_component, msg, clear_output, archive_local
  )
  time_start <- Sys.time()
  .build_impl(version_run_on_list, file, args_engine)
  total_time <- Sys.time() - time_start
  .build_post(
    version_run_on_list, bump_component, msg, old_dev_remove,
    archive_github, archive_local, always_archive, clear_output, total_time
  )
}

.build_get_output_run <- function(bump_component) {
  !(is.null(bump_component) || bump_component == "dev")
}

.build_ensure_version <- function() {
  if (!file.exists(.path_get("VERSION")) &&
        !file.exists(.path_get("DESCRIPTION"))) {
    projr_version_set("0.0.1")
  }
}

.build_get_clear_output <- function(clear_output) {
  if (!is.null(clear_output)) {
    .assert_string(clear_output, TRUE)
    .assert_in(clear_output, c("pre", "post", "never"))
    return(clear_output)
  }
  clear_output <- Sys.getenv("PROJR_CLEAR_OUTPUT", unset = "pre")
  .assert_string(clear_output, TRUE)
  .assert_in(clear_output, c("pre", "post", "never"))
  clear_output
}

# pre
# ------------------------
.build_pre <- function(bump_component,
                       msg,
                       clear_output,
                       archive_local) {
  projr_yml_check(NULL)
  # whether it's an output run  or not
  output_run <- .build_get_output_run(bump_component)

  # check required env vars are present, and
  # that we're not behind upstream remote,
  # and that we have Git and  Git remote setup
  .build_pre_check(output_run)

  # update reng, ignore files, doc directory and version
  .build_pre_document(output_run, archive_local)

  # get DESCRIPTION and build versions under all
  # build outcomes
  version_run_on_list <- .version_run_onwards_get(bump_component)

  # run any scripts
  .build_pre_script_run()

  # clear output and docs directories, and set
  # run version to output run version if need be
  .build_pre_setup_for_output_run(
    version_run_on_list, output_run, clear_output
  )

  # commit any unstaged files pre-run
  .build_pre_commit_git(
    bump_component, version_run_on_list, msg
  )


  # hash cache and raw directories
  .build_manifest_pre(output_run)

  # return version_run_on_list
  invisible(version_run_on_list)
}

# actual
# ------------------------
.build_impl <- function(version_run_on_list, file, args_engine) {
  .build_engine(
    file = file,
    version_run_on_list = version_run_on_list,
    args_engine = args_engine
  )
  invisible(version_run_on_list)
}

# post
# ------------------------
.build_post <- function(version_run_on_list,
                        bump_component,
                        msg,
                        old_dev_remove,
                        archive_github,
                        archive_local,
                        always_archive,
                        clear_output,
                        total_time) {
  output_run <- .build_get_output_run(bump_component)

  # move artefacts to unsafe, final directories
  .build_post_finalise_artefacts(
    bump_component, version_run_on_list, clear_output
  )

  # update documentation
  .build_post_document(bump_component, version_run_on_list, msg, total_time)

  # record (version, rmd, git)
  .build_post_commit_git(bump_component, version_run_on_list, msg)

  # send to remotes
  .build_post_send_dest(
    bump_component, old_dev_remove, archive_github, archive_local,
    always_archive
  )

  # run post-build scripts
  .build_post_script_run()

  # initate dev version
  .build_post_dev(bump_component, version_run_on_list, msg)

  # push
  .build_git_push(output_run)

  invisible(TRUE)
}

# update lock file, help files, citation files, README
# and CHANGELOG
.build_post_document <- function(bump_component,
                                 version_run_on_list,
                                 msg,
                                 total_time) {
  output_run <- .build_get_output_run(bump_component)
  .build_renv_snapshot(output_run)
  .build_roxygenise(output_run)
  .build_cite(output_run)
  .build_changelog_add(
    msg = msg,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list
  )
  .build_buildlog_add(
    msg = msg,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    total_time = total_time
  )
  # hash raw-data and outputs, then save manifest table
  .build_manifest_post(output_run)
  # update README
  .build_readme_rmd_render(output_run)
}


# organise files and folders (clear and copy)
.build_post_finalise_artefacts <- function(bump_component,
                                           version_run_on_list,
                                           clear_output) {
  output_run <- .build_get_output_run(bump_component)
  .build_clear_post(output_run, clear_output)

  # copy outputs to (final) output and data directories
  .build_copy(output_run, bump_component, version_run_on_list)
}

# record (version, rmd, git)
.build_post_commit_git <- function(bump_component,
                                   version_run_on_list,
                                   msg) {
  output_run <- .build_get_output_run(bump_component)

  # commit any files generated by run
  .build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )
}

# record (version, rmd, git)
.build_pre_commit_git <- function(bump_component,
                                  version_run_on_list,
                                  msg) {
  output_run <- .build_get_output_run(bump_component)

  # commit any files generated by run
  .build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "pre",
    msg = msg
  )
}

# record (version, rmd, git)
.build_post_commit_git <- function(bump_component,
                                   version_run_on_list,
                                   msg) {
  output_run <- .build_get_output_run(bump_component)

  # commit any files generated by run
  .build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )
}

# send to remotes
.build_post_send_dest <- function(bump_component,
                                  old_dev_remove,
                                  archive_github,
                                  archive_local,
                                  always_archive) {
  .dest_send(bump_component, archive_github, archive_local, always_archive)
  .build_clear_old(
    .build_get_output_run(bump_component), old_dev_remove
  )
}

# bump to dev
.build_post_dev <- function(bump_component,
                            version_run_on_list,
                            msg) {
  # set version
  .build_version_set_post(
    version_run_on_list = version_run_on_list,
    success = TRUE
  )

  # commit dev version
  .build_git_commit(
    output_run = .build_get_output_run(bump_component),
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "dev",
    msg = msg
  )
}
