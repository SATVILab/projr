.projr_build_changelog_add <- function(msg,
                                       bump_component,
                                       version_run_on_list) {
  if (!.projr_build_changelog_check(bump_component)) {
    return(invisible(FALSE))
  }
  .projr_changelog_add(msg, bump_component, version_run_on_list)
}

.projr_build_changelog_check <- function(bump_component) {
  .projr_build_get_output_run(bump_component)
}

.projr_changelog_read <- function() {
  path_changelog <- .projr_dir_proj_get("CHANGELOG.md")
  if (!file.exists(path_changelog)) {
    return(c("# CHANGELOG", ""))
  }
  readLines(path_changelog)
}

.projr_changelog_add <- function(msg, bump_component, version_run_on_list) {
  init_txt <- c("# CHANGELOG", "")
  add_txt <- .projr_changelog_get_add(msg, bump_component, version_run_on_list)
  append_txt <- .projr_changelog_read()[-c(1, 2)]
  c(init_txt, add_txt, append_txt) |>
    .projr_changelog_write()
  .projr_dir_proj_get("CHANGELOG.md") |>
    .projr_newline_append()
}

.projr_changelog_get_add <- function(msg, bump_component, version_run_on_list) {
  .projr_changelog_get_line(msg, bump_component, version_run_on_list) |>
    .projr_changelog_get_entry(bump_component)
}


.projr_changelog_get_line <- function(msg, bump_component, version_run_on_list) {
  version_txt <- .projr_changelog_get_version(version_run_on_list)
  bump_txt <- .projr_changelog_get_bump_component(bump_component)
  .projr_change_get_line_final(bump_txt, version_txt, msg)
}

.projr_changelog_get_version <- function(version_run_on_list) {
  paste0("v", version_run_on_list[["desc"]][["success"]])
}

.projr_changelog_get_bump_component <- function(bump_component) {
  switch(bump_component,
    "patch" = tools::toTitleCase(bump_component),
    "minor" = paste0("*", tools::toTitleCase(bump_component), "*"),
    "major" = paste0("**", tools::toTitleCase(bump_component), "**")
  )
}

.projr_change_get_line_final <- function(bump, version, msg) {
  c(
    paste0(
      "- ", bump, " (", version, "): ",
      .projr_change_get_author_time()
    ),
    paste0("  - ", msg)
  )
}

.projr_change_get_author_time <- function() {
  author <- .projr_metadata_get_author_host()
  time <- .projr_metadata_get_time()
  if (!nzchar(author)) {
    return(paste0("(", time, ")"))
  }
  paste0("", author, " (", time, ")")
}

.projr_changelog_get_entry <- function(bump_component, line) {
  switch(bump_component,
    "patch" = line,
    "minor" = c(line, ""),
    "major" = c(line, "", "___", "")
  )
}

.projr_changelog_write <- function(txt) {
  path_changelog <- .projr_dir_proj_get("CHANGELOG.md")
  writeLines(txt, path_changelog)
}
