.build_changelog_add <- function(msg,
                                 bump_component,
                                 version_run_on_list) {
  if (!.build_changelog_check(bump_component)) {
    return(invisible(FALSE))
  }
  .changelog_add(msg, bump_component, version_run_on_list)
}

.build_changelog_check <- function(bump_component) {
  .build_get_output_run(bump_component)
}

.changelog_add <- function(msg, bump_component, version_run_on_list) {
  init_txt <- c("# CHANGELOG", "")
  add_txt <- .changelog_get_add(msg, bump_component, version_run_on_list)
  append_txt <- .changelog_read()[-c(1, 2)]
  c(init_txt, add_txt, append_txt) |>
    .changelog_write()
  .path_get("CHANGELOG.md") |>
    .newline_append()
}

.changelog_read <- function() {
  path_changelog <- .path_get("CHANGELOG.md")
  if (!file.exists(path_changelog)) {
    return(c("# CHANGELOG", ""))
  }
  readLines(path_changelog)
}

.changelog_get_add <- function(msg, bump_component, version_run_on_list) {
  .changelog_get_line(msg, bump_component, version_run_on_list) |>
    .changelog_get_entry(bump_component)
}


.changelog_get_line <- function(msg,
                                bump_component,
                                version_run_on_list) {
  version_txt <- .changelog_get_version(version_run_on_list)
  bump_txt <- .changelog_get_bump_component(bump_component)
  .change_get_line_final(bump_txt, version_txt, msg)
}

.changelog_get_version <- function(version_run_on_list) {
  paste0("v", version_run_on_list[["desc"]][["success"]])
}

.changelog_get_bump_component <- function(bump_component) {
  switch(bump_component,
    "patch" = tools::toTitleCase(bump_component),
    "minor" = paste0("*", tools::toTitleCase(bump_component), "*"),
    "major" = paste0("**", tools::toTitleCase(bump_component), "**")
  )
}

.change_get_line_final <- function(bump, version, msg) {
  c(
    paste0(
      "- ", bump, " (", version, "): ",
      .change_get_author_time()
    ),
    paste0("  - ", msg)
  )
}

.change_get_author_time <- function() {
  author <- .metadata_get_author_host()
  date <- .metadata_get_date()
  time <- .metadata_get_time()
  if (!nzchar(author)) {
    return(paste0("(", time, ")"))
  }
  paste0("", author, " (", date, " ", time, ")")
}

.changelog_get_entry <- function(line, bump_component) {
  switch(bump_component,
    "patch" = line,
    "minor" = c(line, ""),
    "major" = c(line, "", "___", "")
  )
}

.changelog_write <- function(txt) {
  path_changelog <- .path_get("CHANGELOG.md")
  writeLines(txt, path_changelog)
}
