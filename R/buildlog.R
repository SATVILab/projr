.build_buildlog_add <- function(msg,
                                bump_component,
                                version_run_on_list,
                                total_time) {
  if (!.build_buildlog_check(bump_component)) {
    return(invisible(FALSE))
  }
  .buildlog_add(
    msg, bump_component, version_run_on_list, total_time
  )
}
.build_buildlog_check <- function(bump_component) {
  .build_get_output_run(bump_component)
}

.buildlog_add <- function(msg,
                          bump_component,
                          version_run_on_list,
                          total_time) {
  init_txt <- c("# BUILDLOG", "")
  add_txt <- .buildlog_get_add(
    msg, bump_component, version_run_on_list, total_time
  )
  append_txt <- .buildlog_read()[-c(1, 2)]
  c(init_txt, add_txt, append_txt) |>
    .buildlog_write()
  .path_get("BUILDLOG.md") |>
    .newline_append()
}

.buildlog_get_add <- function(msg,
                              bump_component,
                              version_run_on_list,
                              total_time) {
  header_txt <- .buildlog_get_header(
    version_run_on_list, bump_component
  )
  desc_txt <- .buildlog_get_desc(msg)
  metadata_txt <- .buildlog_get_metadata(total_time)
  projr_yml_txt <- .buildlog_get_projr_yml()
  session_info_txt <- .buildlog_get_session_info()
  c(
    header_txt,
    desc_txt,
    metadata_txt,
    projr_yml_txt,
    session_info_txt,
    "----",
    ""
  )
}

.buildlog_get_header <- function(version_run_on_list, bump_component) {
  version_txt <- .buildlog_get_version(version_run_on_list, bump_component)
  c(
    paste0(
      version_txt, ": ",
      .change_get_author_time()
    ),
    ""
  )
}


.buildlog_get_version <- function(version_run_on_list, bump_component) {
  version_init <- paste0("v", version_run_on_list[["desc"]][["success"]])
  switch(bump_component,
    "patch" = paste0("#### ", version_init),
    "minor" = paste0("### ", version_init),
    "major" = paste0("## ", version_init)
  )
}

.buildlog_get_desc <- function(msg) {
  c("**Description**", "", msg, "")
}

.buildlog_get_metadata <- function(total_time) {
  c(
    "**Metadata**",
    "",
    "- Total time: ", .buildlog_get_metadata_time(total_time),
    "- `projr` profile: ", projr_profile_get(),
    ""
  )
}

.buildlog_get_metadata_time <- function(duration) {
  total_sec <- as.numeric(duration, units = "secs")
  weeks   <- total_sec %/% 604800
  days    <- (total_sec %% 604800) %/% 86400
  hours   <- (total_sec %% 86400) %/% 3600
  minutes <- (total_sec %% 3600) %/% 60
  seconds <- round(total_sec %% 60)

  parts <- character()
  if (weeks > 0) {
    parts <- c(parts, sprintf("%dw", weeks))
    # Always include days if weeks are present
    parts <- c(parts, sprintf("%dd", days))
    parts <- c(parts, sprintf("%dhr", hours))
    parts <- c(parts, sprintf("%dmin", minutes))
  } else if (days > 0) {
    parts <- c(parts, sprintf("%dd", days))
    parts <- c(parts, sprintf("%dhr", hours))
    parts <- c(parts, sprintf("%dmin", minutes))
  } else if (hours > 0) {
    parts <- c(parts, sprintf("%dhr", hours))
    parts <- c(parts, sprintf("%dmin", minutes))
  } else if (minutes > 0) {
    parts <- c(parts, sprintf("%dmin", minutes))
  }
  # Always include seconds
  parts <- c(parts, sprintf("%ds", seconds))

  paste(parts, collapse = " ")
}

.buildlog_get_projr_yml <- function() {
  yml_projr <- yaml::as.yaml(projr::projr_yml_get())
  c("**`projr` config**", "", "```yaml", yml_projr, "```", "")
}

.buildlog_get_session_info <- function() {
  c(
    "**Session info**",
    "",
    "```",
    utils::capture.output(sessionInfo()),
    "```",
    ""
  )
}


.buildlog_read <- function() {
  path_buildlog <- .path_get("BUILDLOG.md")
  if (!file.exists(path_buildlog)) {
    return(c("# BUILDLOG", ""))
  }
  readLines(path_buildlog)
}

.buildlog_write <- function(txt) {
  path_buildlog <- .path_get("BUILDLOG.md")
  writeLines(txt, path_buildlog)
}
