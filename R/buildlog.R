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
  c(
    header_txt,
    desc_txt,
    metadata_txt,
    projr_yml_txt,
    "----",
    ""
  )
}

.buildlog_get_header <- function(version_run_on_list, bump_component) {
  version_txt <- .buildlog_get_version(version_run_on_list)
  c(
    paste0(
      version_txt, ": ",
      .change_get_author_time(),
      ""
    )
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

.buildlog_get_desc <- function(msg, bu) {
  c("**Description**", "", msg, "")
}

.buildlog_get_metadata <- function(total_time) {
  c(
    "**Metadata**",
    "",
    "- Total time: ", total_time,
    "- `projr` profile: ", projr_profile_get(),
    ""
  )
}

.buildlog_get_projr_yml <- function() {
  yml_projr <- projr_yml_get()
  path_projr_tmp <- file.path(tempdir(), "_projr_tmp.yml")
  writeLines(yml_projr, path_projr_tmp)
  yml_projr <- readLines(path_projr_tmp)
  invisible(file.remove(path_projr_tmp))
  c("**`projr` config**", "", "```yml`", yml_projr, "```", "")
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
