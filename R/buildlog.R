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
  system_resources_txt <- .buildlog_get_system_resources()
  projr_yml_txt <- .buildlog_get_projr_yml()
  session_info_txt <- .buildlog_get_session_info()
  c(
    header_txt,
    desc_txt,
    metadata_txt,
    system_resources_txt,
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

.buildlog_get_system_resources <- function() {
  # Get system information
  sys_info <- Sys.info()
  
  # Get OS information
  os_name <- sys_info[["sysname"]]
  os_release <- sys_info[["release"]]
  os_version <- sys_info[["version"]]
  
  # Get CPU information
  cpu_cores <- parallel::detectCores(logical = TRUE)
  
  # Get memory information (platform-specific)
  memory_info <- .buildlog_get_memory_info()
  
  # Get disk space information
  disk_info <- .buildlog_get_disk_info()
  
  # Get R platform information
  r_platform <- R.version$platform
  r_arch <- R.version$arch
  
  c(
    "**System Resources**",
    "",
    paste0("- OS: ", os_name, " ", os_release),
    paste0("- OS Version: ", os_version),
    paste0("- Architecture: ", r_arch),
    paste0("- Platform: ", r_platform),
    paste0("- CPU Cores: ", cpu_cores),
    memory_info,
    disk_info,
    ""
  )
}

.buildlog_get_memory_info <- function() {
  # Try to get memory information based on platform
  sys_info <- Sys.info()
  
  if (sys_info[["sysname"]] == "Linux") {
    # Linux: use /proc/meminfo or free command
    mem_output <- tryCatch(
      {
        system("free -h 2>/dev/null | grep '^Mem:' | awk '{print $2}'", 
               intern = TRUE)
      },
      error = function(e) NULL
    )
    if (!is.null(mem_output) && length(mem_output) > 0 && mem_output != "") {
      return(paste0("- Total RAM: ", mem_output))
    }
  } else if (sys_info[["sysname"]] == "Darwin") {
    # macOS: use sysctl
    mem_output <- tryCatch(
      {
        system("sysctl -n hw.memsize 2>/dev/null", intern = TRUE)
      },
      error = function(e) NULL
    )
    if (!is.null(mem_output) && length(mem_output) > 0 && mem_output != "") {
      # Convert bytes to GB
      mem_bytes <- as.numeric(mem_output)
      mem_gb <- round(mem_bytes / (1024^3), 2)
      return(paste0("- Total RAM: ", mem_gb, " GB"))
    }
  } else if (sys_info[["sysname"]] == "Windows") {
    # Windows: memory.limit() returns memory in MB
    mem_mb <- tryCatch(
      suppressWarnings(memory.limit()),
      error = function(e) NULL
    )
    if (!is.null(mem_mb) && is.finite(mem_mb)) {
      mem_gb <- round(mem_mb / 1024, 2)
      return(paste0("- Total RAM: ", mem_gb, " GB"))
    }
  }
  
  # Fallback if unable to determine memory
  return("- Total RAM: Unable to determine")
}

.buildlog_get_disk_info <- function() {
  # Get disk space for current directory
  disk_output <- tryCatch(
    {
      if (Sys.info()[["sysname"]] == "Windows") {
        # Windows: use wmic or fsutil
        system("wmic logicaldisk get size,freespace 2>nul", intern = TRUE)
      } else {
        # Unix-like: use df
        system("df -h . 2>/dev/null | tail -1 | awk '{print $2\" total, \"$4\" available\"}'", 
               intern = TRUE)
      }
    },
    error = function(e) NULL
  )
  
  if (!is.null(disk_output) && length(disk_output) > 0 && disk_output != "") {
    if (Sys.info()[["sysname"]] == "Windows") {
      return("- Disk Space: See Windows disk info")
    } else {
      return(paste0("- Disk Space: ", disk_output))
    }
  }
  
  return("- Disk Space: Unable to determine")
}

.buildlog_read <- function() {
  path_buildlog <- .path_get("BUILDLOG.md")
  if (!file.exists(path_buildlog)) {
    return(c("# BUILDLOG", ""))
  }
  readLines(path_buildlog, warn = FALSE)
}

.buildlog_write <- function(txt) {
  path_buildlog <- .path_get("BUILDLOG.md")
  writeLines(txt, path_buildlog)
}
