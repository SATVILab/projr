.projr_metadata_get_author_host <- function() {
  user_name <- .projr_git_config_get_name()
  if (.is_len_1(user_name) && .is_string(user_name)) {
    return(user_name)
  }
  user_name <- .projr_metadata_get_author_host_env()
  if (.is_len_1(user_name) && .is_string(user_name)) {
    return(user_name)
  }
  .projr_metadata_get_author_sys_info()
}

.projr_metadata_get_author_host_env <- function() {
  switch(.projr_metadata_get_os(),
    "Windows" = Sys.getenv("USERNAME"),
    "Linux" = ,
    "Darwin" = Sys.getenv("USER"),
    stop(paste0(.projr_metadata_get_os(), " not recognised"))
  )
}

.projr_metadata_get_author_sys_info <- function() {
  user_name <- Sys.info()[["user"]]
  if (.is_len_1(user_name) && .is_string(user_name)) {
    return(user_name)
  }
  user_name <- Sys.info()[["user"]]
  if (
    .is_len_1(user_name) && !identical(user_name, "unknown") &&
      .is_string(user_name)
  ) {
    return(user_name)
  }
  "anonymous-user"
}

.projr_metadata_get_os <- function() {
  Sys.info()[["sysname"]]
}

.projr_metadata_get_host <- function() {
  Sys.info()[["nodename"]]
}

.projr_metadata_get_time <- function() {
  Sys.time() |> format("%H:%M:%S")
}
