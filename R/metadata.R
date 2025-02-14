.metadata_get_author_host <- function() {
  user_name <- .git_config_get_name()
  if (.is_len_1(user_name) && .is_string(user_name)) {
    return(user_name)
  }
  user_name <- .metadata_get_author_host_env()
  if (.is_len_1(user_name) && .is_string(user_name)) {
    return(user_name)
  }
  .metadata_get_author_sys_info()
}

.metadata_get_author_host_env <- function() {
  switch(.metadata_get_os(),
    "Windows" = Sys.getenv("USERNAME"),
    "Linux" = ,
    "Darwin" = Sys.getenv("USER"),
    stop(paste0(.metadata_get_os(), " not recognised"))
  )
}

.metadata_get_author_sys_info <- function() {
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

.metadata_get_os <- function() {
  Sys.info()[["sysname"]]
}

.metadata_get_host <- function() {
  Sys.info()[["nodename"]]
}

.metadata_get_time <- function() {
  Sys.time() |> format("%H:%M:%S", tz = "UTC")
}

.metadata_get_date <- function() {
  Sys.time() |> format("%Y-%m-%d", tz = "UTC")
}