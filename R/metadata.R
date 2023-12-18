.projr_metadata_get_author_host <- function() {
  user_name <- .projr_git_config_get_name()
  if (nzchar(user_name)) {
    return(user_name)
  }
  use_name <- .projr_metadata_get_author_host_env()
  if (nzchar(use_name)) {
    return(use_name)
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
  if (nzchar(user_name)) {
    return(user_name)
  }
  user_name <- Sys.info()[["user"]]
  if (!identical(user, "unknown")) {
    return(user)
  }
  character()
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