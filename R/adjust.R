projr_usr_add <- function(wd_var = "LOCAL_WORKSPACE_FOLDER") {
  if (nzchar(Sys.getenv(wd_var))) {
    wd <- Sys.getenv(wd_var)
  } else {
    wd <- normalizePath(getwd(), winslash = "/")
  }

  yml <- yaml::read_yaml("_projr.yml")

  yml_dir <- yml[["directories-default"]]
  for (i in seq_along(yml_dir)) {
    yml_dir_curr <- yml_dir[[i]][-which(names(yml_dir[[i]]) %in% c("ignore"))]
    yml_dir_curr$path <- ""
    yml_dir[[i]] <- yml_dir_curr
  }
  yml <- yml |>
    append(
      list(yml_dir) |>
        setNames(paste0("directories-", wd))
    )

  yaml::write_yaml(yml, "_projr.yml")
}
