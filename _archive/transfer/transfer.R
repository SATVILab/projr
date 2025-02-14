.send_file <- function(fn_rel,
                             path_dir_local,
                             dest,
                             remote_type,
                             conflict) {
  if (length(fn_rel) == 0L) {
    return(invisible(FALSE))
  }
  switch(remote_type,
    "osf" = .osf_send_file(
      fn_rel = fn_rel,
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict
    ),
    "local" = .local_send_file(
      fn_rel = fn_rel,
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict
    )
  )
}
