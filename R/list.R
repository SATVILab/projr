.projr_list_add <- function(list_base, x, nm = NULL) {
  if (is.null(nm)) {
    nm <- deparse(substitute(x))
  }
  switch(class(x)[[1]],
    "NULL" = .projr_list_add_null(list_base),
    "list" = .projr_list_add_list(x, nm, list_base),
    .projr_list_add_default(x, nm, list_base)
  )
}

.projr_list_add_list <- function(x, nm, list_base) {
  if (.projr_state_len_z(x)) {
    return(list_base)
  }
  list_base[[nm]] <- x
  list_base
}

.projr_list_add_null <- function(list_base) {
  list_base
}

.projr_list_add_default <- function(x, nm, list_base) {
  list_base[[nm]] <- x
  list_base
}
