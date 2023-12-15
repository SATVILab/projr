# =====================
# make plan
# =====================

# unique plans:
# add_all
# add_missing
# change
# add_all_if_missing
# delete_add_all
# delete_add_all_if_change

.projr_dest_send_get_plan <- function(version_source,
                                      sync_approach,
                                      type,
                                      structure) {
  # get exactly how we decide what to do with the changes
  switch(version_source,
    "none" = .projr_dest_send_get_plan_none(sync_approach),
    "manifest" = ,
    "file" = .projr_dest_send_get_plan_source(
      structure = structure,
      type = type,
      sync_approach = sync_approach
    ),
    stop(
      paste0("version_source '", version_source, "' not supported"),
      call. = FALSE
    )
  )
}

# ------------------------
# version source
# ------------------------

# basic if not using any
.projr_dest_send_get_plan_none <- function(sync_approach) {
  switch(sync_approach,
    "upload-all" = ,
    "upload-missing" = "add_all",
    "sync-using-deletion" = ,
    "sync-using-version" = "delete_add_all",
    stop(
      paste0("sync_approach '", sync_approach, "' not supported"),
      call. = FALSE
    )
  )
}

# more complex if using one
.projr_dest_send_get_plan_source <- function(structure,
                                             type,
                                             sync_approach) {
  switch(type,
    "github" = .projr_dest_send_get_plan_flat(
      sync_approach = sync_approach
    ),
    "local" = ,
    "osf" = .projr_dest_send_get_plan_hier(
      structure = structure,
      sync_approach = sync_approach
    ),
    stop(paste0("type '", type, "' not supported"), call. = FALSE)
  )
}

# ------------------------
# sync approach
# ------------------------

# flat remotes
.projr_dest_send_get_plan_flat <- function(sync_approach) {
  switch(sync_approach,
    "upload-all" = "add_all",
    "upload-missing" = "add_all_if_missing",
    "sync-using-deletion" = "delete_add_all",
    "sync-using-version" = "delete_add_all_if_change",
    stop(
      paste0("sync_approach '", sync_approach, "' not supported"),
      call. = FALSE
    )
  )
}

# hierarchical remotes
.projr_dest_send_get_plan_hier <- function(structure,
                                           sync_approach) {
  switch(sync_approach,
    "upload-all" = "add_all",
    "upload-missing" = .projr_dest_send_get_plan_hier_missing(
      structure = structure
    ),
    "sync-using-deletion" = "delete_add_all",
    "sync-using-version" = .projr_dest_send_get_plan_hier_version(
      structure = structure
    ),
    stop(
      paste0("sync_approach '", sync_approach, "' not supported"),
      call. = FALSE
    )
  )
}

# ------------------------
# remote structure
# ------------------------

.projr_dest_send_get_plan_hier_missing <- function(structure) {
  switch(structure,
    "latest" = "add_missing",
    "version" = "add_all",
    stop(paste0("structure '", structure, "' not supported"), call. = FALSE)
  )
}

.projr_dest_send_get_plan_hier_version <- function(structure) {
  switch(structure,
    "latest" = "change",
    "version" = "delete_add_all_if_change",
    stop(paste0("structure '", structure, "' not supported"), call. = FALSE)
  )
}
