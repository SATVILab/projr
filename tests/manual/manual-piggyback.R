# what do I need to do?
# - Set up a repo as before
Sys.unsetenv("PROJR_TEST")
# initialise, choosing some other repo name
projr::projr_init()
projr::projr_version_set("0.0.1")


# check that a GitHub PAT is configured
(gh_account <- gh::gh_whoami())
(me <- gh_account$login)

gh::gh(
    "DELETE /repos/:username/:pkg",
    username = me,
    pkg = "TailRank"
)



version_format_list <- .projr_version_format_list_get()
bump_component <- "minor"
version_current <- "0.1.0"
yml_projr_dir <- projr_yml_get()[["directories"]]
if (dev_run_n && "github-release" %in% names(yml_projr[["build-output"]])) {
    version_comp_vec <- version_format_list[["components"]] |>
        setdiff("dev")
    version_comp_vec <- version_comp_vec[
        seq_len(which(version_comp_vec == bump_component))
    ]
    gh_list <- yml_projr[["build-output"]][["github-release"]]
    version_current <- projr_version_get()
    if (!requireNamespace("piggyback", quietly = TRUE)) {
        renv::install("piggyback")
    }
    gh_tbl_releases <- piggyback::pb_releases()
    if ("source_code" %in% names(gh_list)) {
        item_list <- gh_list[[which(names(gh_list) == "source-code")]]
        if (!item_list[["add"]]) next
        item_version <- item_list[["version-component-bumped"]]
        if (item_version != "any" && !item_version %in% version_comp_vec) {
            next
        }
        tag <- paste0("v", version_current)
        body <- "Project source code, inputs and/or outputs"
        if (tag %in% gh_tbl_releases[["release_name"]]) {
            piggyback::pb_release_delete(tag = tag, body = body)
        }
        piggyback::pb_release_create()
    }
    gh_list <- gh_list[-which(names(gh_list) == "source-code")]
    for (i in seq_along(gh_list)) {
        gh_tbl_releases <- piggyback::pb_releases()

        # =============================
        # sorting out tag and body
        # =============================

        label <- names(gh_list)[i]
        if (!item_list[["add"]]) {
            next
        }
        tag <- switch(item_list[["name"]],
            "@version" = paste0("v", version_current),
            item_list[["name"]]
        )
        body <- switch(item_list[["name"]],
            "@version" = paste0("Version-linked source code, project inputs and/or outputs"),
            "Project source code, inputs and/or outputs"
        )

        # ============================
        # uploading report
        # ============================

        if (label == "bookdown") {
            dir_bookdown <- projr_dir_get("bookdown")
            path_zip <- file.path(
                dirname(dir_bookdown), "bookdown.zip"
            )
            if (file.exists(path_zip)) {
                file.remove(path_zip)
            }
            if (!dir.exists(dirname(path_zip))) {
                dir.create(dirname(path_zip))
            }
            setwd(dir_bookdown)
            path_zip <- paste0(basename(dir_bookdown), ".zip")
            utils::zip(
                path_zip,
                files = list.files(
                    getwd(),
                    recursive = TRUE, full.names = FALSE, all.files = TRUE
                ),
                flags = "-r9Xq"
            )
            setwd(dir_proj)
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_bookdown, path_zip), overwrite = TRUE, tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_bookdown, path_zip), overwrite = TRUE, tag = tag
                )
            }
            next
        }

        # ======================================
        # uploading non-report and non-source-code items
        # ======================================

        dir_input <- projr_dir_get("label", output_safe = FALSE)
        if (!fs::is_absolute_path(dir_input)) {
            dir_input <- file.path(dir_proj, dir_input)
        }
        fn_vec <- list.files(
            dir_input,
            recursive = TRUE, all.files = TRUE,
            full.names = TRUE
        )

        # removing other items from output directory if present
        # -----------------------------------------------

        if (label == "output") {
            nm_rem_vec <- names(yml_projr_dir)
            nm_rem_vec <- nm_rem_vec[!grepl("^archive|^output$|^bookdown$", nm_rem_vec)]
            for (i in seq_along(nm_rem_vec)) {
                fn_vec <- fn_vec[
                    !basename(fn_vec) %in% paste0(nm_rem_vec, ".zip")
                ]
            }
            dir_bookdown <- projr_dir_get("bookdown")
            fn_vec <- fn_vec[
                !basename(fn_vec) == paste0(basename(dir_bookdown), ".zip")
            ]
        }

        # =======================================
        # BEGIN HERE
        # =======================================

        if (length(fn_vec) == 0) next

        if (is.null(zip_val)) {
            zip_ind <- TRUE
        } else {
            zip_ind <- zip_val
        }

        if (zip_ind) {
            setwd(dir_input)
            path_zip <- paste0(label, ".zip")
            if (file.exists(path_zip)) {
                file.remove(path_zip)
            }
            utils::zip(
                path_zip,
                files = list.files(
                    getwd(),
                    recursive = TRUE, full.names = FALSE, all.files = TRUE
                ),
                flags = "-r9Xq"
            )
            setwd(dir_proj)
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip),
                    overwrite = TRUE,
                    tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip),
                    overwrite = TRUE,
                    tag = tag
                )
            }
            unlink(file.path(dir_input, path_zip))
        } else {
            setwd(dir_input)
            fn_vec <- list.files(
                getwd(),
                all.files = TRUE, recursive = TRUE, full.names = FALSE,
            )
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip), overwrite = TRUE, tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip), overwrite = TRUE, tag = tag
                )
            }
        }
    }
}
