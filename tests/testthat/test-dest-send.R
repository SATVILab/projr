test_that(".projr_remote_create works", {
  skip_if(TRUE)
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # adding
      # -----------------------------------------------

      # check add where type did not exist before
      yml_root_orig <- list("build" = list("package" = FALSE))
      yml_merge_final <- list("build" = list(
        "local" = list("Zlatan" = list("ID" = "MrMr", "Move" = "Karate"))
      ))
      expect_identical(
        .projr_dest_add_get_final(
          yml_root_orig = yml_root_orig,
          yml_merge_final = yml_merge_final,
          type = "local",
          title = "Zlatan"
        ),
        list(
          build = list(
            package = FALSE,
            local = list("Zlatan" = list("ID" = "MrMr", "Move" = "Karate"))
          )
        )
      )

      # check add where type did exist before
      yml_root_orig <- list("build" = list(
        "package" = FALSE,
        "local" = list("Zlatan" = list("ID" = "MrMr", "Move" = "Karate"))
      ))
      yml_merge_final <- yml_root_orig
      yml_merge_final[["build"]][["local"]][["ZlatanIsBack"]] <- list(
        "ID" = "KaPow", "Move" = "ChopChop"
      )
      expect_identical(
        .projr_dest_add_get_final(
          yml_root_orig = yml_root_orig,
          yml_merge_final = yml_merge_final,
          type = "local",
          title = "ZlatanIsBack"
        ),
        list(
          build = list(
            package = FALSE,
            "local" = list(
              Zlatan = list(ID = "MrMr", Move = "Karate"),
              ZlatanIsBack = list(ID = "KaPow", Move = "ChopChop")
            )
          )
        )
      )
    }
  )
})

test_that(".projr_remote_create works", {
  skip_if_offline()
  skip_if(FALSE)
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # adding
      # -----------------------------------------------

      # remote any pre-existing remotes
      .projr_test_yml_dest_remote_rm()

      # add it
      browser()
      debugonce(.projr_yml_remote_check_content)
      projr_dest_add_local(
        title = "Archive",
        content = "data-raw",
        path = "_archive"
      )
      yml_projr_build <- projr_yml_get()[["build"]]
      expect_true(
        identical(
          yml_projr_build[["local"]][["Archive"]],
          list(
            content = "data-raw",
            path = "_archive"
          )
        )
      )
    }
  )
})
