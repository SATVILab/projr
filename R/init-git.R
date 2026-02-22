# ========================================
# Git
# ========================================

.init_git <- function(git, commit) {
  # need to add Git user config...
  if (!git) {
    return(invisible(FALSE))
  }
  .git_system_setup()
  .git_init()
  .ignore_auto(TRUE)
  if (commit) {
    .init_git_config()
    .init_git_commit()
  }
  .init_git_suggest_git()
  invisible(TRUE)
}

.init_git_config <- function() {
  config_list <- .git_config_get()
  .init_git_config_ind("user.name", config_list$name)
  .init_git_config_ind("user.email", config_list$email)
}

.init_git_config_ind <- function(key, val) {
  if (.is_string(val)) {
    return(invisible(FALSE))
  }

  val <- .init_git_config_ind_get_val(key)
  .init_git_config_ind_set(key, val)
  invisible(TRUE)
}

.git_config_get <- function() {
  switch(.git_system_get(),
    "git" = .git_config_get_git(),
    "gert" = .git_config_get_gert()
  )
}

.git_config_get_git <- function() {
  name <- .git_config_get_git_name()
  email <- .git_config_get_git_email()
  list(
    name = name,
    email = email
  )
}

.git_config_get_git_name <- function() {
  .system2_get_stdout("git", c("config", "--get", "user.name"))
}

.git_config_get_git_email <- function() {
  .system2_get_stdout("git", c("config", "--get", "user.email"))
}

.system2_get_stdout <- function(cmd, args) {
  tryCatch(
    {
      out <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE))
      # If system2 returns a numeric exit code (e.g. 127), treat as failure.
      if (is.numeric(out) && length(out) == 1L) {
        return(NULL)
      }
      status <- attr(out, "status")
      if (!is.null(status) && status != 0L) {
        return(NULL)
      }
      if (length(out) == 0L) {
        return(NULL)
      }
      trimws(out[1])
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )
}

.init_git_config_ind_get_default <- function(key) {
  tryCatch(
    {
      switch(key,
        "user.name" = paste0(Sys.info()[["user"]], "_", Sys.info()[["nodename"]]),
        "user.email" = paste0(
          Sys.info()[["user"]], "_", Sys.info()[["nodename"]], "@projr-test.com"
        ),
        "unrecognised_key"
      )
    },
    error = function(e) "default_value"
  )
}

.init_git_config_ind_get_val <- function(key) {
  if (.is_env_var_true("GITHUB_ACTIONS")) {
    .init_git_config_ind_get_val_gha(key)
  } else if (!.is_interactive_and_not_test()) {
    .init_git_config_ind_get_val_default(key)
  } else {
    .init_git_config_ind_get_default(key)
  }
}

.init_git_config_ind_get_val_gha <- function(key) {
  switch(key,
    "user.name" = "GitHub Actions",
    "user.email" = "filler-email@projr-test.com",
    "unrecognised_key"
  )
}

.init_git_config_ind_get_val_default <- function(key) {
  switch(key,
    "user.name" = paste0(Sys.info()[["user"]], "_", Sys.info()[["nodename"]]),
    "user.email" = paste0(
      Sys.info()[["user"]], "_", Sys.info()[["nodename"]], "@projr-test.com"
    ),
    "unrecognised_key"
  )
}

.init_git_config_ind_get_val_prompt <- function(key) {
  switch(key,
    "user.name" = .init_git_config_ind_get_val_prompt_user_name(),
    "user.email" = .init_git_config_ind_get_val_prompt_user_email(),
    stop(paste0("Unrecognised key: ", key))
  )
}

.init_git_config_ind_get_val_prompt_user_name <- function() {
  choice <- utils::menu(
    c("Yes", "No"),
    title = "Your Git user name is not set. Would you like to set it now?"
  )
  if (choice == 1) {
    user_name <- readline("Please enter your Git user name: ")
    while (!.is_string(user_name)) {
      user_name <- readline("Git user.name cannot be empty. Please enter your Git user name: ") # nolint line_length_linter.
    }
  } else {
    stop("Git user.name is required. Please configure it and try again.")
  }
  user_name
}

.init_git_config_ind_get_val_prompt_user_email <- function() {
  choice <- utils::menu(
    c("Yes", "No"),
    title = "Your Git user email is not set. Would you like to set it now?"
  )
  if (choice == 1) {
    user_email <- readline("Please enter your Git user email: ")
    while (!.is_string(user_email)) {
      user_email <- readline("Git user.email cannot be empty. Please enter your Git user email: ") # nolint line_length_linter.
    }
  } else {
    stop("Git user.email is required. Please configure it and try again.")
  }
  user_email
}

.init_git_config_ind_set <- function(key, val) {
  switch(.git_system_get(),
    "git" = system2("git", c("config", "--global", key, val)),
    "gert" = gert::git_config_global_set(key, val)
  )
  invisible(TRUE)
}

.git_config_get_gert <- function() {
  # Attempt to get the Git configuration table.
  .dep_install_only("gert")
  gitconfig_tbl <- tryCatch(
    gert::git_config(),
    error = function(e) {
      # Return an empty data frame if there's an error
      data.frame(
        name = character(),
        value = character(),
        level = character(),
        stringsAsFactors = FALSE
      )
    }
  )
  # Get the latest value for user.name, if any.
  name_cfg <- {
    ind_nm <- which(gitconfig_tbl$name == "user.name")
    if (length(ind_nm) == 0L) {
      NULL
    } else {
      gitconfig_tbl$value[ind_nm][length(ind_nm)]
    }
  }

  # Get the latest value for user.email, if any.
  email_cfg <- {
    ind_em <- which(gitconfig_tbl$name == "user.email")
    if (length(ind_em) == 0L) {
      NULL
    } else {
      gitconfig_tbl$value[ind_em][length(ind_em)]
    }
  }

  list(
    name = name_cfg,
    email = email_cfg
  )
}
