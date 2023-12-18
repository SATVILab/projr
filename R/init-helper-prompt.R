# prompt helpers
# --------------
.projr_init_prompt_ind <- function(.var = NULL,
                                   nm_item_long,
                                   option_default = NULL,
                                   allow_specify_other = TRUE,
                                   allow_complete_later = TRUE,
                                   answer_translate = NULL,
                                   answer_auto) {
  # set up
  # ----------------

  # get option check
  option_check <- c("Yes", "No")

  # get option other
  if (allow_specify_other) {
    option_other <- "Specify other"
  } else {
    option_other <- NULL
  }
  if (allow_complete_later) {
    option_other <- c(option_other, "Complete later")
  } else {
    option_other <- option_other
  }
  # choose question scheme when
  # pre-specified options given
  default_given <- nzchar(Sys.getenv(paste0("PROJR_", .var))) ||
    !is.null(option_default)

  # return auto answer
  # -------------------

  if (.is_testing()) {
    return(answer_auto)
  }

  # question
  # ----------------

  check_correct <- "No"

  while (check_correct == "No") {
    # question again
    answer_list <- .projr_init_prompt_ind_question(
      default_given = default_given,
      .var = .var,
      nm_item_long = nm_item_long,
      option_default = option_default,
      option_other = option_other
    )

    if (length(answer_list[["nm"]]) > 1) {
      message("Please only select one option.")
      next
    }

    # gave no answer, want to complete later
    if (!answer_list[["completed"]]) {
      break
    }
    # check if a provided answer is as desired
    check_correct <- .projr_init_prompt_ind_check(
      nm_long = nm_item_long,
      nm_item = answer_list[["nm"]],
      answer_translate = answer_translate,
      option_check = option_check
    )
  }

  # complete later
  if (!answer_list[["completed"]]) {
    return(NULL)
  }

  .projr_init_prompt_ind_translate(
    answer_list[["nm"]],
    answer_translate = answer_translate
  )
}

.projr_init_prompt_ind_question <- function(default_given,
                                            .var,
                                            nm_item_long,
                                            option_default,
                                            option_other) {
  if (default_given) {
    answer_list <- .projr_init_prompt_ind_default_given(
      .var = .var,
      nm_item_long = nm_item_long,
      option_default = option_default,
      option_other = option_other
    )
  } else {
    answer_list <- .projr_init_prompt_ind_default_not_given(
      nm_item_long = nm_item_long
    )
  }
  answer_list
}

.projr_init_prompt_ind_default_given <- function(.var,
                                                 nm_item_long,
                                                 option_default,
                                                 option_other) {
  request_default <- paste0("Please select ", nm_item_long, ".")
  request_default_n <- paste0("Please specify the ", nm_item_long, ".")
  # add variable to default
  nm_item_var <- strsplit(Sys.getenv(paste0("PROJR_", .var)), ";")[[1]]
  nm_item_default <- c(nm_item_var, option_default)
  # add alternative items to what we can ask
  nm_item_vec_full <- c(nm_item_default, option_other)
  # ask what they want to do
  answer_item <- utils::menu(
    nm_item_vec_full,
    title = request_default
  )
  cat("\n")
  # item chosen one of default
  nm_item <- nm_item_vec_full[answer_item]
  default_chosen <- answer_item %in% seq_along(nm_item_default)
  specify_chosen <- grepl("specify", tolower(nm_item_vec_full[answer_item]))
  later_chosen <- grepl("later", tolower(nm_item_vec_full[answer_item]))
  if (default_chosen) {
    completed_item <- TRUE
  } else if (nm_item_vec_full[answer_item] == nm_item_var) {
    # prompt to specify answer if requested
  } else if (specify_chosen) {
    cat(paste0(request_default_n, "\n"))
    nm_item <- readline(prompt = ">> ")
    cat("\n")
    completed_item <- TRUE
  } else if (later_chosen) {
    completed_item <- FALSE
    nm_item <- NULL
  } else {
    stop("Answer not recognised")
  }

  list(
    "completed" = completed_item,
    "nm" = nm_item
  )
}

.projr_init_prompt_ind_default_not_given <- function(nm_item_long) {
  request_default_n <- paste0("Please specify the ", nm_item_long, ".")
  cat(paste0(request_default_n, "\n"))
  nm_item <- readline(prompt = ">> ")
  cat("\n")
  list("completed" = TRUE, "nm" = nm_item)
}

.projr_init_prompt_ind_check <- function(nm_long,
                                         nm_item,
                                         answer_translate,
                                         option_check) {
  nm_check <- .projr_init_prompt_ind_translate(
    nm_item, answer_translate
  )
  check_init <- paste0(
    "Is the ", nm_long, " `", nm_check, "` correct?"
  )
  answer <- utils::menu(option_check, title = check_init)
  cat("\n")
  option_check[answer]
}

.projr_init_prompt_ind_translate <- function(nm_item,
                                             answer_translate) {
  if (is.null(answer_translate)) {
    return(nm_item)
  }
  answer_translate[[nm_item]]
}


.projr_init_prompt_yn <- function(question,
                                  answer_auto = 2) {
  yn_vec <- c("Yes", "No")
  if (.is_testing()) {
    return(answer_auto)
  }
  utils::menu(yn_vec, title = question)
}
