# Directory license management
# ============================

# Core license generation
# -----------------------

.license_dir_create <- function(label, safe, profile = NULL) {
  # Get license configuration for this label
  license_config <- .yml_dir_get_license(label, profile)
  if (is.null(license_config)) {
    return(invisible(FALSE))
  }

  # Get directory path
  path_dir <- projr_path_get_dir(label, safe = safe)
  if (!dir.exists(path_dir)) {
    # Create directory if it doesn't exist
    dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Generate and write license
  .license_dir_write(path_dir, license_config)
  invisible(TRUE)
}

.license_dir_write <- function(path_dir, license_config) {
  # Parse license config
  # Authors from config take precedence over DESCRIPTION file
  license_info <- .license_config_parse(license_config)

  # Get template path
  template_path <- .license_template_get_path(license_info$type)
  if (!file.exists(template_path)) {
    stop("License template not found: ", license_info$type, call. = FALSE)
  }

  # Read and fill template
  license_text <- readLines(template_path, warn = FALSE)
  license_text <- .license_template_fill(
    license_text,
    license_info$authors,
    license_info$year
  )

  # Write to directory (always overwrites to ensure consistency with config)
  license_path <- file.path(path_dir, "LICENSE")
  writeLines(license_text, license_path)
  invisible(license_path)
}

.license_template_fill <- function(template_text, authors, year) {
  # Join authors with comma
  authors_text <- paste(authors, collapse = ", ")

  # Replace placeholders (use fixed = TRUE to avoid regex issues)
  template_text <- gsub("{{AUTHORS}}", authors_text, template_text, fixed = TRUE)
  template_text <- gsub("{{YEAR}}", as.character(year), template_text, fixed = TRUE)

  template_text
}

.license_template_get_path <- function(license_type) {
  # Normalize license type
  license_type <- .license_type_normalize(license_type)

  # Get path to template
  system.file("licenses", paste0(license_type, ".txt"), package = "projr")
}

.license_type_normalize <- function(license_type) {
  # Map common variations to canonical names
  license_map <- c(
    "ccby" = "CC-BY",
    "cc-by" = "CC-BY",
    "CC-BY" = "CC-BY",
    "cc0" = "CC0",
    "CC0" = "CC0",
    "apache" = "Apache-2.0",
    "apache-2.0" = "Apache-2.0",
    "Apache-2.0" = "Apache-2.0",
    "Apache 2.0" = "Apache-2.0",
    "mit" = "MIT",
    "MIT" = "MIT",
    "proprietary" = "Proprietary",
    "Proprietary" = "Proprietary"
  )

  if (!license_type %in% names(license_map)) {
    stop("Unknown license type: ", license_type, call. = FALSE)
  }
  license_map[[license_type]]
}

# Config parsing
# -----------------------

.license_config_parse <- function(license_config) {
  # Handle simple string format: just license type
  if (is.character(license_config) && length(license_config) == 1) {
    return(list(
      type = license_config,
      authors = .license_get_default_authors(),
      year = .license_get_default_year()
    ))
  }

  # Handle full config with type, authors, year
  .assert_in("type", names(license_config), TRUE)

  list(
    type = license_config[["type"]],
    authors = license_config[["authors"]] %||% .license_get_default_authors(),
    year = license_config[["year"]] %||% .license_get_default_year()
  )
}

.license_get_default_authors <- function() {
  # Try to get from DESCRIPTION
  if (file.exists(.path_get("DESCRIPTION"))) {
    desc <- desc::description$new(.path_get("DESCRIPTION"))
    authors <- desc$get_authors()
    if (length(authors) > 0) {
      author_names <- vapply(authors, function(a) {
        paste(a$given, a$family)
      }, character(1))
      return(author_names)
    }
  }

  # Fallback
  "Project Authors"
}

.license_get_default_year <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

# Build integration
# -----------------------

.license_dir_create_all_pre <- function(output_run, profile = NULL) {
  # Get input labels (raw-data, cache)
  label_vec <- .yml_dir_get_label_in(profile)

  # For input directories, always use unsafe (final) directories
  # Always create/update licenses to match projr config, even in dev builds
  for (label in label_vec) {
    .license_dir_create(label, safe = FALSE, profile)
  }

  invisible(TRUE)
}

.license_dir_create_all_post <- function(output_run, profile = NULL) {
  if (!output_run) {
    return(invisible(FALSE))
  }

  # Get output labels (output, docs, data)
  label_vec <- .yml_dir_get_label_out(profile)

  # For output directories, use safe for dev builds, unsafe for production builds
  # This matches the manifest hashing logic: safe = !output_run
  safe_val <- !output_run

  for (label in label_vec) {
    .license_dir_create(label, safe = safe_val, profile)
  }

  invisible(TRUE)
}
