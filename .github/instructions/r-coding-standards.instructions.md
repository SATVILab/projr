---
applyTo: "**/*.R"
---

# R Coding Standards for projr

## Purpose & Scope

Guidelines for writing R code in the projr package, including naming conventions, documentation requirements, and common patterns.

---

## Naming Conventions

- Use `.` prefix for internal (non-exported) functions (e.g., `.build_manifest_pre()`)
- Use `snake_case` for function and variable names
- Exported functions use the pattern `projr_*` (e.g., `projr_init_prompt()`, `projr_path_get_dir()`)
- Use descriptive variable names (e.g., `label_vec`, `path_manifest`, `dir_test`)

## Code Style

- Use the native pipe `|>` for function composition
- Keep functions focused and modular
- Indent using 2 spaces
- Limit line length to reasonable width (no strict limit, but be readable)

## Documentation (roxygen2)

All exported functions must include:
- `#' @title`: Short one-line title
- `#' @description`: Detailed description
- `#' @param`: Document all parameters with type and description
- `#' @return`: Describe what the function returns (use `invisible(...)` when appropriate)
- `#' @export`: For exported functions only
- `#' @examples`: Provide working examples (wrap in `\dontrun{}` if needed)
- `#' @seealso`: Link to related functions when appropriate

After updating roxygen2 documentation, run `devtools::document()` to regenerate `.Rd` files in `man/`.

Internal functions (starting with `.`) should NOT have `@export` tags.

## Error Handling

- Use `stop()` for errors in internal functions
- Provide informative error messages
- Check for file/directory existence before operations

## Input Validation

- **Type validation**: Use `.assert_chr()` or `.assert_chr_min()` for character inputs, `.assert_string()` for single strings, `.assert_flag()` for logical flags
- **Empty vector handling**: Most filter functions accept empty character vectors (using `.assert_chr_min()`)
- **NULL handling**: Functions that accept NULL parameters validate only when not NULL
- **Required parameters**: Use `required = TRUE` in assertion functions
- **Early validation**: Validate inputs at the start of functions before processing

---

## Code Examples

```r
# Correct: Exported function with full documentation
#' @title Initialise project
#'
#' @description Initialise project with configuration
#'
#' @param yml_path_from character.
#' Path to YAML file to use as `_projr.yml`.
#' If not supplied, then default `_projr.yml` file is used.
#'
#' @param renv_force Logical.
#' Passed to `renv::init()`.
#' Default is \code{FALSE}.
#'
#' @export
projr_init_prompt <- function(yml_path_from = NULL, 
                               renv_force = FALSE) {
  # implementation
}

# Correct: Internal function with validation
.path_filter_spec <- function(fn, exc = NULL) {
  .assert_chr_min(fn, required = TRUE)
  if (is.null(exc)) return(fn)
  .assert_chr(exc, required = TRUE)
  # processing
}

# Incorrect: Exported function without documentation
my_function <- function(x) {
  x + 1
}

# Incorrect: Using old pipe operator
data %>% filter(x > 0) %>% mutate(y = x + 1)
```

---

## Common Patterns

### Conditional Execution

Many functions have an `output_run` parameter:

```r
.build_manifest_pre <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  # actual implementation
}
```

### Data Structures

- Use tibbles/data.frames for tabular data (manifests, etc.)
- Return empty tables with `.zero_tbl_get_manifest()` when appropriate
- Use lists for configurations (e.g., `nm_list`)

### Path Handling

- Use `.path_get()` for project-relative paths
- Use `.dir_get_cache_auto_version()` for cache directories
- Use `projr_path_get_dir()` for directory paths

## Dependencies

- **Core imports** (always available): renv, jsonlite, yaml, rprojroot, desc, fs, digest
- **Suggests** (optional): testthat, devtools, usethis, gert, gh, quarto, knitr, etc.
- When adding dependencies:
  - Add to `DESCRIPTION` under `Imports:` or `Suggests:`
  - Use `package::function()` notation for suggested packages
  - Update `renv.lock` with `renv::snapshot()`
