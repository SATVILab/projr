---
applyTo: "**/*.R"
---

# R Coding Standards for projr

## Purpose & Scope

Guidelines for writing R code in the projr package, including naming conventions, documentation requirements, and common patterns.

---

## Naming Conventions

### Function Names

- Use `.` prefix for internal (non-exported) functions (e.g., `.build_manifest_pre()`)
- Use `snake_case` for function and variable names
- Exported functions use the pattern `projr_*` (e.g., `projr_init_prompt()`, `projr_path_get_dir()`)
- Function names should use specific action verbs (e.g., `.hash_file()`, `.manifest_read()`)

### Variable Names

- **General principle**: Prefer clarity over brevity
- Use descriptive variable names (e.g., `label_vec`, `path_manifest`, `dir_test`)
- Avoid generic abbreviations like `nm`, `fn`, `tmp` without context

### Abbreviation Guidelines

**Acceptable abbreviations**:
- Industry-standard: `id`, `url`, `api`, `http`, `html`
- Mathematical/statistical: `var`, `std`, `min`, `max`, `avg`
- Common suffixes with descriptive base: `_vec`, `_df`, `_list` (e.g., `label_vec`, `results_df`)
- Established domain terms: `param`, `config`, `repo`

**Context-specific naming** (instead of generic `nm`):
- Configuration: `config_name`, `setting_name`, `field_name`
- Files: `filename`, `script_name`, `document_name`
- Users: `user_name`, `author_name`
- Projects: `project_name`, `package_name`

**File path naming** (instead of generic `fn`):
- Use `filename` for just the name (e.g., `"data.csv"`)
- Use `filepath` for full path (e.g., `"path/to/data.csv"`)
- Use `relative_path` or `absolute_path` when distinction matters
- Prefer plural for vectors: `filenames`, `filepaths`

**Parameter naming**:
- Use full words for parameters: `excluded_dirs` not `dir_exc`
- Parallel structure: `excluded_dirs`, `included_dirs`, `excluded_files`
- Be explicit: `source_type`/`target_type` instead of `type_pre`/`type_post`

**Temporal variables**:
- Use context-specific names instead of generic `_pre`/`_post`
- Temporal: `before`/`after`, `previous`/`current`, `old`/`new`, `initial`/`final`
- Spatial: `source`/`target`, `origin`/`destination`, `input`/`output`
- Comparison: `baseline`/`updated`, `reference`/`comparison`

### Examples

```r
# Good: Descriptive parameter names
.hash_dir <- function(path_dir, version = NULL, excluded_dirs = NULL) {
  # Clear what is being excluded
}

# Good: Context-specific naming
.yml_build_get <- function(setting_name, profile) {
  # Clear it's a configuration setting
}

# Good: Explicit file naming
.hash_file <- function(filepaths) {
  # Plural indicates vector, "paths" clarifies includes directory
}

# Good: Clear temporal distinction
.change_get_hash <- function(hash_before, hash_after) {
  # Or: old_hash/new_hash, baseline_hash/updated_hash
}

# Avoid: Generic abbreviations
.process <- function(nm, fn, tmp) {
  # What do these represent?
}
```

For comprehensive naming guidelines and additional examples, see `NAMING_SUGGESTIONS.md` in the repository root.

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

### Logging with .cli_debug

**DO NOT pass output_level as a parameter** to functions that use `.cli_debug()`:
- `.cli_debug()` automatically reads the `PROJR_OUTPUT_LEVEL` environment variable
- This reduces function signature complexity and maintenance burden
- No need to thread `output_level` through every function

**DO NOT add .cli_debug to high-volume operations**:
- Avoid logging for each file in a loop over many files
- Avoid logging for each item in large collections
- Keep logging focused on function entry/exit and key operations
- Example: Log "Processing 100 files" instead of logging each file

```r
# Correct: Use .cli_debug without output_level parameter
.my_function <- function(path_dir) {
  .cli_debug("Starting .my_function()")
  .cli_debug("  path_dir: {path_dir}")
  
  files <- list.files(path_dir)
  .cli_debug("  Found {length(files)} files")  # Summary, not per-file
  
  for (file in files) {
    # Process file without logging each one
    process_file(file)
  }
  
  .cli_debug("Finished .my_function()")
}

# Incorrect: Passing output_level parameter
.my_function <- function(path_dir) {
  .cli_debug("Starting")  # Don't do this
}

# Incorrect: Logging each file in a loop
for (file in files) {
  .cli_debug("Processing file: {file}")  # Too verbose!
  process_file(file)
}
```

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
