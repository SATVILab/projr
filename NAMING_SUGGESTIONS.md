# Variable and Function Naming Suggestions for projr

## Executive Summary

This document provides comprehensive suggestions for improving variable and function naming throughout the projr codebase to enhance code readability and maintainability. The recommendations follow the existing coding standards while providing more specific guidance on common abbreviations and naming patterns.

## Motivation

Clear, descriptive names improve:
- **Code readability**: Developers can understand purpose without context
- **Maintainability**: Easier to modify and debug code
- **Onboarding**: New contributors grasp concepts faster
- **Documentation**: Self-documenting code reduces need for comments

## Current State Analysis

The codebase uses many abbreviations consistently:
- `nm` appears ~301 times (typically means "name")
- `fn` appears ~325 times (typically means "filename" or "file path")
- `dir_exc` appears ~49 times (means "excluded directories")
- `pre`/`post` appear ~100/42 times (temporal indicators)
- `dest` appears ~16 times (means "destination")

## General Naming Principles

### 1. Prefer Full Words Over Abbreviations

**Rationale**: Full words are self-explanatory and reduce cognitive load.

**When to abbreviate**:
- Industry-standard abbreviations (e.g., `id`, `url`, `api`)
- Mathematical/statistical terms (e.g., `var`, `std`, `min`, `max`)
- Very common suffixes when base name is descriptive (e.g., `_vec`, `_tbl`, `_df`)

**When NOT to abbreviate**:
- Core domain concepts
- Function parameters visible to users
- Variables that appear in multiple contexts

### 2. Use Context-Specific Names

Instead of generic names like `nm`, use context-specific alternatives:
- Configuration context: `config_name`, `setting_name`
- User context: `user_name`, `author_name`
- File context: `file_name`, `script_name`
- Project context: `project_name`, `package_name`

### 3. Maintain Consistency Within Scope

If a pattern works well in one area, apply it consistently:
- `label_vec`, `version_vec`, `filename_vec` (consistent `_vec` suffix)
- `manifest_table`, `hash_table`, `config_table` (consistent `_table` suffix)

## Specific Recommendations

### 1. Variable Name: `nm` → Context-Specific Names

**Current Usage**: Used generically for any "name" variable

**Problems**:
- Unclear what type of name it represents
- Requires reading surrounding code for context
- Inconsistent with principle of descriptive naming

**Suggested Replacements**:

```r
# BEFORE
.list_add <- function(list_base, x, nm = NULL) {
  if (is.null(nm)) {
    nm <- deparse(substitute(x))
  }
  # ...
}

# AFTER
.list_add <- function(list_base, x, element_name = NULL) {
  if (is.null(element_name)) {
    element_name <- deparse(substitute(x))
  }
  # ...
}

# BEFORE
.yml_build_get_nm <- function(nm, profile) {
  # ...
}

# AFTER
.yml_build_get_nm <- function(setting_name, profile) {
  # or: config_key, field_name, depending on context
}

# BEFORE
.env_var_set <- function(nm, val) {
  # ...
}

# AFTER
.env_var_set <- function(var_name, value) {
  # Environment variable names are clear from context
}
```

**Context-Specific Alternatives**:
- Configuration/YAML: `config_name`, `setting_name`, `field_name`, `key_name`
- Files: `filename`, `script_name`, `document_name`
- Users/Authors: `user_name`, `author_name`
- Projects: `project_name`, `package_name`, `repo_name`
- Git: `branch_name`, `tag_name`, `commit_name`
- Variables: `var_name`, `param_name`

### 2. Variable Name: `fn` → `filename` or `filepath`

**Current Usage**: Represents either filenames or file paths

**Problems**:
- Ambiguous whether it includes path or just name
- `fn` could be confused with "function" abbreviation
- Different meanings in different contexts

**Suggested Replacements**:

```r
# BEFORE
.hash_file <- function(fn) {
  vapply(fn, .hash_file_single, character(1))
}

# AFTER
.hash_file <- function(filepaths) {
  # Use plural for vectors, "paths" clarifies it includes directory
  vapply(filepaths, .hash_file_single, character(1))
}

# BEFORE
.remote_file_rm_osf <- function(fn, remote) {
  # ...
}

# AFTER
.remote_file_rm_osf <- function(filenames, remote) {
  # Just names without paths in this context
}

# BEFORE - When path is relative
fn_vec <- .file_ls(path_dir)
fn_vec <- file.path(path_dir, fn_vec)

# AFTER
relative_paths <- .file_ls(path_dir)
absolute_paths <- file.path(path_dir, relative_paths)
```

**Guidelines**:
- Use `filename` when referring to just the file name (e.g., `"data.csv"`)
- Use `filepath` when including directory path (e.g., `"path/to/data.csv"`)
- Use `relative_path` or `absolute_path` when distinction matters
- Prefer plural forms for vectors (e.g., `filenames`, `filepaths`)

### 3. Parameter Names: `dir_exc` → `excluded_dirs`, `dir_inc` → `included_dirs`

**Current Usage**: Function parameters for directory filtering

**Problems**:
- Abbreviations not immediately clear to new users
- Inconsistent with R naming conventions for clarity

**Suggested Replacements**:

```r
# BEFORE
.hash_dir <- function(path_dir, version = NULL, dir_exc = NULL) {
  fn_vec <- .path_filter_spec(fn_vec, dir_exc)
  # ...
}

# AFTER
.hash_dir <- function(path_dir, version = NULL, excluded_dirs = NULL) {
  fn_vec <- .path_filter_spec(fn_vec, excluded_dirs)
  # ...
}

# BEFORE
.zip_dir <- function(path_dir, path_zip, dir_exc = NULL, dir_inc = NULL, fn_exc = NULL) {
  # ...
}

# AFTER
.zip_dir <- function(path_dir, 
                     path_zip, 
                     excluded_dirs = NULL, 
                     included_dirs = NULL, 
                     excluded_files = NULL) {
  # Parallel structure: excluded_dirs, included_dirs, excluded_files
}
```

**Benefits**:
- Self-documenting parameters
- Consistent naming pattern
- Easier to remember without consulting documentation

### 4. Temporal Variables: `pre` / `post` → Context-Specific Names

**Current Usage**: Variables named with `_pre` and `_post` suffixes

**Problems**:
- Generic temporal indicators
- Can be confusing when multiple before/after states exist
- Better to be explicit about what state they represent

**Suggested Replacements**:

```r
# BEFORE
.change_get_hash <- function(hash_pre, hash_post) {
  fn_vec_pre_lgl_removed <- !hash_pre[["fn"]] %in% hash_post[["fn"]]
  # ...
}

# AFTER
.change_get_hash <- function(hash_before, hash_after) {
  # or more specifically:
  # (old_hash, new_hash)
  # (previous_hash, current_hash)
  # (baseline_hash, updated_hash)
  files_removed <- !hash_before[["fn"]] %in% hash_after[["fn"]]
  # ...
}

# BEFORE
.change_get_file <- function(type_pre = NULL,
                             remote_pre = NULL,
                             type_post = NULL,
                             remote_post = NULL) {
  # ...
}

# AFTER
.change_get_file <- function(source_type = NULL,
                             source_remote = NULL,
                             target_type = NULL,
                             target_remote = NULL) {
  # Clearer: we're comparing source vs target
  # Or: previous_type/current_type if temporal is key
}
```

**Context-Specific Alternatives**:
- Temporal: `before`/`after`, `previous`/`current`, `old`/`new`, `initial`/`final`
- Spatial: `source`/`target`, `origin`/`destination`, `input`/`output`
- Comparison: `baseline`/`updated`, `reference`/`comparison`

### 5. Variable Name: `dest` → `destination`

**Current Usage**: Short for "destination" in remote operations

**Suggested Replacement**:

```r
# BEFORE
.remote_ls_dest <- function() {
  # ...
}

# AFTER
.remote_ls_destination <- function() {
  # Full word is clearer
}

# BEFORE
.dest_send <- function(bump_component, ...) {
  # ...
}

# AFTER
.destination_send <- function(bump_component, ...) {
  # Or even better: .send_to_destination()
}
```

### 6. Data Structure Suffixes: `_tbl` → `_table` or `_df`

**Current Usage**: Minimal in current codebase, but worth addressing

**Suggested Pattern**:

```r
# Good patterns (existing)
hash_table <- .hash_dir(path)
manifest_table <- .manifest_read(path)

# Avoid
hash_tbl <- .hash_dir(path)  # Less clear

# For data frames specifically
results_df <- compute_results()
```

**Guidelines**:
- Use `_table` for general tabular data
- Use `_df` when specifically a data.frame
- Use `_tibble` when specifically a tibble
- Keep suffix consistent within a function

### 7. Variable Name: `par` → `parameter` or `param`

**Current Usage**: Abbreviated parameter references

**Suggested Replacement**:

```r
# BEFORE
.par_get_option <- function(par_list, par_vec) {
  # ...
}

# AFTER
.param_get_option <- function(param_list, param_names) {
  # "param" is acceptable abbreviation, widely understood
  # "param_names" clarifies it's the names not values
}
```

## Function Naming Recommendations

### 1. Action Verbs Should Be Specific

**Principle**: Function names should clearly indicate what action they perform.

```r
# Good (existing patterns)
.hash_file(filepaths)          # Clear: hashes files
.manifest_read(path)           # Clear: reads manifest
.remote_file_rm_github(...)    # Clear: removes files from GitHub

# Could be improved
.change_get(...)               # Get what kind of change? 
# Better: .change_detect(), .change_calculate()

.dest_send(...)                # Send what where?
# Better: .send_to_destination(), .upload_to_remote()
```

### 2. Prefixes Should Indicate Scope

**Existing Good Patterns**:
- `.build_*` - Build-related operations
- `.git_*` - Git operations
- `.remote_*` - Remote operations
- `.yml_*` - YAML configuration operations
- `.manifest_*` - Manifest operations
- `.hash_*` - Hashing operations

**Maintain Consistency**: When adding new functions, use established prefixes.

### 3. Helper Function Names Should Be Descriptive

```r
# BEFORE
.change_get_manifest_get_closest_mismatch <- function(...) {
  # Name is long but descriptive
}

# This is actually GOOD - the length reflects complexity
# Better than: .change_get_manifest_closest(...)
# which loses information about what "closest" means
```

**Principle**: Don't sacrifice clarity for brevity in internal helper functions.

## Implementation Strategy

### Phase 1: Documentation (Current)
- ✅ Create this suggestions document
- ✅ Add to repository for team review
- Get team consensus on naming conventions

### Phase 2: Guidelines Enhancement
- Update `.github/instructions/r-coding-standards.instructions.md`
- Add specific abbreviation guidelines
- Include before/after examples
- Create quick reference table

### Phase 3: Gradual Refactoring (Future)
- Refactor during related changes (don't change just for naming)
- Update related tests and documentation together
- Use deprecation warnings for exported functions
- Maintain backward compatibility where needed

## Quick Reference Table

| Abbreviation | Context | Suggested Replacement | Rationale |
|--------------|---------|----------------------|-----------|
| `nm` | Generic | `name`, `element_name` | Too generic |
| `nm` | Configuration | `config_name`, `setting_name`, `field_name` | Context-specific |
| `nm` | Files | `filename`, `script_name` | Domain-specific |
| `fn` | File paths | `filepath`, `filepaths` (plural) | Clarifies includes path |
| `fn` | File names only | `filename`, `filenames` (plural) | Clarifies name only |
| `dir_exc` | Parameter | `excluded_dirs` | Self-documenting |
| `dir_inc` | Parameter | `included_dirs` | Parallel structure |
| `fn_exc` | Parameter | `excluded_files` | Consistent pattern |
| `tbl` | Data structure | `table`, `_df`, `_tibble` | Explicit type |
| `vec` | Vector suffix | Keep as `_vec` | Acceptable suffix when base is clear |
| `pre` | Temporal | `before`, `previous`, `old`, `initial` | Context-specific |
| `post` | Temporal | `after`, `current`, `new`, `final` | Context-specific |
| `pre` | Comparison | `source`, `baseline`, `reference` | Semantic meaning |
| `post` | Comparison | `target`, `updated`, `comparison` | Semantic meaning |
| `dest` | General | `destination` | Full word clearer |
| `par` | Parameters | `param`, `parameter` | Standard abbreviation |

## Examples of Good Existing Names

The codebase already has many well-named functions and variables:

```r
# Excellent descriptive names
projr_build_dev()                    # Clear purpose
projr_manifest_last_change()         # Self-documenting
projr_path_get_dir()                 # Explicit action
.build_manifest_pre()                # Clear when in build process
.remote_file_get_all()               # Explicit: all files
.git_last_commit_get()               # Clear what information
.yml_dir_get_label()                 # Clear hierarchy
.assert_string(param, required = TRUE)  # Descriptive validation

# Good use of descriptive suffixes
label_vec                            # Label vector - clear
path_manifest                        # Path to manifest - clear
dir_test                             # Test directory - clear
version_vec                          # Version vector - clear
```

## Anti-Patterns to Avoid

```r
# ❌ Single-letter variables (except loop counters)
x <- compute()          # What is x?
d <- get_data()         # What does d contain?

# ❌ Ambiguous abbreviations
tmp <- process()        # Temporary what?
res <- analyze()        # Result of what?

# ❌ Abbreviating twice
fn_vec_fn <- ...        # Too many abbreviations

# ✅ Clear alternatives
computed_value <- compute()
participant_data <- get_data()
temporary_directory <- process()
analysis_results <- analyze()
filename_vector <- ...
```

## Balancing Brevity and Clarity

**Prefer clarity over brevity**, but recognize:

1. **Very local scope** (within a few lines): Short names acceptable
   ```r
   for (i in seq_along(items)) {  # "i" is fine for loop counter
     # ...
   }
   ```

2. **Mathematical operations**: Standard abbreviations okay
   ```r
   mean_val <- mean(x)
   std_dev <- sd(x)
   ```

3. **Well-established domain terms**: Keep them
   ```r
   api_url <- "..."
   html_content <- "..."
   ```

4. **Everything else**: Prefer full, descriptive names

## Conclusion

Improving variable and function names throughout projr will:
- Enhance code readability for all contributors
- Reduce onboarding time for new developers
- Make the codebase more maintainable long-term
- Align with R community best practices

This document serves as a reference for future development. Refactoring should happen gradually during related changes, not in bulk, to maintain code stability and minimize test churn.

## References

- [R Packages: Style Guide](https://r-pkgs.org/code.html#code-style)
- [Google's R Style Guide](https://google.github.io/styleguide/Rguide.html)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [Clean Code by Robert Martin](https://www.oreilly.com/library/view/clean-code-a/9780136083238/)

---

**Document Status**: Draft for team review
**Created**: 2025-12-09
**Related Issue**: Suggest more descriptive variable and function names
