# Design

## Design philosophy

This article explains the design decisions behind projr and why it works
the way it does.

------------------------------------------------------------------------

### Design goals

#### 1. Minimal cognitive overhead

**Goal**: Reduce the mental burden of maintaining reproducible research.

**How projr achieves this:**

- **One function to remember**:
  [`projr_build()`](https://satvilab.github.io/projr/reference/projr_build.md)
  does everything
- **Sensible defaults**: Most projects work out-of-the-box
- **Convention over configuration**: Standard directory structure
- **Gradual complexity**: Start simple, customise as needed

**Anti-pattern**: Complex build systems requiring extensive
configuration, multiple commands, and deep understanding of internals.

**Example**: Compare a typical `make`-based workflow:

``` bash
# Traditional approach
make clean
make data
make analysis
make figures
make paper
make deploy-to-osf
git add .
git commit -m "Update"
git push
gh release create v0.1.0
# ... upload files manually ...
```

With projr:

``` r
projr_build()  # That's it
```

#### 2. Fail-safe iteration

**Goal**: Make it safe to experiment without losing work.

**How projr achieves this:**

- **Dev builds**: Route outputs to cache, never touch `_output`
- **Manifests**: Always know what inputs created what outputs
- **Git integration**: Automatic commits preserve history
- **Reversible versioning**: Can always access previous versions via
  Git + archives

**Anti-pattern**: In-place modification of output directories leading to
lost results.

**Example**: Without projr, you might:

``` r
# Accidentally overwrite yesterday's figures
render("analysis.Rmd")  # Oh no, the new plot is worse!
# Now you've lost the good version
```

With projr:

``` r
# Safe iteration
projr_build_dev()  # Outputs to _tmp/
# Check results, if bad, just run again
# If good:
projr_build()  # Now commit to _output
```

#### 3. Automation without magic

**Goal**: Automate tedious tasks whilst maintaining transparency.

**How projr achieves this:**

- **Explicit configuration**: `_projr.yml` makes everything visible
- **Predictable behaviour**: Same inputs → same outputs
- **Inspectable artefacts**: Manifests, build logs, Git history
- **No hidden state**: All configuration in version-controlled files

**Anti-pattern**: Build systems with hidden state, implicit
dependencies, or configuration scattered across multiple locations.

**Example**: projr’s manifest system:

``` csv
label,path,hash,version,timestamp
raw-data,_raw_data,abc123...,v0.1.0,2024-01-15
```

You can inspect, version-control, and audit this. Compare to a system
that tracks dependencies in a binary database or in-memory cache.

#### 4. Reproducibility by default

**Goal**: Make it easier to be reproducible than not.

**How projr achieves this:**

- **Automatic versioning**: Every build is versioned
- **Manifests**: Inputs-outputs linkage is automatic
- **renv integration**: Optional package version locking
- **Archiving**: Automatic upload to GitHub/OSF
- **Restoration**: One command to reconstruct

**Anti-pattern**: Reproducibility as an afterthought requiring manual
effort.

**Example**: Without thinking about it, projr users get:

    v0.1.0:
      - code: Git SHA abc123
      - data: Hash def456
      - outputs: Hash ghi789
      - packages: renv.lock
      - archived: GitHub Release v0.1.0

All automatically. To reproduce:

``` r
projr_restore_repo("owner/repo")
renv::restore()
projr_build()
```

------------------------------------------------------------------------

### Core design principles

#### Single-purpose directories

**Principle**: Each directory has exactly one purpose.

**Rationale**:

- **Clarity**: No ambiguity about where things go
- **Selective sharing**: Archive only what’s needed
- **Automation**: Tools can act on directories knowing their contents
- **Restoration**: Simple mapping from label to path

**Trade-offs**:

- ✅ Structure is immediately obvious
- ✅ Easy to share/archive specific parts
- ❌ More directories than minimal structure
- ❌ Some redundancy (e.g., separate output-figures and output-tables)

**Why this trade-off is worth it**:

The clarity and automation benefits outweigh the slight increase in
directory count. Modern file systems handle many directories
efficiently.

#### Versioned builds, not versioned files

**Principle**: Version the entire project state, not individual files.

**Rationale**:

- **Coherence**: All files at version X are consistent with each other
- **Simplicity**: One version number, not per-file versions
- **Traceability**: Know exactly what produced what
- **Restoration**: Restore entire consistent state

**Trade-offs**:

- ✅ Simpler mental model (one version)
- ✅ Guarantees consistency
- ❌ Version bumps even for small changes
- ❌ Can’t mix versions of different components

**Why this trade-off is worth it**:

Scientific outputs depend on multiple inputs. Versioning the whole
project ensures you can always reconstruct a consistent state. Per-file
versioning leads to combinatorial explosion of possible states.

#### Configuration in YAML, not code

**Principle**: Project structure and build behaviour in `_projr.yml`,
not scattered across code.

**Rationale**:

- **Centralised**: One place to understand project configuration
- **Readable**: YAML is human-readable
- **Version-controlled**: Configuration changes are tracked
- **Shareable**: Easy to share configuration across projects

**Trade-offs**:

- ✅ Configuration is explicit and visible
- ✅ Easy to diff and merge configuration changes
- ❌ Less flexible than code-based configuration
- ❌ YAML syntax can be tricky

**Why this trade-off is worth it**:

Most research projects don’t need the flexibility of code-based
configuration. The benefits of having a single, readable,
version-controlled configuration file outweigh the limitations.

#### Dev builds vs final builds

**Principle**: Separate safe iteration from committed releases.

**Rationale**:

- **Safety**: Dev builds can’t accidentally overwrite released outputs
- **Speed**: Dev builds skip versioning and archiving
- **Clarity**: Explicit distinction between “testing” and “committing”

**Trade-offs**:

- ✅ Safe experimentation
- ✅ Fast feedback loop
- ❌ Two commands to remember (dev vs final)
- ❌ Cache directory can grow large

**Why this trade-off is worth it**:

The safety and speed benefits are critical for iterative research. The
cost of remembering two commands is minimal.

#### Git integration, not Git dependency

**Principle**: projr works with or without Git, but works better with
it.

**Rationale**:

- **Accessibility**: Beginners can use projr without learning Git
- **Power**: Advanced users get automatic Git integration
- **Flexibility**: Use Git features without learning them

**Trade-offs**:

- ✅ Low barrier to entry
- ✅ Automatic Git for those who want it
- ❌ More complex codebase (supporting both paths)
- ❌ Some features require Git (versioning)

**Why this trade-off is worth it**:

Git is powerful but intimidating. By making it optional, projr reaches
more users whilst still offering Git benefits to those who want them.

------------------------------------------------------------------------

### Architecture

#### Layered design

projr is organised into layers:

    User-facing API (projr_build, projr_init, ...)
             ↓
    Configuration layer (YAML parsing, validation)
             ↓
    Build engine (rendering, versioning, archiving)
             ↓
    Backend services (Git, GitHub, OSF, file system)

**Benefits**:

- **Modularity**: Each layer can be tested independently
- **Extensibility**: New backends (e.g., Zenodo) can be added
- **Clarity**: Separation of concerns

#### Function naming conventions

projr uses systematic naming:

- `projr_*` - All exported functions
- `.projr_*` - Internal functions (not exported)
- `projr_build*` - Build-related functions
- `projr_init*` - Initialisation functions
- `projr_yml_*` - YAML configuration functions
- `projr_path_*` - Path helper functions

**Benefits**:

- **Discoverability**: Autocomplete groups related functions
- **Clarity**: Function purpose is obvious from name
- **Namespace**: All public functions prefixed to avoid conflicts

#### Configuration precedence

projr uses this precedence for configuration:

1.  **Environment variables** (highest)
2.  **Profile YAML** (`_projr-{profile}.yml`)
3.  **Default YAML** (`_projr.yml`)
4.  **Built-in defaults** (lowest)

**Example**:

    # Built-in default
    output: _output

    # Overridden in _projr.yml
    output: _my_output

    # Overridden in _projr-dev.yml (if PROJR_PROFILE=dev)
    output: _dev_output

    # Overridden by environment variable (if set)
    PROJR_OUTPUT_DIR=_temp_output

Final value: `_temp_output`

**Benefits**:

- **Flexibility**: Different contexts without editing files
- **Explicitness**: Clear hierarchy of precedence
- **Debuggability**: Easy to trace where a setting comes from

#### Manifest format

Manifests use CSV for simplicity and compatibility:

``` csv
label,path,hash,version,timestamp
raw-data,_raw_data,abc123...,v0.1.0,2024-01-15T10:30:00Z
```

**Why CSV?**

- **Universal**: Readable by any tool (R, Python, Excel)
- **Simple**: No complex parsing
- **Diff-friendly**: Git can show line-by-line changes
- **Human-readable**: Open in text editor or spreadsheet

**Alternative considered**: JSON

- ✅ More structured
- ❌ Less human-readable
- ❌ Harder to diff
- ❌ Overkill for simple tabular data

------------------------------------------------------------------------

### Design decisions

#### Why semantic versioning?

**Decision**: Use x.y.z versioning (major.minor.patch)

**Rationale**:

- **Familiar**: Most developers know SemVer
- **Expressive**: Can communicate scale of changes
- **Tooling**: Many tools understand SemVer

**Alternative considered**: Date-based versioning (2024-01-15)

- ✅ Chronological ordering
- ❌ Doesn’t communicate significance of changes
- ❌ Multiple versions per day require disambiguation

#### Why default to GitHub Releases?

**Decision**: GitHub Releases is the default archive destination

**Rationale**:

- **Ubiquity**: Most R projects already use GitHub
- **Free**: Unlimited public releases, generous private quotas
- **Integrated**: Works with existing Git workflow
- **Accessible**: Web interface for downloads

**Alternative considered**: OSF as primary

- ✅ Designed for research
- ✅ Better for large datasets
- ❌ Separate account/authentication
- ❌ Less familiar to R developers

**Solution**: Support both; default to GitHub for familiarity.

#### Why clear \_output before builds?

**Decision**: Default to clearing `_output` before final builds

**Rationale**:

- **Correctness**: Ensures outputs match current code
- **No cruft**: Old outputs don’t linger
- **Idempotency**: Same code → same outputs

**Alternative considered**: Incremental updates

- ✅ Faster (only update changed files)
- ❌ Risk of stale files
- ❌ Non-deterministic (depends on previous state)

**Solution**: Clear by default; allow override via `PROJR_OUTPUT_CLEAR`.

#### Why route dev builds to cache?

**Decision**: Dev builds write to `_tmp/projr/v<version>/` not `_output`

**Rationale**:

- **Safety**: Can’t accidentally overwrite released outputs
- **Isolation**: Multiple dev builds don’t conflict
- **Cleanup**: Cache can be deleted without losing work

**Alternative considered**: Use `_output` with flag to prevent
overwrites

- ✅ Simpler mental model (one output location)
- ❌ Risk of accidental overwrites
- ❌ Harder to keep dev and release outputs separate

#### Why YAML not TOML/JSON?

**Decision**: Use YAML for configuration

**Rationale**:

- **Familiar**: Most R users know YAML (R Markdown, pkgdown)
- **Readable**: Comments, no quotes on strings
- **Expressive**: Supports lists, nested structures

**Alternatives considered**:

**TOML**: - ✅ Simpler syntax - ❌ Less familiar in R ecosystem - ❌
Harder to nest deeply

**JSON**: - ✅ Strict, machine-friendly - ❌ Less human-readable
(quotes, no comments) - ❌ Harder to hand-edit

------------------------------------------------------------------------

### Future directions

#### Potential enhancements

These are design considerations for future versions:

**1. Incremental builds**

**Idea**: Only rebuild changed documents

**Pros**: Faster builds, less re-rendering

**Cons**: More complexity, risk of stale outputs

**Decision**: Consider for v2.0 with careful invalidation logic

**2. Dependency graphs**

**Idea**: Track which outputs depend on which inputs

**Pros**: Finer-grained rebuilding, better traceability

**Cons**: Complexity, requires analysing code

**Decision**: Interesting but out-of-scope for now

**3. Remote execution**

**Idea**: Build on CI/cloud instead of locally

**Pros**: Reproducible environment, faster hardware

**Cons**: Network dependency, setup complexity

**Decision**: Possible via existing CI integrations (GitHub Actions)

**4. Multi-language support**

**Idea**: Support Python, Julia, etc., not just R

**Pros**: Broader audience, more use cases

**Cons**: Different ecosystems, more maintenance

**Decision**: Focus on R first; generalise later if demand exists

------------------------------------------------------------------------

### Comparison to alternatives

#### projr vs targets

**targets**: Pipeline tool for dependency tracking

**Similarities**: - Both focus on reproducibility - Both integrate with
R Markdown

**Differences**: - **targets**: Focuses on caching intermediate
results - **projr**: Focuses on versioning and archiving final outputs

**Use together?** Yes! Use targets for complex pipelines, projr for
versioning and sharing.

#### projr vs workflowr

**workflowr**: Website-based project template

**Similarities**: - Both provide project structure - Both integrate with
Git

**Differences**: - **workflowr**: Focuses on website generation -
**projr**: Focuses on versioning and archiving

**Use together?** Potentially, though there’s overlap in Git
integration.

#### projr vs usethis

**usethis**: Package development infrastructure

**Similarities**: - Both automate setup tasks - Both follow conventions

**Differences**: - **usethis**: For R packages - **projr**: For research
projects

**Use together?** Yes! Use usethis for package development, projr for
analysis projects.

------------------------------------------------------------------------

### Conclusion

projr’s design prioritises:

1.  **Simplicity**: One function does everything
2.  **Safety**: Dev builds can’t break releases
3.  **Transparency**: Configuration is visible and version-controlled
4.  **Reproducibility**: Automatic versioning and archiving

These principles guide every design decision, from directory structure
to function naming to configuration format.

The result is a tool that makes reproducible research easier than
non-reproducible research—which is exactly the point.
