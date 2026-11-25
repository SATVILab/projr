# Destination Send Workflow

This vignette explains the internal workflow for sending content to
remote destinations during builds. It describes the three main stages
that occur when projr uploads artifacts to GitHub releases or local
directories.

## Overview

When you run a production build
([`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md),
[`projr_build_minor()`](https://satvilab.github.io/projr/reference/projr_build.md),
or
[`projr_build_major()`](https://satvilab.github.io/projr/reference/projr_build.md)),
projr can automatically send artifacts to configured remote
destinations. This process is controlled by the `.dest_send_label()`
function in `R/dest-send-label.R`, which orchestrates three distinct
stages:

1.  **Getting the Remotes** - Resolve remote locations and metadata
2.  **Building the Plan** - Determine which files need to be added,
    removed, or updated
3.  **Implementing the Plan** - Execute the file operations and update
    remote metadata

On narrow screens, scroll a flowchart sideways to read its branches.
With a keyboard, focus the flowchart and use the arrow keys.

## Stage 1: Getting the Remotes

The first stage resolves remote locations and determines what versions
exist on the remote destination.

``` mermaid
flowchart TD
    Start([Start: .dsl_get_remotes]) --> GetPre[Get remote_pre<br/>Base remote location]
    GetPre --> GetDestFull[Get remote_dest_full<br/>Full version destination]
    GetDestFull --> GetDestEmpty[Get remote_dest_empty<br/>Empty version destination]
    GetDestEmpty --> GetVersionComp[Determine version_comp<br/>Version to compare against]
    
    GetVersionComp --> CheckInspect{inspect == 'none'?}
    CheckInspect -->|Yes| ReturnNull[version_comp = NULL]
    CheckInspect -->|No| CheckStructure{structure?}
    
    CheckStructure -->|latest| LatestLogic[Check if remote exists<br/>Trust based on manifest]
    CheckStructure -->|archive| ArchiveLogic[Find latest acceptable<br/>archive version]
    
    LatestLogic --> GetComp[Get remote_comp<br/>Remote for comparison]
    ArchiveLogic --> GetComp
    ReturnNull --> GetComp
    
    GetComp --> End([Return remote list])
    
    style Start fill:#e1f5e1
    style End fill:#e1f5e1
    style GetVersionComp fill:#fff4e1
```

**Key Outputs:**

- `remote_pre` - The base remote location (e.g., GitHub release tag,
  local directory path)
- `remote_dest_full` - Where files will be uploaded (full version, not
  empty)
- `remote_dest_empty` - Empty version variant (for GitHub empty
  placeholders)
- `version_comp` - Version to compare against (NULL if no trusted
  version available)
- `remote_comp` - The actual remote object used for comparison

**Version Comparison Logic:**

The `version_comp` determines which remote version can be trusted for
comparisons:

- **NULL** - No trusted version is available. If the destination exists
  and inspection is enabled, projr can inspect its files directly.
- **Version string** - Trusted version exists; compare local files
  against this version

For `latest` structure: - Uses the current remote contents if they
exist - Trusts manifest if available and valid

For `archive` structure: - Finds the latest archive version that matches
local state - Must be recent enough (no local changes since that
version) - For `inspect = "file"`, always trusts the version by hashing
files - For `inspect = "manifest"`, only trusts if manifest is valid

## Stage 2: Building the Plan

The second stage compares local and remote state to determine which
files need to be uploaded, removed, or updated.

``` mermaid
flowchart TD
    Start([Start: .dsl_get_plan]) --> GetFilenames[Get file lists<br/>fn_source & fn_dest]
    
    GetFilenames --> Strategy{send_strategy?}
    
    Strategy -->|upload-all| UploadAll[fn_add = all local files<br/>fn_rm = none]
    Strategy -->|upload-missing| UploadMissing[fn_add = local files not on remote<br/>fn_rm = none]
    Strategy -->|sync-diff| SyncDiff[Compare hashes<br/>fn_add = new + changed<br/>fn_rm = deleted]
    Strategy -->|sync-purge| SyncPurge[Compare hashes<br/>fn_add = all local<br/>purge = true]
    
    UploadAll --> BuildActions[Translate to actions]
    UploadMissing --> BuildActions
    SyncDiff --> BuildActions
    SyncPurge --> BuildActions
    
    BuildActions --> CheckCue{send_cue?}
    CheckCue -->|always| ProceedSend[Proceed with upload]
    CheckCue -->|if-change| CheckChanges{Any changes?}
    
    CheckChanges -->|Yes| ProceedSend
    CheckChanges -->|No| SkipSend[Reuse existing files]
    
    ProceedSend --> BuildMetadata[Build metadata updates<br/>- manifest<br/>- VERSION file<br/>- changelog]
    SkipSend --> BuildMetadata
    BuildMetadata --> End([Return plan])
    
    style Start fill:#e1f5e1
    style End fill:#e1f5e1
    style BuildActions fill:#fff4e1
```

**Key Outputs:**

- `fn_add` - Files to upload to the remote
- `fn_rm` - Files to remove from the remote
- `version` - Updated VERSION file content
- `manifest` - Updated directory manifest
- `purge` - Whether to purge all remote files before upload
- `ensure_remote_dest_exists` - Whether to create the remote if it
  doesn’t exist
- `is_remote_dest_empty` - Whether the remote will be empty after
  operations

**Send Strategies:**

Different strategies determine how local and remote files are compared:

- **upload-all** - Upload all local files, ignore remote state
- **upload-missing** - Only upload files that don’t exist on remote
- **sync-diff** - Upload new/changed files, remove deleted files
- **sync-purge** - Replace the selected label’s files with the local
  files

**Inspection Modes:**

The `inspect` parameter controls how projr determines what’s on the
remote:

- **manifest** - Use the remote manifest (fast, requires valid manifest)
- **file** - Download and hash actual files (slower, always accurate)
- **none** - Treat remote as empty (always upload everything)

## Stage 3: Implementing the Plan

The third stage executes the file operations and updates remote
metadata.

``` mermaid
flowchart TD
    Start([Start: .dsl_implement_plan]) --> CheckPurge{purge?}
    
    CheckPurge -->|Yes| PurgeRemote[Remove all files<br/>from remote_dest_full]
    CheckPurge -->|No| AddFiles
    
    PurgeRemote --> AddFiles[Add files from fn_add]
    
    AddFiles --> CheckAdd{fn_add empty?}
    CheckAdd -->|No| CreateIfNeeded{remote exists?}
    CheckAdd -->|Yes| RemoveFiles
    
    CreateIfNeeded -->|No| CreateRemote[Create remote destination]
    CreateIfNeeded -->|Yes| UploadFiles[Upload files to remote]
    
    CreateRemote --> UploadFiles
    UploadFiles --> RemoveFiles[Remove files from fn_rm]
    
    RemoveFiles --> CheckRm{fn_rm empty?}
    CheckRm -->|No| DeleteFiles[Delete files from remote]
    CheckRm -->|Yes| FinalizeRemotes
    
    DeleteFiles --> FinalizeRemotes[Finalize remote states<br/>- Remove empty variant if full exists<br/>- Create empty variant if needed]
    
    FinalizeRemotes --> CheckBoth{Both full & empty exist?}
    CheckBoth -->|Yes| RemoveEmpty[Remove empty remote]
    CheckBoth -->|No| CheckCreate{Create empty needed?}
    
    RemoveEmpty --> UpdateMetadata
    CheckCreate -->|Yes| CreateEmpty[Create empty remote]
    CheckCreate -->|No| UpdateMetadata
    
    CreateEmpty --> UpdateMetadata[Upload metadata files<br/>- manifest<br/>- VERSION file<br/>- CHANGELOG.md]
    
    UpdateMetadata --> End([Complete])
    
    style Start fill:#e1f5e1
    style End fill:#e1f5e1
    style FinalizeRemotes fill:#fff4e1
```

**Key Operations:**

1.  **Purge** (if `purge = TRUE`) - Clear the files at the destination
    for this label and version. Preserve its release metadata.
2.  **Add files** - Upload files in `fn_add` to remote destination
3.  **Remove files** - Delete files in `fn_rm` from remote destination
4.  **Finalize remotes** - Handle empty/full remote variants:
    - For GitHub: manages `-empty.zip` placeholder files
    - For local: manages empty version directories
    - Ensures only one variant exists (full takes priority)
5.  **Update metadata** - Write the directory histories, VERSION file,
    and optional CHANGELOG.md. Local destinations store histories in
    `manifests/`. GitHub Releases carry one `manifest.zip` asset.

**Remote Variants:**

For archive structures, projr manages two variants of each version:

- **Full remote** (`remote_dest_full`) - Contains actual files (e.g.,
  `output-v0.0.1.zip`)
- **Empty remote** (`remote_dest_empty`) - Placeholder when no files
  exist (e.g., `output-v0.0.1-empty.zip`)

This allows projr to distinguish between: - “Version never uploaded”
(neither variant exists) - “Version uploaded but empty” (empty variant
exists) - “Version uploaded with files” (full variant exists)

Only one variant exists at a time. When files are added to an empty
remote, the empty variant is deleted and the full variant is created.

## Complete Workflow

Here’s how all three stages work together during a build:

``` mermaid
flowchart TD
    Start([Production Build]) --> Loop{For each<br/>destination}
    
    Loop --> Stage1[Stage 1: Get Remotes<br/>.dsl_get_remotes]
    
    Stage1 --> Stage2[Stage 2: Build Plan<br/>.dsl_get_plan]
    
    Stage2 --> Stage3[Stage 3: Apply file operations<br/>and update metadata]
    
    Stage3 --> Loop
    Loop --> End([Build Complete])
    
    style Start fill:#e1f5e1
    style End fill:#e1f5e1
    style Stage1 fill:#e3f2fd
    style Stage2 fill:#fff9c4
    style Stage3 fill:#f3e5f5
```

**Build Integration:**

The destination send process is triggered during post-build operations:

1.  **Pre-build** - Clear directories, hash input files
2.  **Build** - Render documents and scripts
3.  **Post-build** - Hash output files, commit to git
4.  **Destination Send** - Upload to remotes (this workflow)
5.  **Dev Version Bump** - Increment to next dev version

For each selected destination and directory label, projr determines
which version to compare, plans the file operations, and records the
build in the destination’s history. A build that reuses an existing
upload still gets a manifest entry.

## Configuration Parameters

This example publishes `output` to a GitHub release named `reports`. The
`send` options shown are the defaults.

``` yaml
remotes:
  github:
    reports:
      structure: archive
      send:
        cue: if-change
        strategy: sync-diff
        inspect: manifest
build:
  dest:
    reports:
      content: [output]
```

With these settings, the first build uploads the directory’s files. If
the next build has the same filenames and contents, projr can reuse the
first upload. The release’s `manifest.zip` records both builds with the
same snapshot ID. If a file changes, the new version gets its own files
and file list. Older versions remain available on the release.

For a destination that always holds the current files, change the
definition to `structure: latest` and use the settings below under
`send`.

``` yaml
cue: always
strategy: sync-purge
inspect: none
```

This replaces the files for the selected label on every publication. Its
history can identify a previous build’s files, but retrieving them still
requires a copy with the recorded contents.

## Trust and manifest validation

For manifest-based comparisons, projr checks the recorded version and
compares the destination’s file list for that version with the analysis
manifest. An asterisk beside the directory’s version in `VERSION` marks
its files as untrusted. For example, an upload that leaves older, extra
files in place can leave the directory in this state. When a comparison
needs file hashes and the destination has no trusted manifest for them,
projr can hash the files themselves.

## Debugging

Enable debug output to see workflow details:

``` r

# Set debug output level
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")

# Run production build
projr_build_patch()

# View detailed log
projr_log_view()
```

Debug output shows: - Remote configuration (id, structure, strategy,
inspect) - Remote existence checks - Upload plan (files to add/remove) -
File operation details - Metadata updates

## See Also

- [`vignette("send-to-remotes")`](https://satvilab.github.io/projr/articles/send-to-remotes.md) -
  User guide for configuring remote destinations
- [`vignette("build-process")`](https://satvilab.github.io/projr/articles/build-process.md) -
  Complete build process overview
- [`?projr_yml_dest_add_github`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_github.md) -
  GitHub remote configuration
- [`?projr_yml_dest_add_local`](https://satvilab.github.io/projr/reference/projr_yml_dest_add.md) -
  Local remote configuration
