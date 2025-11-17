# Copilot Instructions Directory

This directory contains topic-specific instruction files for GitHub Copilot code review and coding agent.

## Structure

- **Main file**: `../.github/copilot-instructions.md` - High-level, repo-wide guidelines
- **Topic files**: Individual files for specific concerns with targeted `applyTo` rules

## Topic Files

| File | Lines | Applies To | Purpose |
|------|-------|------------|---------|
| `r-coding-standards.instructions.md` | 133 | `**/*.R` | R code style, naming, documentation |
| `testing.instructions.md` | 162 | `tests/**/*` | Test patterns, modes, helpers |
| `yaml-configuration.instructions.md` | 187 | `**/*.{yml,yaml}` | YAML config, scripts, hooks |
| `package-development.instructions.md` | 192 | `**/*` (excludes code review) | Dev workflow, commands |
| `authentication.instructions.md` | 235 | `R/auth*.R` | GitHub/OSF auth checks |
| `git-version-control.instructions.md` | 225 | `R/{git,yml-git}*.R` | Git operations, versioning |
| `build-system.instructions.md` | 248 | `R/{build,manifest,hash,change,log}*.R` | Build process, manifest, logging |

## File Format

Each file follows this structure:

```markdown
---
applyTo: "file/pattern/**/*"
# or
excludeAgent: copilot_code_review
---

# Title

## Purpose & Scope
Brief description of what this file covers.

---

## Section 1
Guidelines...

## Section 2
More guidelines...

---

## Code Examples
```language
// Examples...
```
```

## Best Practices

✅ **Keep files concise** - Each file under 250 lines (recommended: under 1000)
✅ **Clear structure** - Use headings, bullets, and sections
✅ **Show examples** - Include correct and incorrect patterns
✅ **Be direct** - Short, imperative rules
✅ **Path-specific** - Use `applyTo` frontmatter for targeted application
✅ **No external links** - External links don't work in Copilot
✅ **No vague language** - Avoid "be more accurate", "identify all issues", etc.

## Maintenance

When updating these files:

1. Keep the same structure and format
2. Ensure `applyTo` patterns are correct
3. Add examples for new concepts
4. Keep files focused on their specific topic
5. Update the main `copilot-instructions.md` if adding new topic files

## References

- [GitHub Blog: Mastering Copilot Instructions](https://github.blog/ai-and-ml/unlocking-the-full-power-of-copilot-code-review-master-your-instructions-files/)
- Main instructions file: `../.github/copilot-instructions.md`
