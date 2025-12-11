# Copilot Instructions Directory

This directory contains topic-specific instruction files for GitHub Copilot code review and coding agent.

## Structure

- **Main file**: `../.github/copilot-instructions.md` - High-level, repo-wide guidelines
- **Topic files**: Individual files for specific concerns with targeted `applyTo` rules

## Topic Files

| File | Lines | Applies To | Purpose |
|------|-------|------------|---------|
| `r-coding-standards.instructions.md` | 133 | `**/*.R` | R code style, naming, documentation |
| `remote-system.instructions.md` | 216 | `R/remote*.R` | Remote destinations, file operations |
| `git-version-control.instructions.md` | 225 | `R/{git,yml-git}*.R` | Git operations, versioning |
| `yaml-configuration.instructions.md` | 247 | `**/*.{yml,yaml}` | YAML config, scripts, hooks |
| `build-system.instructions.md` | 252 | `R/{build,manifest,hash,change,log}*.R` | Build process, manifest, logging |
| `testing.instructions.md` | 253 | `tests/**/*` | Test patterns, modes, helpers |
| `package-development.instructions.md` | 263 | `**/*` (excludes code review) | Dev workflow, commands |
| `authentication.instructions.md` | 267 | `R/auth*.R` | GitHub/OSF auth checks |

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

When updating these files, **follow GitHub's best practices** for Copilot instructions:

### General Guidelines

1. **Keep it concise** - Files under 250 lines ideal, never exceed 1000 lines
2. **Structure matters** - Use clear headings, bullet points, and sections
3. **Be direct** - Short, imperative rules are more effective than long paragraphs
4. **Show examples** - Demonstrate concepts with sample code (correct and incorrect patterns)

### Specific Requirements

1. **Keep the same structure and format** - All files follow the template below
2. **Ensure `applyTo` patterns are correct** - Target only relevant files
3. **Add examples for new concepts** - Include code blocks showing usage
4. **Keep files focused on their specific topic** - Don't mix concerns
5. **Update the main `copilot-instructions.md`** if adding new topic files

### What NOT to Do

❌ **Don't include external links** - Copilot won't follow them; copy relevant info instead
❌ **Don't use vague language** - Avoid "be more accurate", "identify all issues", etc.
❌ **Don't try to change Copilot's UX** - Can't change comment formatting, fonts, etc.
❌ **Don't request product behavior changes** - Can't block PRs, modify overview comments, etc.
❌ **Don't create long dense paragraphs** - Use bullets and short sections instead

## References

- [GitHub Blog: Mastering Copilot Instructions](https://github.blog/ai-and-ml/unlocking-the-full-power-of-copilot-code-review-master-your-instructions-files/)
- Main instructions file: `../.github/copilot-instructions.md`
