# GitHub Project configuration

| Key | Value |
| --- | --- |
| Contract version | 1 |
| Sub-project storage | field |
| Sub-project field | Sub-project |
| Sub-project field type | SINGLE_SELECT |
| Mode | single |
| Project key | proj |
| Issue repository | MiguelRodo/issues |
| Project owner | MiguelRodo |
| Owner type | user |
| Project number | 45 |
| Project title | proj |
| Routing | membership |
| Privacy | private central issue store; public implementation repository |
| Implementation repository | SATVILab/projr |
| Queue source | mirror |

## Field locations

| Common dimension | Provider location | Provider field |
| --- | --- | --- |
| Class | project field | Class |
| Priority | project field | Priority |
| Status | project field | Status |
| Due date | project field | Due date |
| Sub-project | project field | Sub-project |

## Sub-project vocabulary

| Key | Provider value | Implementation repository |
| --- | --- | --- |
| projr | projr | SATVILab/projr |
| cli | cli | MiguelRodo/projcli |

## Governance

- Collaboration mode: collaborative administration in a shared project.
- Planning issues belong in the private central issue repository. Do not copy private planning content into public implementation repositories.

## Storage authority

- The field includes a schema-only `Not assigned` option. Leave unclassified item values blank; clearing does not select that option.
- `Sub-project` is the sole writable grouping representation. Legacy grouping labels are read-only migration artifacts until verified cleanup.
- Resolve field and option IDs freshly; require the exact configured name, SINGLE_SELECT type and vocabulary mapping. Stop if the expected field is missing or inaccessible.
- Multiple memberships require explicit Project context unless exactly one relevant managed membership resolves it. An explicit add-membership request may name its target before placement.
