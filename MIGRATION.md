# Cross-Platform Agent Instruction Management

How Claude Code, OpenAI Codex CLI, and GitHub Copilot instruction files relate, and how to keep them in sync.

## Platform File Map

| Purpose | Claude Code | Codex CLI | GitHub Copilot |
|---------|-------------|-----------|----------------|
| **Main instructions** | `.claude/CLAUDE.md` | `AGENTS.md` (repo root) | `.github/copilot-instructions.md` |
| **Rule/instruction dir** | `.claude/rules/*.md` | `.codex/rules/*.md` | `.github/instructions/*.md` |
| **Skills** | `.claude/skills/*.md` | `.agents/skills/*/SKILL.md` | n/a |
| **MCP config** | `.mcp.json` (JSON) | `.codex/config.toml` (TOML) | `.vscode/mcp.json` (JSON) |
| **Execution policy** | n/a (instruction-based) | `.codex/rules/default.rules` (Starlark) | n/a |
| **Hooks** | `.claude/hooks/*.js` | n/a (`notify` only) | n/a |
| **Project config** | `.claude/settings.local.json` | `.codex/config.toml` | `.vscode/settings.json` |

## Directory Layout

```
repo/
├── AGENTS.md                           # Codex main instructions
├── codex.toml                          # Root-level Codex fallback (MCP + reasoning)
├── .mcp.json                           # Claude Code MCP (not committed)
│
├── .claude/
│   ├── CLAUDE.md                       # Claude main instructions
│   ├── mcp-server.R                    # Shared MCP server script (all platforms)
│   ├── session_startup.R               # Shared R bootstrap
│   ├── settings.local.json             # Claude local config (not committed)
│   ├── hooks/
│   │   └── block-test-edits.js         # Claude-only PreToolUse hook
│   ├── rules/                          # 12 rule files (paths: frontmatter)
│   │   ├── r-instructions.md
│   │   ├── qml-instructions.md
│   │   └── ...
│   └── skills/
│       └── fix-debug-analysis.md       # Claude skill (flat .md)
│
├── .codex/
│   ├── config.toml                     # Codex project config + MCP
│   └── rules/                          # 12 rule files (no frontmatter) + Starlark
│       ├── default.rules               # Execution policy (Starlark)
│       ├── r-instructions.md
│       ├── qml-instructions.md
│       └── ...
│
├── .agents/
│   └── skills/
│       └── fix-debug-analysis/
│           └── SKILL.md                # Codex skill (YAML frontmatter)
│
├── .github/
│   ├── copilot-instructions.md         # Copilot main instructions
│   └── instructions/                   # 12 instruction files (applyTo: frontmatter)
│       ├── R.instructions.md
│       ├── inst.qml.instructions.md
│       └── ...
│
└── .vscode/
    └── mcp.json                        # Copilot MCP config (not committed)
```

## How Each Platform Discovers Instructions

### Claude Code
1. Reads `.claude/CLAUDE.md` automatically
2. Reads `.claude/rules/*.md` — files with `paths:` YAML frontmatter are loaded only when the agent touches matching files
3. Skills in `.claude/skills/*.md` — flat markdown, no frontmatter required

### Codex CLI
1. Walks from git root to CWD, collecting `AGENTS.md` (or `AGENTS.override.md`) per directory
2. References `.codex/rules/*.md` via links in `AGENTS.md` — **no frontmatter**, no automatic path-scoping; the model must read them on demand
3. Skills in `.agents/skills/*/SKILL.md` — requires `name:` and `description:` YAML frontmatter
4. Execution policy in `.codex/rules/default.rules` — Starlark, controls shell command permissions

### GitHub Copilot
1. Reads `.github/copilot-instructions.md` automatically
2. Reads `.github/instructions/*.md` — files with `applyTo:` YAML frontmatter are loaded when matching files are in context
3. No skill or execution policy support

## What's Shared vs Platform-Specific

### Shared (identical content)
- **Rule body text** — the markdown after frontmatter in each rule file is identical across all three platforms
- **MCP server script** — `.claude/mcp-server.R` is used by all platforms
- **R session startup** — `.claude/session_startup.R`
- **Skill body text** — fix-debug-analysis content is identical

### Platform-specific wrappers

**Frontmatter** differs per platform for the same rule content:

```markdown
# Claude Code (.claude/rules/r-instructions.md)
---
paths:
  - "**/R/*.R"
---

# GitHub Copilot (.github/instructions/R.instructions.md)
---
applyTo: "**/R/*.R"
description: "R function structure, validation, jaspResults API"
---

# Codex CLI (.codex/rules/r-instructions.md)
(no frontmatter — referenced by link from AGENTS.md)
```

**Main instruction files** (CLAUDE.md, AGENTS.md, copilot-instructions.md) are structurally identical but differ in:
- Rule directory references (`.claude/rules/` vs `.codex/rules/` vs `.github/instructions/`)
- MCP config notes (`.mcp.json` vs `config.toml` vs `.vscode/mcp.json`)

**MCP config format** is semantically identical but syntactically different:

```json
// .mcp.json (Claude Code)
{ "mcpServers": { "r-mcptools": { "type": "stdio", "command": "Rscript", "args": [...] } } }
```
```json
// .vscode/mcp.json (GitHub Copilot)
{ "servers": { "r-mcptools": { "command": "Rscript", "args": [...] } } }
```
```toml
# .codex/config.toml (Codex CLI)
[mcp_servers.r-mcptools]
command = "Rscript"
args = ["-e", "source('.claude/mcp-server.R')"]
```

**Note on root `codex.toml` vs `.codex/config.toml`:** Both exist intentionally. The root-level `codex.toml` is a fallback used by `container_entrypoint.sh` when Codex CLI is invoked from the repo root without a `.codex/` directory present. It contains only MCP config and reasoning settings. The `.codex/config.toml` is the full project config (sandbox mode, approval policy, project doc limit, MCP, reasoning) and takes precedence when the `.codex/` directory exists.

**Skills** have different formats for the same content:

```markdown
# Claude Code (.claude/skills/fix-debug-analysis.md)
(flat markdown, no frontmatter)

# Codex CLI (.agents/skills/fix-debug-analysis/SKILL.md)
---
name: fix-debug-analysis
description: >
  Guide for debugging JASP analysis functions...
---
(same markdown body)
```

## Editing Workflow

### When changing rule content (most common)

Rule content is the markdown body — identical across platforms.

1. **Edit the canonical copy** in `.claude/rules/` (arbitrarily chosen as source of truth)
2. **Sync to other platforms:**

| Source | Target | Transformation |
|--------|--------|---------------|
| `.claude/rules/foo.md` | `.codex/rules/foo.md` | Strip `paths:` frontmatter |
| `.claude/rules/foo.md` | `.github/instructions/foo.instructions.md` | Replace `paths:` with `applyTo:`, add `description:` |

The body after frontmatter should be byte-identical.

### When changing main instructions

Edit `.claude/CLAUDE.md`, then replicate structural changes to `AGENTS.md` and `.github/copilot-instructions.md`, updating directory references per the table above.

### When adding a new rule file

1. Create in `.claude/rules/new-topic.md` with `paths:` frontmatter
2. Copy to `.codex/rules/new-topic.md` — strip frontmatter
3. Copy to `.github/instructions/new-topic.instructions.md` — change to `applyTo:` frontmatter
4. Add link in `.claude/CLAUDE.md` → `.claude/rules/new-topic.md`
5. Add link in `AGENTS.md` → `.codex/rules/new-topic.md`
6. Add link in `.github/copilot-instructions.md` → `.github/instructions/new-topic.instructions.md`

### When adding a new skill

1. Create in `.claude/skills/my-skill.md` (flat markdown)
2. Create `.agents/skills/my-skill/SKILL.md` — add `name:` and `description:` frontmatter, same body
3. GitHub Copilot has no skill equivalent — embed critical parts in an `.instructions.md` file instead

### When changing MCP servers

Update all three config files:
- `.mcp.json` — JSON with `mcpServers` key and `type: "stdio"`
- `.codex/config.toml` — TOML `[mcp_servers.<name>]` section
- `.vscode/mcp.json` — JSON with `servers` key (no type field)

### When changing execution policy

Only Codex has execution policy (`.codex/rules/default.rules`). Claude Code uses hooks (`.claude/hooks/`) and instruction-based rules. Copilot has neither.

| Safety concern | Claude Code | Codex CLI | Copilot |
|---------------|-------------|-----------|---------|
| Block test edits | `.claude/hooks/block-test-edits.js` (PreToolUse) | Instruction in AGENTS.md | Instruction in copilot-instructions.md |
| Block force-push | Instruction in CLAUDE.md | `default.rules`: `decision = "forbidden"` | Instruction in copilot-instructions.md |
| Prompt before push | Instruction in CLAUDE.md | `default.rules`: `decision = "prompt"` | Instruction in copilot-instructions.md |

## Frontmatter Reference

### Claude Code (`paths:`)
```yaml
---
paths:
  - "**/R/*.R"
  - "**/tests/testthat/*.R"
---
```
- Accepts a list of glob patterns
- Rule is injected only when the agent touches matching files

### GitHub Copilot (`applyTo:`)
```yaml
---
applyTo: "**/R/*.R"
description: "Short description for UI display"
---
```
- Single glob string (not a list)
- `description:` is optional but recommended for Copilot's instruction picker UI

### Codex CLI (no frontmatter)
```markdown
# Rule Title
Rule content starts immediately...
```
- Rules live in `.codex/rules/` but have no built-in path scoping
- `AGENTS.md` links to rules with prose like: "Read when working on `R/*.R`"
- The model must decide when to read each file based on these hints

### Codex Skills (`SKILL.md`)
```yaml
---
name: skill-name
description: >
  When to invoke this skill. Be specific about
  trigger conditions.
---
```
- `name:` and `description:` are required
- Codex uses `description:` for implicit invocation matching

## Platform Capability Gaps

| Feature | Claude Code | Codex CLI | Copilot |
|---------|:-----------:|:---------:|:-------:|
| Auto path-scoped rules | **yes** | no | **yes** |
| Skills / custom commands | **yes** | **yes** | no |
| Pre/post tool hooks | **yes** | no | no |
| Execution policy (Starlark) | no | **yes** | no |
| MCP servers | **yes** | **yes** | **yes** |
| Nested directory instructions | partial | **yes** | partial |
| Multi-agent / sub-agents | **yes** | **yes** | no |
| Web search built-in | **yes** | **yes** | no |
| Override file (`*.override.md`) | no | **yes** | no |

## Quick Sync Checklist

When updating instructions, verify all three platforms stay aligned:

- [ ] Rule body text is identical across `.claude/rules/`, `.codex/rules/`, `.github/instructions/`
- [ ] Main instruction files reference the correct rule directory for their platform
- [ ] MCP server list matches across `.mcp.json`, `.codex/config.toml`, `.vscode/mcp.json`
- [ ] New rules are linked in all three main instruction files
- [ ] Skills exist in both `.claude/skills/` and `.agents/skills/`
- [ ] Execution policy in `.codex/rules/default.rules` reflects any new safety constraints
- [ ] Claude hooks in `.claude/hooks/` reflect any new safety constraints
