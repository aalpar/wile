# Scripts Directory

This directory contains automation scripts for managing Wile's GitHub issues, project board, and contributor infrastructure.

## Available Scripts

### `create-github-issues.sh`
Creates 30 GitHub issues from TODO.md items with labels and detailed descriptions.

**Usage:**
```bash
./scripts/create-github-issues.sh
```

**Creates:**
- 15 labels (architecture, performance, testing, etc.)
- 30 issues with full descriptions and phase checklists
- 8 labeled "good-first-issue"
- 8 labeled "help-wanted"

### `create-plan-issues.sh` ⭐
Creates GitHub issues from plan files in `plans/` directory. Automatically extracts titles, phases, and status from each plan file.

**Usage:**
```bash
# Preview what would be created (recommended first)
./scripts/create-plan-issues.sh --dry-run

# Create the issues
./scripts/create-plan-issues.sh
```

**What it does:**
- Reads all `.md` files in `plans/`
- Extracts title (from `# Plan: ...`)
- Extracts status (from `**Status**: ...`)
- Extracts phases (from `### Phase N:` headings)
- Determines appropriate labels based on filename/content
- Creates GitHub issue with link back to plan document
- Skips files marked as "Complete"

**Labels assigned:**
- `PERFORMANCE_REFACTORING_PLAN.md` → `planned,performance`
- `*COVERAGE*.md` → `planned,testing`
- `AUTHORIZATION_FRAMEWORK.md` → `planned,security`
- `*DEBUGGING*.md`, `*TRACING*.md` → `planned,tooling`
- `*REFACTORING*.md`, `*CONSOLIDATION*.md` → `planned,refactoring`
- `SCHEME_EXAMPLES.md` → `planned,documentation`
- Others → `planned,enhancement`

### `create-project-board.sh`
Creates a GitHub Project board via CLI. Prints manual steps for web UI configuration.

**Usage:**
```bash
./scripts/create-project-board.sh
```

**Recommendation:** Use the web UI instead. Follow `docs/PROJECT_BOARD_SETUP.md` for detailed instructions (updated for 2026 GitHub Projects v2 UI).

---

## Workflow

### Initial Setup (Run Once)

1. **Create TODO-based issues:**
   ```bash
   ./scripts/create-github-issues.sh
   ```

2. **Create plan-based issues:**
   ```bash
   ./scripts/create-plan-issues.sh --dry-run  # Preview
   ./scripts/create-plan-issues.sh            # Create
   ```

3. **Set up project board:**
   - Follow `docs/PROJECT_BOARD_SETUP.md` (web UI recommended)
   - Or run `./scripts/create-project-board.sh` and complete manual steps

### Ongoing Maintenance

**When adding a new plan file:**
1. Create `plans/NEW_PLAN.md` following existing format:
   - Include `# Plan: Title`
   - Include `**Status**: Planned/In Progress`
   - Include `### Phase N:` headings for tasks
2. Run `./scripts/create-plan-issues.sh` to generate the GitHub issue
3. Add to project board

**When updating an existing plan:**
1. Edit `plans/PLAN_NAME.md`
2. Manually update the corresponding GitHub issue
3. Or close old issue and recreate with `create-plan-issues.sh`

---

## File Structure

```
scripts/
├── README.md                      # This file
├── create-github-issues.sh        # TODO.md → GitHub issues
├── create-plan-issues.sh          # plans/*.md → GitHub issues
└── create-project-board.sh        # Project board creation (CLI)

docs/
└── PROJECT_BOARD_SETUP.md         # Project board setup (web UI, recommended)

private/
├── SETUP_SUMMARY.md               # Full deployment guide
└── ANNOUNCEMENT_PLATFORM_STRATEGY.md  # Posting strategy
```

---

## Templates

- `PLAN_ISSUES_TEMPLATE.md` — Structure reference for plan-based issues
- `PLAN_TODO_TEMPLATE.md` — Structure for TODO.md entries

These are reference templates showing the expected format.
