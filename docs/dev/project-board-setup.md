# Project Board Setup Guide

This guide shows how to set up a GitHub Project board for tracking Wile development.

**Updated for GitHub Projects v2 (2026 UI)**

## Quick Start (Web UI)

### 1. Create the Project

1. Go to: https://github.com/users/aalpar/projects
2. Click **"New project"**
3. Select **"Board"** template
4. Name it: **"Wile Development"**
5. Click **"Create project"**

### 2. Configure Status Field Values

GitHub Projects uses a **Status** field to create board columns. Edit the Status values:

1. Click **"⋮"** (three dots, top-right) → **"Settings"**
2. Under **"Fields"**, click **"Status"**
3. Rename/add values to match your workflow:

| Status Value | Purpose |
|--------------|---------|
| **Backlog** | All tracked work (rename "Todo" to this) |
| **Ready** | Ready to start, needs owner (add new) |
| **In Progress** | Actively being worked on (keep default) |
| **Review** | PR open, awaiting review (add new) |
| **Done** | Completed work (keep default) |

**To add a status value:** Click "Add option" → Name it → Choose a color → Save

**To rename:** Click the value → "Edit" → Change name → Save

The board view will automatically show columns for each Status value.

### 3. Set Up Workflows (Automations)

Click **"⋮"** (three dots) → **"Workflows"**

You'll see:
```
Default workflows
├─ Item closed
├─ Item reopened
├─ Pull request merged
└─ Auto-add to project
```

#### Configure "Auto-add to project"

This automatically adds new issues/PRs to your project.

1. Click **"Auto-add to project"**
2. Click **"Edit"** (top-right)
3. Under **"Filters"**, select repository: `aalpar/wile`
4. Add filter in text box:
   ```
   is:issue is:open
   ```
   (This adds all new open issues)
5. Click **"Save and turn on workflow"**

**Note:** This only affects NEW issues. Existing issues must be added manually (see Step 4).

#### Configure "Item closed" Workflow

1. Click **"Item closed"**
2. Click **"Edit"**
3. Set "Set status to" → **Done**
4. Click **"Save and turn on workflow"**

#### Configure "Pull request merged" Workflow

1. Click **"Pull request merged"**
2. Click **"Edit"**
3. Set "Set status to" → **Done**
4. Click **"Save and turn on workflow"**

#### Configure "Item reopened" Workflow

1. Click **"Item reopened"**
2. Click **"Edit"**
3. Set "Set status to" → **Backlog**
4. Click **"Save and turn on workflow"**

### 4. Add Existing Issues

The auto-add workflow doesn't apply retroactively. Add existing issues manually:

1. In your project, click **"+ Add item"** (bottom of board)
2. Type `#` to see all issues, or search:
   ```
   repo:aalpar/wile is:issue is:open
   ```
3. Select multiple issues (Shift+Click to bulk-select)
4. Press **Enter** to add them

They'll appear in whatever status they're set to (default: no status).

### 5. Bulk-Set Status for New Items

After adding existing issues:

1. Select all issues with no status
2. Click **"Status"** field
3. Choose **"Backlog"**

Or manually drag items into appropriate columns.

### 6. Make Project Public

1. Click **"⋮"** → **"Settings"**
2. Scroll to **"Danger zone"**
3. Click **"Change project visibility"**
4. Select **"Public"**
5. Confirm

### 7. Pin to Profile (Optional)

1. Go to your profile: https://github.com/aalpar
2. Click **"Customize your pins"**
3. Select **"Wile Development"** project
4. Save

---

## Advanced: Additional Workflows

### Auto-Archive Closed Items

Keep your board clean by auto-archiving old completed items:

1. Go to **Workflows** → **"Auto-archive items"**
2. Click **"Edit"**
3. Set filter:
   ```
   is:closed
   ```
4. Set "Archive after" → **30 days**
5. Click **"Save and turn on workflow"**

### Create Duplicate Workflow for Labeled Items

You can create additional auto-add workflows for specific labels:

1. In **Workflows**, click **"Auto-add to project"**
2. Click **"⋮"** → **"Duplicate workflow"**
3. Click **"Edit"** on the new workflow
4. Set filter:
   ```
   is:issue is:open label:good-first-issue
   ```
5. Under "Set status to", choose **Ready**
6. Save and enable

**Limitation:** the cap is on *auto-add* workflows specifically. Free allows 1, Pro/Team 5, Enterprise Cloud/Server 20.

---

## Available Filters

Use these in auto-add workflows:

| Filter | Example | Description |
|--------|---------|-------------|
| `is:open` | `is:issue is:open` | Open issues |
| `is:closed` | `is:issue is:closed` | Closed issues |
| `is:merged` | `is:pr is:merged` | Merged PRs |
| `is:draft` | `is:pr is:draft` | Draft PRs |
| `label:X` | `label:good-first-issue` | Has label X |
| `-label:X` | `-label:wontfix` | Does NOT have label X |
| `assignee:X` | `assignee:aalpar` | Assigned to user X |
| `no:assignee` | `no:assignee` | No assignee |

**Combine filters:**
```
is:issue is:open label:help-wanted -label:blocked
```
(Open issues with help-wanted label, excluding blocked ones)

---

## Custom Fields (Advanced)

Add custom fields to track metadata:

### Priority Field

1. **Settings** → **Custom fields** → **"+ New field"**
2. Name: **Priority**
3. Type: **Single select**
4. Options: `P0-Critical`, `P1-High`, `P2-Medium`, `P3-Low`
5. Save

### Size/Effort Field

1. **New field** → Name: **Size**
2. Type: **Single select**
3. Options: `XS`, `S`, `M`, `L`, `XL`
4. Save

### Phase Field (for multi-phase issues)

1. **New field** → Name: **Phase**
2. Type: **Text**
3. Use for: "Phase 2/6" tracking

---

## Board Views

Create filtered views for different perspectives:

### Good First Issues View

1. Click current view name (top-left, probably "Board")
2. Click **"+ New view"**
3. Name: **Good First Issues**
4. Layout: **Board**
5. Under "Filter", add:
   ```
   label:good-first-issue
   ```
6. Save

### Help Wanted View

1. **New view** → Name: **Help Wanted**
2. Filter: `label:help-wanted`
3. Group by: **Status** (default)
4. Sort by: **Priority** (if you added Priority field)

### My Issues View

1. **New view** → Name: **Assigned to Me**
2. Filter: `assignee:@me`
3. Layout: **Table** (better for personal work overview)

---

## Troubleshooting

**Issues not auto-adding?**
- Workflows only affect NEW or UPDATED items
- Add existing issues manually (Step 4)
- Check filter syntax matches available qualifiers

**Can't see Workflows menu?**
- You must be the project owner
- Organization projects may require admin permissions

**Columns not showing up?**
- Board columns come from Status field values
- Add/rename values in Settings → Fields → Status

---

## Sources

- [GitHub Docs: Using Built-in Automations](https://docs.github.com/en/issues/planning-and-tracking-with-projects/automating-your-project/using-the-built-in-automations)
- [GitHub Docs: Adding Items Automatically](https://docs.github.com/en/issues/planning-and-tracking-with-projects/automating-your-project/adding-items-automatically)
- [GitHub Docs: Automating Projects](https://docs.github.com/en/issues/planning-and-tracking-with-projects/automating-your-project)
