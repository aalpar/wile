#!/bin/bash
# Create GitHub issues from plan files in plans/
# Uses PLAN_ISSUES_TEMPLATE.md as structure reference
#
# Usage:
#   ./scripts/create-plan-issues.sh          # Create issues
#   ./scripts/create-plan-issues.sh --dry-run  # Preview without creating

set -e

# Change to repository root (parent of scripts directory where this script lives)
cd "$(dirname "$0")/.."

# Parse arguments
DRY_RUN=false
if [ "$1" = "--dry-run" ]; then
    DRY_RUN=true
fi

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

if [ "$DRY_RUN" = true ]; then
    echo -e "${BLUE}DRY RUN: Previewing issues that would be created...${NC}"
else
    echo "Creating GitHub issues from plan files..."
fi
echo ""

# Function to extract plan title
get_title() {
    local file="$1"
    # Try "# Plan: Title" format first
    local title=$(grep "^# Plan:" "$file" | head -1 | sed 's/^# Plan: //')

    # If not found, try just "# Title" (first h1 heading)
    if [ -z "$title" ]; then
        title=$(grep "^# " "$file" | head -1 | sed 's/^# //')
    fi

    echo "$title"
}

# Function to extract status
get_status() {
    local file="$1"
    grep "^\*\*Status\*\*:" "$file" | head -1 | sed 's/\*\*Status\*\*: //' || echo "Not started"
}

# Function to extract phases/tasks
get_phases() {
    local file="$1"
    local phases=""

    # Try format 1: "### Phase N: description"
    phases=$(grep "^### Phase [0-9]" "$file" | sed 's/^### /- [ ] /')

    # Try format 2: "## I. Title", "## II. Title" (Roman numerals)
    if [ -z "$phases" ]; then
        phases=$(grep "^## [IVX]\+\." "$file" | sed 's/^## /- [ ] /')
    fi

    # Try format 3: "## Phase N" (without colon)
    if [ -z "$phases" ]; then
        phases=$(grep "^## Phase [0-9]" "$file" | sed 's/^## /- [ ] /')
    fi

    # Try format 4: Any numbered ## headings
    if [ -z "$phases" ]; then
        phases=$(grep "^## [0-9]\+\." "$file" | sed 's/^## /- [ ] /')
    fi

    # If still empty, extract all ## level headings except specific ones
    if [ -z "$phases" ]; then
        phases=$(grep "^## " "$file" | grep -v "^## Summary\|^## Design\|^## Motivation\|^## Context\|^## References\|^## Completed" | sed 's/^## /- [ ] /')
    fi

    echo "$phases"
}

# Function to determine labels based on content and filename
get_labels() {
    local file="$1"
    local labels="planned"

    # Add category-based labels
    case "$(basename "$file")" in
        PERFORMANCE_REFACTORING_PLAN.md)
            labels="$labels,performance"
            ;;
        *COVERAGE*.md)
            labels="$labels,testing"
            ;;
        AUTHORIZATION_FRAMEWORK.md)
            labels="$labels,security"
            ;;
        *DEBUGGING*.md|*TRACING*.md)
            labels="$labels,tooling"
            ;;
        EXTERNAL_EXTENSIONS_PLAN.md|PLUGIN_ARCHITECTURE_PROPOSAL.md)
            labels="$labels,architecture"
            ;;
        *REFACTORING*.md|*CONSOLIDATION*.md|*SIMPLIFICATION*.md|ALGEBRAIC_REDUCTIONS.md)
            labels="$labels,refactoring"
            ;;
        SCHEME_EXAMPLES.md)
            labels="$labels,documentation"
            ;;
        TOKENIZER_CONSOLIDATION_PLAN.md|EMPTY_LIST_VOID_REFACTORING.md)
            labels="$labels,refactoring"
            ;;
        *)
            labels="$labels,enhancement"
            ;;
    esac

    echo "$labels"
}

# Function to get summary from TODO.md
get_summary() {
    local plan_name="$1"
    # Try to extract from TODO.md table
    # This is a simplified extraction - you might need to enhance this
    grep -i "$(basename "$plan_name" .md)" TODO.md | \
        head -1 | \
        sed 's/^|[^|]*|[^|]*|[^|]*|//' | \
        sed 's/|$//' | \
        sed 's/See `plans.*$//' | \
        sed 's/^[[:space:]]*//' | \
        sed 's/[[:space:]]*$//' || \
        echo "See plans/$(basename "$plan_name") for details"
}

# Track created issues
CREATED=0
SKIPPED=0

# Process each plan file
for plan_file in plans/*.md; do
    if [ ! -f "$plan_file" ]; then
        continue
    fi

    filename=$(basename "$plan_file")
    title=$(get_title "$plan_file")
    status=$(get_status "$plan_file")
    labels=$(get_labels "$plan_file")
    summary=$(get_summary "$plan_file")
    phases=$(get_phases "$plan_file")

    # Skip if title is empty or if marked as complete/deleted
    if [ -z "$title" ]; then
        echo -e "${YELLOW}⊘ Skipping $filename (no title found)${NC}"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    # Skip if status is "Complete"
    if echo "$status" | grep -qi "complete"; then
        echo -e "${YELLOW}⊘ Skipping $filename (marked complete)${NC}"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    echo -e "${GREEN}Creating: $title${NC}"

    # Build issue body
    body=$(cat <<EOF
## Summary
$summary

## Design
See [\`plans/$filename\`](../plans/$filename) for full design and implementation details.

## Tasks
$phases

## Status
**Current status:** $status

## References
- [Plan Document](../plans/$filename)
EOF
)

    # Create the issue (or preview in dry-run mode)
    if [ "$DRY_RUN" = true ]; then
        echo -e "${BLUE}Would create issue:${NC}"
        echo "  Title: $title"
        echo "  Labels: $labels"
        echo "  Body preview:"
        echo "$body" | sed 's/^/    /'
        echo ""
        CREATED=$((CREATED + 1))
    else
        if gh issue create \
            --title "$title" \
            --label "$labels" \
            --body "$body"; then
            CREATED=$((CREATED + 1))
            echo ""
        else
            echo -e "${RED}✗ Failed to create issue for $filename${NC}"
            echo ""
        fi
    fi
done

echo ""
if [ "$DRY_RUN" = true ]; then
    echo -e "${BLUE}✓ Would create $CREATED issues from plan files${NC}"
    if [ $SKIPPED -gt 0 ]; then
        echo -e "${YELLOW}⊘ Would skip $SKIPPED files${NC}"
    fi
    echo ""
    echo "To create these issues for real, run:"
    echo "  ./scripts/create-plan-issues.sh"
else
    echo -e "${GREEN}✅ Created $CREATED issues from plan files${NC}"
    if [ $SKIPPED -gt 0 ]; then
        echo -e "${YELLOW}⊘ Skipped $SKIPPED files${NC}"
    fi
    echo ""
    echo "Next steps:"
    echo "1. Review issues at: https://github.com/aalpar/wile/issues"
    echo "2. Add to project board"
    echo "3. Triage and assign priorities"
fi
echo ""
