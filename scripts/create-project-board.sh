#!/bin/bash
# Create GitHub Project board for Wile
# Requires: gh CLI v2.20.0+ with project support
set -e

echo "Creating Wile Development project board..."

# Check gh version
if ! gh version | grep -q "gh version 2"; then
    echo "Error: gh CLI v2.20.0+ required"
    echo "Update with: brew upgrade gh"
    exit 1
fi

# Create project (returns project URL)
PROJECT_URL=$(gh project create \
    --owner aalpar \
    --title "Wile Development" \
    --format json | jq -r '.url' 2>/dev/null || true)

if [ -z "$PROJECT_URL" ]; then
    echo "Error: Failed to create project"
    echo ""
    echo "Alternative: Create manually via web UI"
    echo "1. Go to: https://github.com/users/aalpar/projects"
    echo "2. Click 'New project'"
    echo "3. Select 'Board' template"
    echo "4. Name it 'Wile Development'"
    echo "5. Follow the setup steps below"
    exit 1
fi

echo "✅ Project created: $PROJECT_URL"
echo ""

# Extract project number from URL
PROJECT_NUMBER=$(echo "$PROJECT_URL" | grep -oE '[0-9]+$')

if [ -z "$PROJECT_NUMBER" ]; then
    echo "Warning: Could not extract project number"
    echo "Complete setup manually at: $PROJECT_URL"
    exit 0
fi

echo "Configuring project board (project #$PROJECT_NUMBER)..."

# Note: The gh project field/item commands are still evolving
# This script uses basic commands available in gh 2.20+
# For advanced customization, use the web UI

echo ""
echo "✅ Project board created!"
echo ""
echo "Next steps (complete in web UI):"
echo "1. Visit: $PROJECT_URL"
echo "2. Add these columns (rename default columns):"
echo "   - Backlog (default: Todo)"
echo "   - Ready (add new)"
echo "   - In Progress (default: In Progress)"
echo "   - Review (add new)"
echo "   - Done (default: Done)"
echo ""
echo "3. Add workflow automations:"
echo "   - Auto-add: When issues labeled 'good-first-issue' → Backlog"
echo "   - Auto-add: When issues labeled 'help-wanted' → Ready"
echo "   - Auto-move: When PR opened → Review"
echo "   - Auto-move: When PR merged → Done"
echo "   - Auto-move: When issue closed → Done"
echo ""
echo "4. Add existing issues to board:"
echo "   - Use 'Add items' button"
echo "   - Filter by label: good-first-issue, help-wanted"
echo "   - Bulk-add to Backlog"
echo ""
echo "5. Pin the project to your profile:"
echo "   - Project settings → Make project public"
echo "   - Pin to profile for visibility"
echo ""
