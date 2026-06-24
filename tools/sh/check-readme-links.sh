#!/usr/bin/env bash
# Validate local file links in a markdown file.
# Only checks local file references — skips HTTP(S) and anchor links.
#
# Usage:
#   ./tools/sh/check-readme-links.sh README.md

set -euo pipefail

FILE="${1:?Usage: check-readme-links.sh <markdown-file>}"

if [ ! -f "$FILE" ]; then
    echo "Error: File not found: $FILE"
    exit 1
fi

# Markdown relative links resolve against the directory containing the file,
# not the repo root. For README.md (at the repo root) these coincide; for a
# file like docs/INDEX.md, a link "reference/scheme.md" means
# docs/reference/scheme.md.
BASE_DIR="$(cd "$(dirname "$FILE")" && pwd)"
passed=0
failed=0
failures=()

# Extract markdown links: [text](target)
# Skip HTTP(S) links and anchor-only links (#section)
while IFS= read -r link; do
    # Skip empty, HTTP(S), anchor-only, and mailto links
    if [[ -z "$link" || "$link" =~ ^https?:// || "$link" =~ ^# || "$link" =~ ^mailto: ]]; then
        continue
    fi

    # Strip anchor from local links (e.g., docs/foo.md#section → docs/foo.md)
    target="${link%%#*}"
    if [ -z "$target" ]; then
        continue
    fi

    # Resolve relative to the markdown file's own directory
    if [ -e "$BASE_DIR/$target" ]; then
        passed=$((passed + 1))
    else
        failed=$((failed + 1))
        failures+=("$target")
    fi
done < <(perl -ne 'while (/\[[^\]]*\]\(([^)]+)\)/g) { print "$1\n" }' "$FILE" | sort -u)

total=$((passed + failed))
echo "Links checked: $passed/$total valid"

if [ "$failed" -gt 0 ]; then
    echo ""
    echo "Broken links:"
    for f in "${failures[@]}"; do
        echo "  MISSING  $f"
    done
    exit 1
fi
