#!/usr/bin/env bash
# Detect orphaned documentation: tracked Markdown files under a docs tree that
# no navigation hub links to.
#
# check-readme-links.sh verifies the FORWARD direction — that every link in a
# hub resolves to a file. This verifies the REVERSE direction — that every file
# is reachable from a hub — which a link-checker structurally cannot do (it only
# sees links that are present). TOC.md claims to list "every document in docs/";
# this check keeps that claim honest.
#
# Only the curated hub files are parsed for links, never the content docs, so
# this is immune to the false positives a naive [..](..) regex hits on code
# spans like `arr[i](x, y)`.
#
# Usage (run from the repo root):
#   ./tools/sh/check-docs-orphans.sh [docs-dir]   # docs-dir defaults to "docs"

set -euo pipefail

DOCS_DIR="${1:-docs}"

if [ ! -d "$DOCS_DIR" ]; then
    echo "Error: docs directory not found: $DOCS_DIR"
    exit 1
fi

# Navigation hubs expected to reference every document. A hub is not expected to
# list itself.
hubs=("$DOCS_DIR/INDEX.md" "$DOCS_DIR/TOC.md")

linked="$(mktemp)"
trap 'rm -f "$linked"' EXIT

# Collect the documents the hubs link to, as repo-relative paths. Markdown links
# resolve against the linking file's own directory; anchors and URLs are skipped.
for hub in "${hubs[@]}"; do
    [ -f "$hub" ] || continue
    hub_dir="$(dirname "$hub")"
    while IFS= read -r target; do
        case "$target" in
            ""|http://*|https://*|mailto:*) continue ;;
        esac
        printf '%s/%s\n' "$hub_dir" "${target%/}"
    done < <(perl -ne 'while (/\[[^\]]*\]\(([^)#]+)/g) { print "$1\n" }' "$hub")
done | sed 's#/\./#/#g' | sort -u > "$linked"

# Every tracked .md under the docs tree, minus the hubs themselves and
# local-only (*.local.md) files.
orphans=()
while IFS= read -r doc; do
    case "$doc" in
        "$DOCS_DIR/INDEX.md"|"$DOCS_DIR/TOC.md"|*.local.md) continue ;;
    esac
    if ! grep -qxF "$doc" "$linked"; then
        orphans+=("$doc")
    fi
done < <(git ls-files -- "$DOCS_DIR" | grep -E '\.md$' | sort -u)

echo "Documents linked from hubs: $(wc -l < "$linked" | tr -d ' '); orphans: ${#orphans[@]}"

if [ "${#orphans[@]}" -gt 0 ]; then
    echo ""
    echo "Orphaned docs (tracked under $DOCS_DIR/ but linked from neither INDEX.md nor TOC.md):"
    for o in "${orphans[@]}"; do
        echo "  ORPHAN  $o"
    done
    echo ""
    echo "Add each to $DOCS_DIR/TOC.md (and the relevant $DOCS_DIR/INDEX.md row), or remove the file."
    exit 1
fi
