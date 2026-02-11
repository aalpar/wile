#!/bin/bash

set -e

echo "Creating: Feature Flags..."
gh issue create \
  --title "Feature Flags (3-tier)" \
  --label "runtime" \
  --body "$(cat <<EOF
## Summary
Three-tier feature flag system for controlling Wile behavior at different lifecycle stages.

## Tiers
1. **Compile-time** — set via Go build tags/ldflags (immutable after build)
2. **Runtime global** — configured during Wile initialization from Go (mutable)
3. **Extension-defined** — extensions register their own flags via registry

## Design Requirements
- [ ] Flag registry with typed values (bool, int, string)
- [ ] Compile-time flags via build tags and linker-injected constants
- [ ] Runtime flag registry queryable from both Go and Scheme
- [ ] Extension interface for registering custom flags
- [ ] Scheme primitives: \`(feature-flag? name)\`, \`(feature-flags)\`
- [ ] Thread-safe reads/writes for runtime flags
- [ ] Immutability enforcement for compile-time flags
- [ ] Integration with R7RS \`cond-expand\`

## Use Cases
- Minimal embedding (strip unused features at compile time)
- Runtime behavior configuration (tail-call optimization, recursion limits)
- Extension-specific settings (custom GC strategies, debug modes)
EOF
)"

echo "Creating: Event Callbacks..."
gh issue create \
  --title "Event Callbacks" \
  --label "tooling" \
  --body "$(cat <<EOF
## Summary
Callback hooks for compiler/expander/runtime events, enabling IDE integration, debugging, and profiling.

## Features
- [ ] Expansion events (before/after macro expansion)
- [ ] Compilation events (before/after compilation)
- [ ] Runtime debugging (variable set/get)

## Pattern
Similar to \`dynamic-wind\` but for compiler/expander phases.

## Use Cases
- IDE integration (syntax highlighting, code navigation)
- Debuggers (breakpoints, step execution)
- Profilers (macro expansion time, compilation bottlenecks)
EOF
)"

echo "Creating: Debugger / DAP Integration..."
gh issue create \
  --title "Debugger / DAP Integration" \
  --label "tooling" \
  --body "$(cat <<EOF
## Summary
Implement Debug Adapter Protocol (DAP) support for Wile, enabling standard IDE debugging.

## Features
- [ ] Breakpoints
- [ ] Step execution (step into, step over, step out)
- [ ] Variable inspection
- [ ] Call stack visualization
- [ ] REPL evaluation at breakpoint

## Reference
- [Debug Adapter Protocol Specification](https://microsoft.github.io/debug-adapter-protocol/)

## Context
Enables debugging Wile programs in VS Code, Emacs (dap-mode), and other DAP-compatible editors.
EOF
)"

echo "Creating: Tokenizer Consolidation..."
gh issue create \
  --title "Tokenizer Consolidation" \
  --label "refactoring,planned" \
  --body "$(cat <<EOF
## Summary
Consolidate ~700 lines of repetitive number parsing code in the tokenizer using quasi-sub-token helpers, reducing boilerplate by ~295 lines without changing public behavior.

## Design
See [\`plans/TOKENIZER_CONSOLIDATION_PLAN.md\`](./plans/TOKENIZER_CONSOLIDATION_PLAN.md) for full analysis.

## Opportunities Identified
- Optional sign handling (12 occurrences)
- Decimal fraction parsing (6 occurrences)
- Special numbers (inf.0/nan.0) handling
- Imaginary suffix handling (10 occurrences)
- Complex number suffix dispatch (4 occurrences)
- Error-check-return pattern (50+ occurrences)

## Phases
- [ ] Phase 1: Low-risk helpers (mayConsumeSign, mayConsumeDecimalFraction, mayConsumeImaginary)
- [ ] Phase 2: Special number consolidation
- [ ] Phase 3: Complex suffix unification
- [ ] Phase 4: State lookup table
- [ ] Phase 5: Optional sub-tokenizer architecture (advanced)

## Risk
Medium-High (number parsing is security-critical)
EOF
)"

echo "Creating: EmptyList/Void Refactoring..."
gh issue create \
  --title "EmptyList/Void Refactoring" \
  --label "refactoring" \
  --body "$(cat <<EOF
## Summary
Fix fundamental type confusion where \`EmptyList\` is defined as a \`*Pair\` singleton but should have its own type. Eliminates ~160 defensive \`IsEmptyList()\` checks and fixes type switches that incorrectly match empty lists.

## Design
See [\`plans/EMPTY_LIST_VOID_REFACTORING.md\`](./plans/EMPTY_LIST_VOID_REFACTORING.md) for options.

## Recommended Approach
Option A (minimal, lower risk) — Create \`emptyListType struct{}\` implementing \`Value\` and \`Tuple\`.

## Tasks
- [ ] Step 1: Create \`emptyListType\`
- [ ] Step 2: Update \`Pair\` type assumptions
- [ ] Step 3: Update \`ArrayList\`
- [ ] Step 4: Update utility functions
- [ ] Step 5: Simplify predicates (pair?, null?)
- [ ] Step 6: Create \`syntaxEmptyListType\` in syntax layer
- [ ] Step 7: Fix type switches (~13 locations)
- [ ] Step 8: Remove FIXME comments

## Impact
Better type safety, simpler code, ~160 defensive checks removed.
EOF
)"

echo "Creating: Subsystem Simplification..."
gh issue create \
  --title "Subsystem Simplification (Remaining Items)" \
  --label "refactoring" \
  --body "$(cat <<'EOF'
## Summary
Additional architectural simplifications identified during codebase review. Most items already complete; this tracks remaining opportunities.

## Design
See [\`plans/SUBSYSTEM_SIMPLIFICATION.md\`](./plans/SUBSYSTEM_SIMPLIFICATION.md) for full list.

## Status
9/9 original phases complete. This issue tracks follow-up items and periodic review for new simplification opportunities.

## Context
Regular architectural review keeps the codebase maintainable and prevents accidental complexity.
EOF
)"

echo "Creating: Algebraic Reductions..."
gh issue create \
  --title "Algebraic Reductions (Remaining Items)" \
  --label "refactoring" \
  --body "$(cat <<'EOF'
## Summary
Structural simplifications where multiple operations expressed differently can be unified. Port/Binding items complete; 10 opportunities remain.

## Design
See [\`plans/ALGEBRAIC_REDUCTIONS.md\`](./plans/ALGEBRAIC_REDUCTIONS.md) for priority lattice.

## Identified Opportunities (Priority Order)
1. **Optional range argument parsing** (Low Risk)
2. **Chain equality predicates** (Low Risk)
3. **Structural equality cycle detection** (Low Risk)
4. **ForEach / SyntaxForEach** (Low Risk)
5. **Bootstrap environment initialization** (Low Risk)
6. **Syntax interface boilerplate** (Low-Medium Risk)
7. **Scope propagation asymmetry** (Low-Medium Risk)
8. **Scope-aware symbol resolution** (Medium Risk)
9. **Operation interface boilerplate** (Medium Risk)
10. **Numeric tower dispatch** (High Risk — deferred)

## Recommendation
Start with items 1-5 (all Low Risk).
EOF
)"

echo "Creating: Code Consolidation (Codegen)..."
gh issue create \
  --title "Code Consolidation (Operation Boilerplate)" \
  --label "refactoring" \
  --body "$(cat <<'EOF'
## Summary
Optional refactoring to eliminate ~220 lines of boilerplate across 29 operation files. Only code generation approach included (HIGH risk).

## Design
See [\`plans/CODE_CONSOLIDATION_ARCHITECTURAL.md\`](./plans/CODE_CONSOLIDATION_ARCHITECTURAL.md) for analysis.

## Alternative
Embedding with generics (lower risk, ~60 line savings).

## Decision
Optional, deferred (not blocking release).

## Tasks (if pursued)
- [ ] Evaluate embedding vs code generation approach
- [ ] Implement chosen approach
- [ ] Verify all operation types covered
- [ ] Test edge cases
EOF
)"

echo "Creating: Hygiene Debugging..."
gh issue create \
  --title "Hygiene Debugging Tools" \
  --label "tooling,planned" \
  --body "$(cat <<EOF
## Summary
Rich debugging tooling for hygienic macro system. Emphasizes introspection over manipulation.

## Design
See [\`plans/HYGIENE_DEBUGGING_DESIGN.md\`](./plans/HYGIENE_DEBUGGING_DESIGN.md) for full design.

## Components
- Enhanced \`Scope\` type with provenance fields (\`id\`, \`reason\`, \`formName\`, \`location\`)
- Debugging primitives: \`identifier-scopes\`, \`scope-info\`, \`binding-info\`, \`scope?\`, \`scope=?\`
- Enhanced error messages showing scope sets and resolution details

## Phases
- [ ] Phase 1: Scope provenance infrastructure (~100 LOC)
- [ ] Phase 2: Update scope creation sites (~50 LOC)
- [ ] Phase 3: Debugging primitives (~200 LOC)
- [ ] Phase 4: Enhanced error messages (~100 LOC)

## Use Cases
- Understanding why a macro doesn\'t work
- Debugging hygiene violations
- Learning how scope sets work
EOF
)"

echo ""
echo "✅ All 30 issues created successfully!"
echo ""
echo "Next steps:"
echo "1. Review issues at: https://github.com/aalpar/wile/issues"
echo "2. Create CONTRIBUTING.md (see output below)"
echo "3. Update README.md with Contributing section"
echo "4. Announce on Reddit, Lobsters, HN"
echo ""
