# REPL Enhancements: Meta-Commands, Autocomplete, Pager

**Date:** 2026-02-26
**Status:** Implemented (see `plans/2026-02-26-repl-enhancements.md`)

## Summary

Add interactive development features to the Wile REPL: meta-commands (`,help`,
`,doc`, `,edit`), tab completion for Scheme bindings and meta-commands, and
`$PAGER`/`$EDITOR` integration. These are REPL-session concerns implemented in
Go, not Scheme primitives — they can be promoted to primitives later. The Go
layer stays thin; richer Smalltalk-style tooling will be built in Scheme on top.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Command syntax | `,` prefix (meta-commands) | Matches existing debug commands; not Scheme forms |
| `,help` scope | Meta-commands only | Like bash `help` vs `man`; separate from `,doc` |
| `,doc` scope | Scheme bindings (all phases) | Like bash `man`; distinct help surface |
| `,edit` no-arg | Error | Simple; no scratch buffer complexity |
| `$EDITOR` fallback | Error if unset | Explicit > implicit; no guessing `vi` |
| `$PAGER` | Always pipe when set | Let pager decide (e.g. `less -F`); no length threshold |
| Doc for user bindings | Type + value | e.g. `x: integer = 42` |
| Autocomplete scope | All phase bindings + meta-commands + filenames | Context-dependent completion |

## Architecture

### Meta-Command Dispatch (Two-Layer)

```
REPL.Run()
  ├── MetaCommandHandler.Handle(line)    ← new, owns all "," dispatch
  │   ├── session commands: ,help, ,doc, ,edit
  │   └── delegates to DebugContext for debug commands
  └── Scheme eval (if not a meta-command)
```

`MetaCommandHandler` replaces the direct `DebugContext.HandleDebugCommand` call
in the REPL loop. Keeps debug.go untouched.

### `,doc` — Phase Traversal

Walk `PhaseRegistry.envs` sorted by phase index (0 → 1 → 2). First match wins.

| Binding found in | Binding type | Display |
|-----------------|-------------|---------|
| Runtime (0) | Primitive | `PrimitiveSpec.Doc`, `ParamNames`, `Category` |
| Runtime (0) | Variable (user-defined) | Type + value (no doc string) |
| Expand (1) | Primitive (expander) | "primitive expander" (no doc yet) |
| Expand (1) | Syntax (macro) | "syntax transformer" (no doc yet) |
| Compile (2) | Primitive (compiler) | "syntax compiler" (no doc yet) |
| Not found | — | "Unbound identifier" |

Primitive doc output format:
```
(+ n1 n2 ...)
  Returns the sum of its arguments.
  Category: arithmetic
```

User binding output format:
```
x: integer = 42
```

### DocProvider Interface

Decouples `internal/repl` from `registry`:

```go
type DocProvider interface {
    LookupDoc(name string) (doc string, paramNames []string, category string, found bool)
}
```

The `cmd/scheme` binary wires `*registry.Registry` as the doc provider.

### `,edit` — Editor Integration

`$EDITOR <file>` as subprocess, block REPL until editor exits. Error if
`$EDITOR` unset. Error if no file argument.

### `,help` — Meta-Command Help

Lists all meta-commands grouped by category (session, debug). Piped through
`$PAGER` when set. `,help <command>` shows detailed help for a specific
meta-command.

### Pager Helper

```go
func writeWithPager(out io.Writer, content string)
```

Always invokes `$PAGER` when set. Falls back to direct write when unset.

### Autocomplete

Implements `readline.AutoCompleter` interface:

```go
type SchemeCompleter struct {
    env *environment.EnvironmentFrame
    // meta-command names for "," completion
}

func (c *SchemeCompleter) Do(line []rune, pos int) ([][]rune, int)
```

Context detection:
1. Line starts with `,` → complete meta-command names
2. After `,edit ` or `,load ` → complete filenames
3. Otherwise → complete Scheme bindings from all phase environments

Binding enumeration: walk `PhaseRegistry.Phases()` sorted, collect
`GlobalEnvironmentFrame.Keys()` from each phase. Deduplicate (first phase wins).

## File Changes

```
internal/repl/
  repl.go           ← modified: wire MetaCommandHandler + AutoComplete
  meta.go           ← new: MetaCommandHandler, session commands
  completer.go      ← new: SchemeCompleter
  pager.go          ← new: writeWithPager helper
  debug.go          ← unchanged

cmd/scheme/
  main.go           ← modified: wire DocProvider
```

## Future Work (Explicitly Out of Scope)

- `Doc` field on `FormSpec` for syntax form documentation
- Stash/scratch buffer system
- Meta-object protocol on bindings (Lua-style)
- Promoting meta-commands to Scheme primitives
- Syntax highlighting in REPL output
