# Disassembler Design

**Goal:** Expose bytecode disassembly as structured Scheme data (alists), a Go debug string, a REPL meta-command, and an MCP tool.

**Architecture:** Core logic in `machine/`, Scheme primitive in `extensions/introspection/`, REPL command in `internal/repl/`, MCP tool in `cmd/wile/`.

**Tech Stack:** Go, existing `machine` types, `extensions/introspection` extension, `internal/repl` meta-commands

---

## Background

Wile compiles Scheme to bytecode (`NativeTemplate.code []Instruction`) and executes it on a stack-based VM. When working on the compiler, optimizer, or debugging macro expansion, there is no way to inspect what bytecode a Scheme expression compiles to. You must trace through `compile_*.go` by hand or read Go test assertions.

A disassembler that shows the opcode sequence with rich annotations (resolved literals, binding names, absolute branch targets, source locations) makes the compilation output directly inspectable.

### Existing Infrastructure

| Component | Location | Provides |
|-----------|----------|----------|
| `OpCode.String()` | `machine/opcode.go:220` | Human-readable opcode name |
| `Instruction.String()` | `machine/instruction.go:45` | Formatted instruction with decoded args |
| `NativeTemplate.Code()` | `machine/native_template.go:577` | Bytecode slice |
| `NativeTemplate.SideTable()` | `machine/native_template.go:582` | Complex ops referenced by `OpComplex` |
| `NativeTemplate.Literals()` | `machine/native_template.go:572` | Literal pool |
| `NativeTemplate.SourceAt(pc)` | `machine/native_template.go:235` | Source location per instruction |
| `MachineClosure.Template()` | `machine/machine_closure.go:59` | Access to closure's template |
| `CaseLambdaClosure.Clauses()` | `machine/case_lambda_closure.go:38` | Access to clause closures |
| `opcodeTable[op].isBranch` | `machine/opcode.go:150` | Branch opcode identification |

### Missing

| What | Why Needed |
|------|-----------|
| `NativeTemplate.CachedBindings()` | Read-only access to the `cachedBindings` array for annotation |
| Disassembly logic | No function walks a template producing annotated output |
| Scheme primitive | No `(disassemble proc)` |
| REPL command | No `,dis name` |
| MCP tool | No `disassemble` tool |

---

## Design

### Layer 1: Go Core (`machine/disassemble.go`)

#### DisassembledInstruction

```go
type DisassembledInstruction struct {
    PC      int
    Op      string  // OpCode.String()
    Arg     int32   // raw Arg value
    Slot    int     // decoded slot (LoadLocal/StoreLocal/PushLocal family)
    Depth   int     // decoded depth (LoadLocal/StoreLocal/PushLocal family)
    Target  int     // absolute branch target (branch/save ops)
    Literal string  // SchemeString() of resolved literal value
    Binding string  // name of cached binding's value (closure Name())
    SideOp  string  // String() of InlinedOperation (OpComplex)
    Source  string  // "file:line:col" or ""
}
```

Fields are zero-valued when not applicable to the instruction's opcode. The `Op` field is always set.

#### DisassembledTemplate

```go
type DisassembledTemplate struct {
    Name         string
    ParamCount   int
    IsVariadic   bool
    Doc          string
    Literals     []string // SchemeString() of each literal
    Bindings     []string // name of each cached binding
    Instructions []DisassembledInstruction
}
```

#### Functions

```go
// Disassemble produces structured disassembly of a NativeTemplate.
func Disassemble(tpl *NativeTemplate) DisassembledTemplate

// DisassembleString produces a human-readable disassembly listing.
// One line per instruction, columnar format with annotations.
func DisassembleString(tpl *NativeTemplate) string
```

#### Annotation Rules

| OpCode Family | Annotations |
|--------------|-------------|
| `OpLoadLiteral`, `OpPushLiteral` | Resolve `Arg` as literal index → `Literal` = `literals[arg].SchemeString()` |
| `OpLoadLocal`, `OpStoreLocal`, `OpPushLocal`, `OpCallLocal` | Decode bit-packed arg → `Slot`, `Depth` |
| `OpLoadGlobal`, `OpStoreGlobal`, `OpPushGlobal` | Resolve `Arg` as literal index → `Literal` (the symbol name) |
| `OpLoadCachedBinding`, `OpPushCachedBinding`, `OpCallCachedBinding`, `OpCallForeignCached`, `OpCallForeignCachedTail` | Resolve `Arg` as binding index → `Binding` (name of the bound closure) |
| Promoted ops (`OpAdd`, `OpCar`, `OpEqQ`, etc.) | Resolve `Arg` as binding index → `Binding` |
| `OpBranch`, `OpBranchOnFalseValue`, `OpSaveContinuation` | Compute `Target` = `PC + Arg` |
| `OpComplex` | Resolve `Arg` as side table index → `SideOp` = `sideTable[arg].String()` |
| `OpMakeClosure` | Check if the preceding `LoadLiteral`/`PushLiteral` loaded a `*NativeTemplate`; if so, set `Literal` = `<lambda:name>` or `<lambda>` |
| `OpPushEnv` | `Literal` = slot count (from Arg) |
| `OpPeekK` | `Literal` = depth (from Arg) |
| All ops | `Source` from `tpl.SourceAt(pc)` |

#### Go String Format

```
add1  (params: 1, variadic: #f)
literals: [x, +, 1]
cached bindings: [0: +]

  PC  OP                   ARG   DETAIL                  SOURCE
   0  PushEnv                1   slots=1
   1  PushCachedBinding      0   +
   2  PushLocal          65536   slot=0 depth=1          test.scm:1:16
   3  PushLiteral            2   1
   4  PullApply
   5  PopEnv
   6  RestoreContinuation
```

#### New Accessor

Add to `native_template.go`:

```go
// CachedBindings returns the compile-time resolved bindings array.
// Used by the disassembler to annotate binding references.
func (p *NativeTemplate) CachedBindings() []*environment.Binding {
    return p.cachedBindings
}
```

### Layer 2: Scheme Primitive (`extensions/introspection/prim_disassemble.go`)

#### `(disassemble proc) → list`

Accepts any callable. Returns a list whose `car` is a header alist and whose `cdr` is instruction alists.

**Type dispatch:**

| Input Type | Behavior |
|-----------|----------|
| `*MachineClosure` | Disassemble `Template()` |
| `*CaseLambdaClosure` | Header with `(type . case-lambda-closure)`, `(clauses . (dis0 dis1 ...))` |
| `*ForeignClosure` | Header only: `(type . foreign-closure)`, `(name ...)`, `(params ...)`, `(variadic ...)`, `(doc ...)` |
| Other | Error: "disassemble: not a procedure" |

**Header alist keys:**

| Key | Value | Present When |
|-----|-------|-------------|
| `type` | symbol: `native-closure`, `case-lambda-closure`, `foreign-closure` | always |
| `name` | string | always |
| `params` | integer | always |
| `variadic` | boolean | always |
| `doc` | string | always (may be empty) |
| `literals` | vector of values | native-closure only |
| `bindings` | vector of strings | native-closure only |
| `clauses` | list of disassemblies | case-lambda only |

**Instruction alist keys:**

| Key | Value | Present When |
|-----|-------|-------------|
| `pc` | integer | always |
| `op` | symbol | always |
| `arg` | integer | when Arg ≠ 0 |
| `slot` | integer | local ops |
| `depth` | integer | local ops |
| `target` | integer | branch/save ops |
| `literal` | string | literal/global ops |
| `binding` | string | cached binding ops |
| `side-op` | string | OpComplex |
| `source` | string | when source available |

**Registration** in `extensions/introspection/register.go`:

```go
{Name: "disassemble", ParamCount: 1, Impl: PrimDisassemble,
    Doc: "Returns structured disassembly of a procedure as a list of alists. "
         "The first element is a header with metadata; remaining elements are "
         "instruction alists with pc, op, and annotation keys.",
    ParamNames: []string{"proc"}, Category: "introspection"},
```

### Layer 3: REPL Meta-Command (`,dis`)

**Command:** `,disassemble <name>` / `,dis <name>`

Looks up `<name>` as a binding in the environment (same lookup path as `,doc`), extracts the value, type-switches on closure types, calls `DisassembleString`, and prints. For `CaseLambdaClosure`, prints each clause separated by a blank line. For `ForeignClosure`, prints a summary (name, arity, doc).

**Implementation** in `internal/repl/meta.go`:

1. Add case `"disassemble", "dis":` to the `Handle` switch
2. Add `cmdDisassemble(args, out)` method
3. Add entry to `metaCommands` table
4. Method body: look up binding → get value → type switch → format → `writeWithPager`

The method imports `machine` (already imported by `meta.go` for `callableDoc`).

### Layer 4: MCP Tool

**Tool name:** `disassemble`

**Parameters:**
- `expression` (string, required): Scheme expression that evaluates to a procedure

**Behavior:** Evaluates the expression in the session, then calls the REPL `MetaCommandHandler`'s disassemble path. Returns the formatted text output.

**Implementation** in `cmd/wile/mcp.go`: follows the same pattern as the existing `doc` tool — evaluate → format → return text.

---

## Scope

**In scope:**
- `DisassembledInstruction` and `DisassembledTemplate` Go types
- `Disassemble()` and `DisassembleString()` in `machine/`
- `CachedBindings()` accessor on `NativeTemplate`
- `(disassemble proc)` Scheme primitive in `extensions/introspection/`
- `,dis` / `,disassemble` REPL meta-command
- `disassemble` MCP tool
- Tests for all layers

**Not in scope:**
- Assembler (future — the alist format is designed for it)
- Recursive sub-closure disassembly (MakeClosure shows name only)
- Modifying `OpCode`, `Instruction`, or `NativeTemplate` (beyond the accessor)

---

## Open Questions

1. **Should `,dis` accept expressions or only names?** The meta-command handler doesn't have an evaluator — it only has the environment frame. `,doc` works the same way (looks up by name). The Scheme `(disassemble expr)` primitive handles expressions since the VM evaluates the argument. Design: `,dis` does name lookup only; `(disassemble (lambda () ...))` for ad-hoc expressions.

2. **MakeClosure annotation**: When `OpMakeClosure` follows a literal load, the literal is a `*NativeTemplate`. Annotate with `<lambda:name>` if named, `<lambda>` otherwise. The actual sub-template contents require a separate `(disassemble ...)` call — consistent with how Chez and Guile work.
