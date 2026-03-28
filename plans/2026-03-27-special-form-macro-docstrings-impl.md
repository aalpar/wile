# Special Form & Macro Docstrings Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `,doc` display documentation for compiler special forms (~20) and bootstrap macros (~15) by adding a `Doc` field to `BindingMeta` and flowing doc content through the registry into environment bindings.

**Architecture:** Three layers — (1) `BindingMeta.Doc` field in `environment/`, (2) `BindingSpec`/`DocEntry` types + `ApplyDocs` in `registry/`, (3) content in `registry/core/specialforms.go` + display in `internal/repl/meta.go`. No new Scheme primitives.

**Tech Stack:** Go, existing `environment/`, `registry/`, `registry/core/`, `internal/repl/` packages. No new dependencies.

**Design doc:** `plans/2026-03-27-special-form-macro-docstrings-design.md`

---

### Task 1: Add `Doc` field to `BindingMeta`

**Files:**
- Modify: `environment/binding.go`
- Modify: `environment/binding_test.go`

**Step 1: Write the failing test**

Add to `environment/binding_test.go`:

```go
func TestBinding_Doc(t *testing.T) {
	b := NewBinding(values.Void, BindingTypePrimitive)
	qt.Assert(t, b.Doc(), qt.Equals, "")

	b.SetDoc("Conditional expression.")
	qt.Assert(t, b.Doc(), qt.Equals, "Conditional expression.")
}

func TestBinding_Doc_PreservesExistingMeta(t *testing.T) {
	scope := syntax.NewScope()
	b := NewBindingWithScopes(values.NewInteger(1), BindingTypeVariable, []*syntax.Scope{scope})

	b.SetDoc("A documented binding.")
	qt.Assert(t, b.Doc(), qt.Equals, "A documented binding.")
	qt.Assert(t, b.Scopes(), qt.HasLen, 1)
}

func TestBinding_Copy_WithDoc(t *testing.T) {
	b1 := NewBinding(values.Void, BindingTypePrimitive)
	b1.SetDoc("Original doc.")

	copied := b1.Copy()
	b2, ok := copied.(*Binding)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, b2.Doc(), qt.Equals, "Original doc.")

	// Mutation independence
	b2.SetDoc("Changed doc.")
	qt.Assert(t, b1.Doc(), qt.Equals, "Original doc.")
	qt.Assert(t, b2.Doc(), qt.Equals, "Changed doc.")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run 'TestBinding_Doc|TestBinding_Copy_WithDoc' ./environment/...`
Expected: FAIL — `Doc` and `SetDoc` don't exist

**Step 3: Implement**

In `environment/binding.go`:

1. Add `Doc` field to `BindingMeta` (line 27):

```go
type BindingMeta struct {
	Scopes []*syntax.Scope
	Source *syntax.SourceContext
	Doc    string
}
```

2. Add accessors after `SetSource` (after line 123):

```go
// Doc returns the documentation string for this binding.
// Returns empty string for bindings without documentation.
func (p *Binding) Doc() string {
	if p.meta == nil {
		return ""
	}
	return p.meta.Doc
}

// SetDoc sets the documentation string for this binding.
func (p *Binding) SetDoc(doc string) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Doc = doc
}
```

3. Update `Copy()` to include `Doc` (line 160):

```go
if p.meta != nil {
	b.meta = &BindingMeta{
		Scopes: p.meta.Scopes,
		Source: p.meta.Source,
		Doc:    p.meta.Doc,
	}
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run 'TestBinding_Doc|TestBinding_Copy_WithDoc' ./environment/...`
Expected: PASS

**Step 5: Run full environment tests for regressions**

Run: `go test ./environment/... -count=1`
Expected: PASS

**Step 6: Commit**

```
feat(environment): add Doc field to BindingMeta
```

---

### Task 2: Add `BindingSpec` and `DocEntry` types to Registry

**Files:**
- Modify: `registry/registry.go`
- Modify: `registry/registry_test.go`

**Step 1: Write the failing tests**

Add to `registry/registry_test.go`:

```go
func TestRegistry_AddBindingSpecs(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddBindingSpecs([]BindingSpec{
		{Name: "if", Doc: "Conditional expression."},
		{Name: "lambda", Doc: "Anonymous procedure."},
		{Name: "else"},
	})
	c.Assert(r.BindingCount(), qt.Equals, 3)

	// Bindings() still returns names for backward compatibility
	bindings := r.Bindings()
	c.Assert(bindings, qt.DeepEquals, []string{"if", "lambda", "else"})

	// BindingSpecs() returns full specs
	specs := r.BindingSpecs()
	c.Assert(specs, qt.HasLen, 3)
	c.Assert(specs[0].Name, qt.Equals, "if")
	c.Assert(specs[0].Doc, qt.Equals, "Conditional expression.")
	c.Assert(specs[2].Doc, qt.Equals, "")
}

func TestRegistry_AddBindings_BackwardCompat(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddBindings([]string{"if", "lambda"})
	specs := r.BindingSpecs()
	c.Assert(specs, qt.HasLen, 2)
	c.Assert(specs[0].Name, qt.Equals, "if")
	c.Assert(specs[0].Doc, qt.Equals, "")
}

func TestRegistry_AddDocumentation(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddDocumentation("and", "Short-circuit conjunction.")
	r.AddDocumentation("or", "Short-circuit disjunction.")

	docs := r.Docs()
	c.Assert(docs, qt.HasLen, 2)
	c.Assert(docs[0].Name, qt.Equals, "and")
	c.Assert(docs[0].Doc, qt.Equals, "Short-circuit conjunction.")
}

func TestRegistry_Clone_IncludesDocs(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddDocumentation("and", "Short-circuit conjunction.")
	r.AddBindingSpecs([]BindingSpec{{Name: "if", Doc: "Conditional."}})

	r2 := r.Clone()
	c.Assert(r2.Docs(), qt.HasLen, 1)
	c.Assert(r2.BindingSpecs(), qt.HasLen, 1)

	// Independence: mutating r2 doesn't affect r
	r2.AddDocumentation("or", "Disjunction.")
	c.Assert(r.Docs(), qt.HasLen, 1)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run 'TestRegistry_AddBindingSpecs|TestRegistry_AddBindings_BackwardCompat|TestRegistry_AddDocumentation|TestRegistry_Clone_IncludesDocs' ./registry/...`
Expected: FAIL — types don't exist

**Step 3: Implement**

In `registry/registry.go`:

1. Add types after `GlobalValue` (after line 51):

```go
// BindingSpec defines a compile-time binding with optional documentation.
type BindingSpec struct {
	Name string
	Doc  string
}

// DocEntry pairs a binding name with documentation to be injected
// after bootstrap loading (for macros defined in Scheme source).
type DocEntry struct {
	Name string
	Doc  string
}
```

2. Change `bindings` field from `[]string` to `[]BindingSpec` in `Registry` struct (line 57):

```go
type Registry struct {
	mu           sync.RWMutex
	primitives   []PrimitiveRegistration
	bindingSpecs []BindingSpec // Compile-time only bindings
	docs         []DocEntry   // Post-bootstrap doc injection
	initFuncs    []InitFunc
	macroSources []string
	globalValues []GlobalValue
}
```

3. Update `NewRegistry` (line 64):

```go
func NewRegistry() *Registry {
	q := &Registry{
		primitives:   make([]PrimitiveRegistration, 0, 128),
		bindingSpecs: make([]BindingSpec, 0, 32),
		docs:         make([]DocEntry, 0, 16),
		initFuncs:    make([]InitFunc, 0, 8),
		macroSources: make([]string, 0, 4),
		globalValues: make([]GlobalValue, 0, 4),
	}
	return q
}
```

4. Update `AddBinding` (line 124):

```go
func (p *Registry) AddBinding(name string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindingSpecs = append(p.bindingSpecs, BindingSpec{Name: name})
}
```

5. Update `AddBindings` (line 131):

```go
func (p *Registry) AddBindings(names []string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, name := range names {
		p.bindingSpecs = append(p.bindingSpecs, BindingSpec{Name: name})
	}
}
```

6. Add new methods after `AddBindings`:

```go
// AddBindingSpecs registers compile-time bindings with optional documentation.
func (p *Registry) AddBindingSpecs(specs []BindingSpec) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindingSpecs = append(p.bindingSpecs, specs...)
}

// AddDocumentation registers documentation to be injected into an existing
// binding after bootstrap loading. Use for macros defined in Scheme source
// whose bindings don't exist until bootstrap evaluates define-syntax.
func (p *Registry) AddDocumentation(name, doc string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.docs = append(p.docs, DocEntry{Name: name, Doc: doc})
}
```

7. Update `BindingCount` (line 197):

```go
func (p *Registry) BindingCount() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.bindingSpecs)
}
```

8. Update `Bindings()` to return names for backward compatibility (line 222):

```go
func (p *Registry) Bindings() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]string, len(p.bindingSpecs))
	for i, spec := range p.bindingSpecs {
		q[i] = spec.Name
	}
	return q
}
```

9. Add `BindingSpecs()` and `Docs()` accessors:

```go
// BindingSpecs returns a copy of the compile-time binding specs.
func (p *Registry) BindingSpecs() []BindingSpec {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]BindingSpec, len(p.bindingSpecs))
	copy(q, p.bindingSpecs)
	return q
}

// Docs returns a copy of the post-bootstrap documentation entries.
func (p *Registry) Docs() []DocEntry {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]DocEntry, len(p.docs))
	copy(q, p.docs)
	return q
}
```

10. Update `Clone` to include new fields (line 249):

```go
func (p *Registry) Clone() *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, len(p.primitives)),
		bindingSpecs: make([]BindingSpec, len(p.bindingSpecs)),
		docs:         make([]DocEntry, len(p.docs)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	copy(q.primitives, p.primitives)
	copy(q.bindingSpecs, p.bindingSpecs)
	copy(q.docs, p.docs)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.globalValues, p.globalValues)
	return q
}
```

11. Update `WithoutBindings` to use `bindingSpecs` (line 394):

```go
func (p *Registry) WithoutBindings(names ...string) *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()

	exclude := make(map[string]struct{}, len(names))
	for _, name := range names {
		exclude[name] = struct{}{}
	}

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, len(p.primitives)),
		bindingSpecs: make([]BindingSpec, 0, len(p.bindingSpecs)),
		docs:         make([]DocEntry, len(p.docs)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	copy(q.primitives, p.primitives)
	for _, spec := range p.bindingSpecs {
		_, ok := exclude[spec.Name]
		if ok {
			continue
		}
		q.bindingSpecs = append(q.bindingSpecs, spec)
	}
	copy(q.docs, p.docs)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.globalValues, p.globalValues)
	return q
}
```

12. Update `filterPrimitives` to include `bindingSpecs` and `docs` (line 360):

```go
func (p *Registry) filterPrimitives(exclude []string, keyFn func(PrimitiveRegistration) string) *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()

	set := make(map[string]struct{}, len(exclude))
	for _, v := range exclude {
		set[v] = struct{}{}
	}

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, 0, len(p.primitives)),
		bindingSpecs: make([]BindingSpec, len(p.bindingSpecs)),
		docs:         make([]DocEntry, len(p.docs)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	for _, reg := range p.primitives {
		_, ok := set[keyFn(reg)]
		if ok {
			continue
		}
		q.primitives = append(q.primitives, reg)
	}
	copy(q.bindingSpecs, p.bindingSpecs)
	copy(q.docs, p.docs)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.globalValues, p.globalValues)
	return q
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestRegistry_AddBindingSpecs|TestRegistry_AddBindings|TestRegistry_AddDocumentation|TestRegistry_Clone_IncludesDocs' ./registry/...`
Expected: PASS

**Step 5: Run full registry tests for regressions**

Run: `go test ./registry/... -count=1`
Expected: PASS

**Step 6: Commit**

```
feat(registry): add BindingSpec, DocEntry types and AddDocumentation method
```

---

### Task 3: Wire `BindingSpec` through `Apply` and add `ApplyDocs`

**Files:**
- Modify: `registry/apply.go`

**Step 1: Update `Apply` to use `bindingSpecs`**

In `registry/apply.go`, change the compile-time binding loop (line 33) and `registerCompileTimeBinding` (line 90):

Replace line 33-38:
```go
	// Register compile-time bindings first
	for _, spec := range p.bindingSpecs {
		err := registerCompileTimeBinding(env, spec)
		if err != nil {
			return err
		}
	}
```

Replace line 42-43 (compile-time primitives also call `registerCompileTimeBinding`):
```go
		if reg.Phases.HasCompile() && !reg.Phases.HasRuntime() {
			err := registerCompileTimeBinding(env, BindingSpec{Name: reg.Spec.Name})
```

Replace `registerCompileTimeBinding` (lines 90-95):
```go
func registerCompileTimeBinding(env *environment.EnvironmentFrame, spec BindingSpec) error {
	compileEnv := env.Compile()
	sym := values.NewSymbol(spec.Name)
	gi, _ := compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
	if spec.Doc != "" {
		bnd := compileEnv.GetOwnGlobalBinding(gi)
		if bnd != nil {
			bnd.SetDoc(spec.Doc)
		}
	}
	return nil
}
```

**Step 2: Add `ApplyDocs` method**

Add after `registerExpandTimePrimitive` (after line 147):

```go
// ApplyDocs injects documentation into existing bindings across all phases.
// Call after bootstrap macro loading so that expand-time bindings exist.
func (p *Registry) ApplyDocs(env *environment.EnvironmentFrame) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	topLevel := env.Namespace()
	if topLevel == nil {
		return
	}
	phases := topLevel.Phases()

	for _, doc := range p.docs {
		sym := values.NewSymbol(doc.Name)
		for _, phase := range phases.Phases() {
			phaseEnv := phases.Get(phase)
			if phaseEnv == nil {
				continue
			}
			bnd := phaseEnv.GetBinding(sym)
			if bnd != nil {
				bnd.SetDoc(doc.Doc)
				break
			}
		}
	}
}
```

**Step 3: Check that `GetOwnGlobalBinding` is accessible**

`GetOwnGlobalBinding` is on `GlobalEnvironmentFrame`, but we need it via `EnvironmentFrame`. Check if there's a delegation method, or use `GetBinding` instead.

Look at `registerCompileTimeBinding`: after `MaybeCreateOwnGlobalBinding` returns `(gi, _)`, we need the binding. The cleanest path: use `compileEnv.GetBinding(sym)` which already exists and does the right thing.

Revised `registerCompileTimeBinding`:
```go
func registerCompileTimeBinding(env *environment.EnvironmentFrame, spec BindingSpec) error {
	compileEnv := env.Compile()
	sym := values.NewSymbol(spec.Name)
	compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
	if spec.Doc != "" {
		bnd := compileEnv.GetBinding(sym)
		if bnd != nil {
			bnd.SetDoc(spec.Doc)
		}
	}
	return nil
}
```

**Step 4: Run tests**

Run: `go test ./registry/... -count=1`
Expected: PASS

**Step 5: Commit**

```
feat(registry): wire BindingSpec docs through Apply, add ApplyDocs
```

---

### Task 4: Call `ApplyDocs` after bootstrap in Engine

**Files:**
- Modify: `engine.go`

**Step 1: Add `ApplyDocs` call in `applyBaseEnvironment`**

In `engine.go`, function `applyBaseEnvironment` (line 566), add the call after `loadBootstrapMacros` (after line 585):

```go
func applyBaseEnvironment(ctx context.Context, env *environment.EnvironmentFrame, reg *registry.Registry, macroSources []string, resolver machine.FileResolver) error {
	err := reg.Apply(ctx, env)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "apply registry")
	}

	err = machine.RegisterSyntaxCompilers(env)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register syntax compilers")
	}

	err = machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register primitive expanders")
	}

	err = loadBootstrapMacros(ctx, env, macroSources, resolver)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "load bootstrap macros")
	}

	// Inject documentation into bootstrap macro bindings (expand-time).
	// Must run after loadBootstrapMacros so define-syntax bindings exist.
	reg.ApplyDocs(env)

	return nil
}
```

**Step 2: Run engine tests**

Run: `go test ./... -count=1 -short`
Expected: PASS (no behavioral change yet — no docs registered)

**Step 3: Commit**

```
feat(engine): call ApplyDocs after bootstrap macro loading
```

---

### Task 5: Add doc content for special forms

**Files:**
- Modify: `registry/core/specialforms.go`

**Step 1: Convert `compileTimeBindings` to `compileTimeBindingSpecs`**

Replace the entire `compileTimeBindings` variable and `addSpecialForms` function:

```go
//nolint:govet // BindingSpec uses unkeyed fields for conciseness
var compileTimeBindingSpecs = []registry.BindingSpec{
	{Name: "if", Doc: "Conditional expression.\nSyntax: (if <test> <consequent> <alternate>)\nEvaluates <test>; if it yields a true value, <consequent> is\nevaluated and its value returned. Otherwise <alternate> is\nevaluated and returned. If <alternate> is omitted and <test>\nyields #f, the result is unspecified. R7RS §4.1.5."},
	{Name: "lambda", Doc: "Anonymous procedure.\nSyntax: (lambda <formals> <body>)\n<formals> is a list of parameter names, a single symbol (rest arg),\nor a dotted pair (fixed args + rest). <body> is one or more\nexpressions with implicit begin. R7RS §4.1.4."},
	{Name: "case-lambda", Doc: "Multi-clause procedure.\nSyntax: (case-lambda (<formals> <body>) ...)\nDispatches to the first clause whose <formals> match the number\nof arguments. Each clause is like a lambda. R7RS §4.2.9."},
	{Name: "quote", Doc: "Literal datum.\nSyntax: (quote <datum>) or '<datum>\nReturns <datum> without evaluating it. R7RS §4.1.2."},
	{Name: "define", Doc: "Variable or procedure definition.\nSyntax: (define <variable> <expression>)\n        (define (<variable> <formals>) <body>)\nThe second form is equivalent to\n(define <variable> (lambda (<formals>) <body>)).\nR7RS §5.3."},
	{Name: "define-syntax", Doc: "Macro definition.\nSyntax: (define-syntax <keyword> <transformer>)\nBinds <keyword> to the syntax transformer produced by\n<transformer> (typically a syntax-rules form). R7RS §5.4."},
	{Name: "set!", Doc: "Assignment.\nSyntax: (set! <variable> <expression>)\nEvaluates <expression> and stores the result in <variable>,\nwhich must already be bound. R7RS §4.1.6."},
	{Name: "begin", Doc: "Sequencing.\nSyntax: (begin <expression1> <expression2> ...)\nEvaluates expressions left-to-right, returns the value of the\nlast. At top level or in a body, also acts as a splicing form\nthat merges its contents into the enclosing sequence. R7RS §4.2.3."},
	{Name: "include", Doc: "File inclusion.\nSyntax: (include <filename1> <filename2> ...)\nReplaces the include form with the contents of the named files,\nread as Scheme expressions. Processed at expand time.\nR7RS §4.1.7."},
	{Name: "include-ci", Doc: "Case-insensitive file inclusion.\nSyntax: (include-ci <filename1> <filename2> ...)\nLike include, but reads identifiers case-insensitively.\nR7RS §4.1.7."},
	{Name: "quasiquote", Doc: "Quasiquotation template.\nSyntax: (quasiquote <template>) or `<template>\nLike quote, but unquote and unquote-splicing escapes within\n<template> are evaluated and substituted. R7RS §4.2.8."},
	{Name: "unquote", Doc: "Quasiquotation escape.\nSyntax: (unquote <expression>) or ,<expression>\nWithin a quasiquote template, evaluates <expression> and\ninserts its value. Only valid inside quasiquote. R7RS §4.2.8."},
	{Name: "unquote-splicing", Doc: "Quasiquotation splicing escape.\nSyntax: (unquote-splicing <expression>) or ,@<expression>\nWithin a quasiquote template, evaluates <expression> (which\nmust return a list) and splices its elements. R7RS §4.2.8."},
	{Name: "cond-expand", Doc: "Feature-based conditional expansion.\nSyntax: (cond-expand <clause1> <clause2> ...)\nEach clause is (<feature-requirement> <expression> ...) or\n(else <expression> ...). Expands the first clause whose feature\nrequirement is satisfied. R7RS §4.2.1."},
	{Name: "define-for-syntax", Doc: "Phase-1 definition.\nSyntax: (define-for-syntax <variable> <expression>)\nDefines <variable> in the expand-time environment."},
	{Name: "begin-for-syntax", Doc: "Phase-1 sequencing.\nSyntax: (begin-for-syntax <expression> ...)\nEvaluates expressions in the expand-time environment."},
	{Name: "eval-when", Doc: "Phase-conditional evaluation.\nSyntax: (eval-when (<phase> ...) <expression> ...)\nEvaluates expressions only in the specified phases."},
	{Name: "syntax-error", Doc: "Compile-time error.\nSyntax: (syntax-error <message> <irritant> ...)\nSignals a compile-time error with <message>. Typically used\nin syntax-rules templates for better error messages. R7RS §4.3.1."},
	{Name: "dynamic-wind", Doc: "Control flow with cleanup handlers.\nSyntax: (dynamic-wind <before> <thunk> <after>)\nCalls <before>, then <thunk>, then <after>. If control leaves\nor re-enters <thunk> via continuations, <after> and <before>\nare called accordingly. R7RS §6.10."},
	{Name: "apply", Doc: "Procedure application.\nSyntax: (apply <proc> <arg1> ... <args>)\nApplies <proc> to the list formed by prepending <arg1> ...\nto <args>. The last argument must be a list. R7RS §6.10."},
	{Name: "with-continuation-mark", Doc: "Continuation mark annotation.\nSyntax: (with-continuation-mark <key> <value> <expression>)\nEvaluates <expression> with a mark mapping <key> to <value>\nattached to the current continuation frame."},
	{Name: "let", Doc: "Local binding.\nSyntax: (let ((<var1> <init1>) ...) <body>)\n        (let <name> ((<var1> <init1>) ...) <body>)  ; named let\nBinds each <var> to the value of its <init> (evaluated in the\nenclosing environment), then evaluates <body>. Named let creates\na local procedure <name> for iteration. R7RS §4.2.2."},
	{Name: "let*", Doc: "Sequential local binding.\nSyntax: (let* ((<var1> <init1>) ...) <body>)\nLike let, but each <init> is evaluated in an environment that\nincludes the preceding bindings. R7RS §4.2.2."},
	{Name: "letrec", Doc: "Recursive local binding.\nSyntax: (letrec ((<var1> <init1>) ...) <body>)\nAll <var>s are bound before any <init> is evaluated.\nReferencing a <var> before its <init> completes is an error.\nR7RS §4.2.2."},
	{Name: "letrec*", Doc: "Sequential recursive local binding.\nSyntax: (letrec* ((<var1> <init1>) ...) <body>)\nLike letrec, but <init> expressions are evaluated left-to-right,\neach in an environment where the preceding variables are bound.\nR7RS §4.2.2."},
	{Name: "else", Doc: "Auxiliary syntax.\nUsed as a literal keyword in cond, case, guard, and cond-expand\nto denote the default clause. Not a procedure. R7RS §4.2.1."},
	{Name: "=>", Doc: "Auxiliary syntax.\nUsed in cond clauses: (<test> => <proc>) applies <proc> to the\nvalue of <test> when <test> is true. R7RS §4.2.1."},
	{Name: "syntax-rules", Doc: "Pattern-based macro transformer.\nSyntax: (syntax-rules (<literal> ...) <rule> ...)\nEach <rule> is (<pattern> <template>). Matches input against\npatterns and produces output from templates with hygienic\nsubstitution. R7RS §4.3.2."},
	{Name: "...", Doc: "Auxiliary syntax.\nEllipsis marker in syntax-rules patterns and templates.\nIn a pattern, matches zero or more repetitions of the preceding\nsubpattern. In a template, replicates the preceding subtemplate.\nR7RS §4.3.2."},
	{Name: "_", Doc: "Auxiliary syntax.\nWildcard in syntax-rules patterns. Matches any input form\nwithout binding it to a pattern variable. R7RS §4.3.2."},
}

func addSpecialForms(r *registry.Registry) error {
	r.AddBindingSpecs(compileTimeBindingSpecs)
	r.AddDocEntries(macroDocs)
	return nil
}
```

Wait — the method is `AddDocumentation(name, doc)`, not `AddDocEntries`. Let me use individual calls or add a batch method. Using a loop is simplest:

```go
func addSpecialForms(r *registry.Registry) error {
	r.AddBindingSpecs(compileTimeBindingSpecs)
	for _, doc := range macroDocs {
		r.AddDocumentation(doc.Name, doc.Doc)
	}
	return nil
}
```

**Step 2: Add bootstrap macro doc content**

Add after `compileTimeBindingSpecs` in `specialforms.go`:

```go
// macroDocs provides documentation for bootstrap macros defined in
// bootstrap_macros.scm. These are injected via ApplyDocs after bootstrap
// loading, since the expand-time bindings don't exist until define-syntax
// is evaluated.
var macroDocs = []registry.DocEntry{
	{Name: "and", Doc: "Boolean conjunction with short-circuit evaluation.\nSyntax: (and <test1> ...)\nEvaluates expressions left-to-right. Returns #f as soon as any\nexpression evaluates to #f. Returns the value of the last\nexpression if all are true, or #t if no expressions. R7RS §4.2.1."},
	{Name: "or", Doc: "Boolean disjunction with short-circuit evaluation.\nSyntax: (or <test1> ...)\nEvaluates expressions left-to-right. Returns the first true\nvalue without evaluating remaining expressions. Returns #f\nif all expressions evaluate to #f. R7RS §4.2.1."},
	{Name: "cond", Doc: "Multi-way conditional.\nSyntax: (cond <clause1> <clause2> ...)\nEach clause is (<test> <expression> ...), (<test> => <proc>),\nor (else <expression> ...). Evaluates each <test> in order;\nwhen one is true, evaluates its expressions and returns the\nlast value. R7RS §4.2.1."},
	{Name: "case", Doc: "Datum dispatch.\nSyntax: (case <key> <clause1> <clause2> ...)\nEach clause is ((<datum> ...) <expression> ...) or\n(else <expression> ...). Evaluates <key>, then compares it\nusing eqv? against each <datum>. R7RS §4.2.1."},
	{Name: "when", Doc: "One-armed conditional (true branch).\nSyntax: (when <test> <expression1> <expression2> ...)\nIf <test> is true, evaluates expressions in order and returns\nthe value of the last one. Otherwise returns unspecified.\nR7RS §4.2.1."},
	{Name: "unless", Doc: "One-armed conditional (false branch).\nSyntax: (unless <test> <expression1> <expression2> ...)\nIf <test> is false, evaluates expressions in order and returns\nthe value of the last one. Otherwise returns unspecified.\nR7RS §4.2.1."},
	{Name: "do", Doc: "Iteration.\nSyntax: (do ((<var> <init> <step>) ...) (<test> <expr> ...) <command> ...)\nBinds each <var> to <init>, then loops: if <test> is true,\nevaluates <expr>s and returns the last value. Otherwise\nevaluates <command>s for effect and updates each <var> to\nits <step> value. R7RS §4.2.4."},
	{Name: "guard", Doc: "Exception handling.\nSyntax: (guard (<var> <clause1> <clause2> ...) <body>)\nEvaluates <body>. If an exception is raised, binds it to <var>\nand evaluates clauses like cond. If no clause matches and no\nelse clause exists, re-raises the exception. R7RS §4.2.7."},
	{Name: "parameterize", Doc: "Dynamic binding.\nSyntax: (parameterize ((<param1> <value1>) ...) <body>)\nTemporarily rebinds parameter objects to new values for the\ndynamic extent of <body>. Restores original values on exit,\nincluding non-local exit via continuations. R7RS §4.2.6."},
	{Name: "delay", Doc: "Lazy evaluation.\nSyntax: (delay <expression>)\nCreates a promise that, when forced, evaluates <expression>\nand caches the result. R7RS §4.2.5."},
	{Name: "delay-force", Doc: "Iterative lazy evaluation.\nSyntax: (delay-force <expression>)\nLike delay, but <expression> must return a promise. Enables\ntail-recursive lazy algorithms by avoiding nested promise\nwrappers. R7RS §4.2.5."},
	{Name: "define-record-type", Doc: "Record type definition.\nSyntax: (define-record-type <name> (<constructor> <field-name> ...)\n  <pred> <field-spec> ...)\nDefines a new record type with a constructor, predicate, and\nfield accessors/mutators. R7RS §5.5."},
	{Name: "let-values", Doc: "Multiple-value binding.\nSyntax: (let-values (((<var> ...) <init>) ...) <body>)\nLike let, but each <init> may return multiple values which\nare bound to the corresponding <var>s. R7RS §4.2.2."},
	{Name: "let*-values", Doc: "Sequential multiple-value binding.\nSyntax: (let*-values (((<var> ...) <init>) ...) <body>)\nLike let-values, but bindings are sequential: each <init>\nis evaluated in an environment with preceding bindings visible.\nR7RS §4.2.2."},
	{Name: "define-values", Doc: "Multiple-value definition.\nSyntax: (define-values <formals> <expression>)\nDefines multiple variables from a single expression that\nreturns multiple values. <formals> follows the same syntax\nas lambda formals. R7RS §5.3.3."},
}
```

**Step 3: Run tests**

Run: `go test ./registry/core/... -count=1`
Expected: PASS

**Step 4: Run full test suite**

Run: `go test ./... -count=1 -short`
Expected: PASS

**Step 5: Commit**

```
docs(core): add docstrings for special forms and bootstrap macros
```

---

### Task 6: Display doc content in `,doc` output

**Files:**
- Modify: `internal/repl/meta.go`

**Step 1: Update `formatBindingDoc`**

In `internal/repl/meta.go`, replace `formatBindingDoc` (lines 275-289):

```go
func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int) {
	phaseName := phaseLabel(phase)

	switch bnd.BindingType() {
	case environment.BindingTypePrimitive:
		fmt.Fprintf(w, "%s: special form (%s)\n", name, phaseName)
	case environment.BindingTypeSyntax:
		fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)
	case environment.BindingTypeVariable:
		val := bnd.Value()
		fmt.Fprintf(w, "%s: %s (%s)\n", name, val.SchemeString(), phaseName)
	default:
		fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
	}

	if doc := bnd.Doc(); doc != "" {
		fmt.Fprintf(w, "\n  %s\n", doc)
	}
}
```

**Step 2: Run repl tests**

Run: `go test ./internal/repl/... -count=1`
Expected: PASS

**Step 3: Commit**

```
feat(repl): display binding docs in ,doc output
```

---

### Task 7: Lint and final verification

**Step 1: Run linter**

Run: `make lint`
Expected: PASS

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: PASS

**Step 3: Run full test suite**

Run: `make test`
Expected: PASS

**Step 4: Manual smoke test**

Build and test in REPL:

```bash
make build && ./dist/darwin/arm64/wile
> ,doc if
> ,doc let
> ,doc and
> ,doc cond
> ,doc else
> ,doc ...
```

Verify each shows the type label, phase, and doc content.

**Step 5: Commit any fixups if needed**

```
fix: address lint/test issues from special form docs
```

---

### Task 8: Update TODO.md

**Files:**
- Modify: `TODO.md`

Mark the "Docstrings for special forms and macros" item as done. Update the parent doc system item's "Remaining gaps" summary.

**Commit:**

```
docs: mark special form/macro docstrings as complete in TODO
```
