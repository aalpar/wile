# Go SSA Extension — Phase 1B+1C Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Complete the `(wile goast ssa)` instruction mapper by adding 24 remaining SSA types from Phase 1B (collections, channels, goroutines, iteration) and Phase 1C (type ops, interfaces, closures).

**Architecture:** All changes are in `extensions/goastssa/mapper.go`. The pattern is identical to Phase 1A: add a `case *ssa.Foo:` to `mapInstruction`, implement a `mapFoo` method, write a test in `mapper_test.go`. No new files, no new packages.

**Tech Stack:** `golang.org/x/tools/go/ssa` (already vendored via `golang.org/x/tools v0.42.0`)

**Design doc:** `plans/GO-STATIC-ANALYSIS.md` (Phase 1B/1C sub-phase status → "Complete" when done)

**Reference:** `plans/GO-SSA-PHASE-1A.md` — established patterns. `extensions/goastssa/mapper.go` — current type switch and mapper methods.

---

### Task 1: Map operations (MakeMap, MapUpdate, Lookup, Extract)

`Extract` is included here because it is produced alongside `Lookup` when `commaok=true` splits a tuple result.

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

Add to `mapper_test.go`:

```go
func TestMapMakeMapAndLookup(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UseMap() (int, bool) {
	m := make(map[string]int)
	m["key"] = 42
	v, ok := m["key"]
	return v, ok
}
`, "UseMap")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	// MakeMap
	mkMap := findNodeByTag(result, "ssa-make-map")
	c.Assert(mkMap, qt.IsNotNil, qt.Commentf("expected ssa-make-map"))

	// MapUpdate
	mu := findNodeByTag(result, "ssa-map-update")
	c.Assert(mu, qt.IsNotNil, qt.Commentf("expected ssa-map-update"))

	// Lookup
	lk := findNodeByTag(result, "ssa-lookup")
	c.Assert(lk, qt.IsNotNil, qt.Commentf("expected ssa-lookup"))

	commaOk, ok := goast.GetField(lk.(*values.Pair).Cdr(), "comma-ok")
	c.Assert(ok, qt.IsTrue)
	c.Assert(commaOk, qt.Equals, values.TrueValue)

	// Extract (from the commaok tuple)
	ex := findNodeByTag(result, "ssa-extract")
	c.Assert(ex, qt.IsNotNil, qt.Commentf("expected ssa-extract from commaok lookup"))
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapMakeMapAndLookup`
Expected: FAIL — `ssa-make-map` not found (falls through to `ssa-unknown`).

**Step 3: Add cases to mapInstruction and implement mappers**

In `mapper.go`, add to the type switch (after the existing `case *ssa.Return:`):

```go
case *ssa.MakeMap:
	return p.mapMakeMap(v)
case *ssa.MapUpdate:
	return p.mapMapUpdate(v)
case *ssa.Lookup:
	return p.mapLookup(v)
case *ssa.Extract:
	return p.mapExtract(v)
```

Add mapper methods (before `mapUnknown`):

```go
func (p *ssaMapper) mapMakeMap(v *ssa.MakeMap) values.Value {
	return goast.Node("ssa-make-map",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("reserve", valName(v.Reserve)),
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapMapUpdate(v *ssa.MapUpdate) values.Value {
	return goast.Node("ssa-map-update",
		goast.Field("map", valName(v.Map)),
		goast.Field("key", valName(v.Key)),
		goast.Field("val", valName(v.Value)),
		goast.Field("operands", goast.ValueList([]values.Value{
			valName(v.Map), valName(v.Key), valName(v.Value),
		})),
	)
}

func (p *ssaMapper) mapLookup(v *ssa.Lookup) values.Value {
	return goast.Node("ssa-lookup",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("index", valName(v.Index)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("comma-ok", values.BoolToBoolean(v.CommaOk)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Index)})),
	)
}

func (p *ssaMapper) mapExtract(v *ssa.Extract) values.Value {
	return goast.Node("ssa-extract",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("tup", valName(v.Tuple)),
		goast.Field("index", values.NewInteger(int64(v.Index))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Tuple)})),
	)
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastssa/... -run TestMapMakeMapAndLookup`
Expected: PASS.

**Step 5: Run full test suite**

Run: `go test -v ./extensions/goastssa/...`
Expected: PASS.

**Step 6: Run lint**

Run: `make lint`
Expected: Clean.

**Step 7: Commit**

```
feat(goastssa): map map operations (MakeMap, MapUpdate, Lookup, Extract)

MakeMap includes a reserve field (capacity hint; #f if absent).
Lookup includes comma-ok boolean for multi-value map reads.
Extract is the tuple-element projection produced by commaok returns.
```

---

### Task 2: Slice operations (MakeSlice, Slice)

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

```go
func TestMapMakeSliceAndSlice(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UseSlice() []int {
	s := make([]int, 5)
	return s[1:3]
}
`, "UseSlice")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	ms := findNodeByTag(result, "ssa-make-slice")
	c.Assert(ms, qt.IsNotNil, qt.Commentf("expected ssa-make-slice"))

	sl := findNodeByTag(result, "ssa-slice")
	c.Assert(sl, qt.IsNotNil, qt.Commentf("expected ssa-slice"))

	xField, ok := goast.GetField(sl.(*values.Pair).Cdr(), "x")
	c.Assert(ok, qt.IsTrue)
	c.Assert(xField, qt.Not(qt.Equals), values.FalseValue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapMakeSliceAndSlice`

**Step 3: Add cases and implement mappers**

In the type switch:

```go
case *ssa.MakeSlice:
	return p.mapMakeSlice(v)
case *ssa.Slice:
	return p.mapSlice(v)
```

Mapper methods:

```go
func (p *ssaMapper) mapMakeSlice(v *ssa.MakeSlice) values.Value {
	return goast.Node("ssa-make-slice",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("len", valName(v.Len)),
		goast.Field("cap", valName(v.Cap)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Len), valName(v.Cap)})),
	)
}

func (p *ssaMapper) mapSlice(v *ssa.Slice) values.Value {
	return goast.Node("ssa-slice",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("low", valName(v.Low)),
		goast.Field("high", valName(v.High)),
		goast.Field("max", valName(v.Max)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}
```

Note: `Low`, `High`, `Max` are nil when absent; `valName(nil)` returns `#f`.

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastssa/... -run TestMapMakeSliceAndSlice`
Expected: PASS.

**Step 5: Commit**

```
feat(goastssa): map slice operations (MakeSlice, Slice)

Slice includes low/high/max fields (#f when absent) for both
2-index and 3-index slice expressions.
```

---

### Task 3: Channel operations (MakeChan, Send, Select)

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

```go
func TestMapChannels(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	t.Run("MakeChan and Send", func(t *testing.T) {
		c := qt.New(t)
		fn := buildSSAFromSource(t, dir, `
package testpkg

func UseChan() {
	ch := make(chan int, 1)
	ch <- 42
}
`, "UseChan")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		mc := findNodeByTag(result, "ssa-make-chan")
		c.Assert(mc, qt.IsNotNil, qt.Commentf("expected ssa-make-chan"))

		sn := findNodeByTag(result, "ssa-send")
		c.Assert(sn, qt.IsNotNil, qt.Commentf("expected ssa-send"))
	})

	t.Run("Select", func(t *testing.T) {
		c := qt.New(t)
		dir2 := t.TempDir()
		fn := buildSSAFromSource(t, dir2, `
package testpkg

func UseSelect(c1, c2 chan int) int {
	select {
	case v := <-c1:
		return v
	case v := <-c2:
		return v
	}
}
`, "UseSelect")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		sel := findNodeByTag(result, "ssa-select")
		c.Assert(sel, qt.IsNotNil, qt.Commentf("expected ssa-select"))

		states, ok := goast.GetField(sel.(*values.Pair).Cdr(), "states")
		c.Assert(ok, qt.IsTrue)
		c.Assert(listLength(states), qt.Equals, 2)
	})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapChannels`

**Step 3: Add cases and implement mappers**

In the type switch:

```go
case *ssa.MakeChan:
	return p.mapMakeChan(v)
case *ssa.Send:
	return p.mapSend(v)
case *ssa.Select:
	return p.mapSelect(v)
```

Mapper methods:

```go
func (p *ssaMapper) mapMakeChan(v *ssa.MakeChan) values.Value {
	return goast.Node("ssa-make-chan",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("size", valName(v.Size)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Size)})),
	)
}

func (p *ssaMapper) mapSend(v *ssa.Send) values.Value {
	return goast.Node("ssa-send",
		goast.Field("chan", valName(v.Chan)),
		goast.Field("x", valName(v.X)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Chan), valName(v.X)})),
	)
}

func (p *ssaMapper) mapSelect(v *ssa.Select) values.Value {
	states := make([]values.Value, len(v.States))
	var operands []values.Value
	for i, s := range v.States {
		stateFields := []values.Value{
			goast.Field("chan", valName(s.Chan)),
			goast.Field("dir", goast.Sym(chanDirStr(s.Dir))),
		}
		operands = append(operands, valName(s.Chan))
		if s.Send != nil {
			stateFields = append(stateFields, goast.Field("send", valName(s.Send)))
			operands = append(operands, valName(s.Send))
		}
		states[i] = goast.Node("ssa-select-state", stateFields...)
	}
	return goast.Node("ssa-select",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("blocking", values.BoolToBoolean(v.Blocking)),
		goast.Field("states", goast.ValueList(states)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList(operands)),
	)
}

// chanDirStr converts a channel direction to a Scheme symbol string.
func chanDirStr(dir types.ChanDir) string {
	switch dir {
	case types.SendOnly:
		return "send"
	case types.RecvOnly:
		return "recv"
	default:
		return "both"
	}
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/... -run TestMapChannels`
Expected: PASS.

**Step 5: Commit**

```
feat(goastssa): map channel operations (MakeChan, Send, Select)

Select renders each SelectState as (ssa-select-state (chan ...) (dir send|recv))
with an optional send field for send states. Blocking field indicates
whether the select has a default case.
```

---

### Task 4: Goroutines, defers, iteration, panic (Go, Defer, RunDefers, Range, Next, Panic)

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing tests**

```go
func TestMapGoroutineAndDefer(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func noop() {
}

func UseGoDefer() {
	go noop()
	defer noop()
}
`, "UseGoDefer")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	goNode := findNodeByTag(result, "ssa-go")
	c.Assert(goNode, qt.IsNotNil, qt.Commentf("expected ssa-go"))

	deferNode := findNodeByTag(result, "ssa-defer")
	c.Assert(deferNode, qt.IsNotNil, qt.Commentf("expected ssa-defer"))

	// RunDefers is inserted at the return site when defers are present.
	rdNode := findNodeByTag(result, "ssa-run-defers")
	c.Assert(rdNode, qt.IsNotNil, qt.Commentf("expected ssa-run-defers"))
}

func TestMapRangeAndNext(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UseRange(m map[string]int) int {
	sum := 0
	for _, v := range m {
		sum += v
	}
	return sum
}
`, "UseRange")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	rn := findNodeByTag(result, "ssa-range")
	c.Assert(rn, qt.IsNotNil, qt.Commentf("expected ssa-range"))

	nx := findNodeByTag(result, "ssa-next")
	c.Assert(nx, qt.IsNotNil, qt.Commentf("expected ssa-next"))
}

func TestMapPanic(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UsePanic(x int) int {
	if x < 0 {
		panic("negative")
	}
	return x
}
`, "UsePanic")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	pn := findNodeByTag(result, "ssa-panic")
	c.Assert(pn, qt.IsNotNil, qt.Commentf("expected ssa-panic"))
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastssa/... -run 'TestMapGoroutineAndDefer|TestMapRangeAndNext|TestMapPanic'`

**Step 3: Add cases and implement mappers**

In the type switch:

```go
case *ssa.Go:
	return p.mapGo(v)
case *ssa.Defer:
	return p.mapDefer(v)
case *ssa.RunDefers:
	return p.mapRunDefers(v)
case *ssa.Range:
	return p.mapRange(v)
case *ssa.Next:
	return p.mapNext(v)
case *ssa.Panic:
	return p.mapPanic(v)
```

Mapper methods:

```go
func (p *ssaMapper) mapGo(v *ssa.Go) values.Value {
	fields := p.mapCallCommon(&v.Call)
	return goast.Node("ssa-go", fields...)
}

func (p *ssaMapper) mapDefer(v *ssa.Defer) values.Value {
	fields := p.mapCallCommon(&v.Call)
	return goast.Node("ssa-defer", fields...)
}

func (p *ssaMapper) mapRunDefers(_ *ssa.RunDefers) values.Value {
	return goast.Node("ssa-run-defers",
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapRange(v *ssa.Range) values.Value {
	return goast.Node("ssa-range",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapNext(v *ssa.Next) values.Value {
	return goast.Node("ssa-next",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("iter", valName(v.Iter)),
		goast.Field("is-string", values.BoolToBoolean(v.IsString)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Iter)})),
	)
}

func (p *ssaMapper) mapPanic(v *ssa.Panic) values.Value {
	return goast.Node("ssa-panic",
		goast.Field("x", valName(v.X)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/... -run 'TestMapGoroutineAndDefer|TestMapRangeAndNext|TestMapPanic'`
Expected: PASS.

**Step 5: Run lint**

Run: `make lint`

**Step 6: Commit**

```
feat(goastssa): map goroutines, defers, iteration, panic (Go, Defer, RunDefers, Range, Next, Panic)

Go and Defer reuse mapCallCommon — same call encoding as ssa-call
but wrapped in ssa-go/ssa-defer. Range produces an iterator value;
Next consumes it with an is-string flag for string ranging.
```

---

### Task 5: Type conversions (ChangeType, Convert, ChangeInterface, SliceToArrayPointer)

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing tests**

```go
func TestMapTypeConversions(t *testing.T) {
	t.Run("Convert numeric", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

func ToInt64(x int) int64 {
	return int64(x)
}
`, "ToInt64")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		cv := findNodeByTag(result, "ssa-convert")
		c.Assert(cv, qt.IsNotNil, qt.Commentf("expected ssa-convert"))
	})

	t.Run("ChangeType channel direction", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

func SendOnly(c chan int) chan<- int {
	return c
}
`, "SendOnly")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		ct := findNodeByTag(result, "ssa-change-type")
		c.Assert(ct, qt.IsNotNil, qt.Commentf("expected ssa-change-type"))
	})

	t.Run("SliceToArrayPointer", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

func SliceToArr(s []int) *[3]int {
	return (*[3]int)(s)
}
`, "SliceToArr")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		sap := findNodeByTag(result, "ssa-slice-to-array-ptr")
		c.Assert(sap, qt.IsNotNil, qt.Commentf("expected ssa-slice-to-array-ptr"))
	})

	t.Run("ChangeInterface", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

type Stringer interface {
	String() string
}

type ReadStringer interface {
	String() string
	Read() []byte
}

func ToStringer(x ReadStringer) Stringer {
	return x
}
`, "ToStringer")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		// ChangeInterface may be elided by the SSA compiler in some cases.
		// If absent, verify by dumping SSA with ssa.WriteFunction and
		// adjusting the test source.
		ci := findNodeByTag(result, "ssa-change-interface")
		c.Assert(ci, qt.IsNotNil,
			qt.Commentf("expected ssa-change-interface; if absent, verify SSA output"))
	})
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastssa/... -run TestMapTypeConversions`

**Step 3: Add cases and implement mappers**

In the type switch:

```go
case *ssa.ChangeType:
	return p.mapChangeType(v)
case *ssa.Convert:
	return p.mapConvert(v)
case *ssa.ChangeInterface:
	return p.mapChangeInterface(v)
case *ssa.SliceToArrayPointer:
	return p.mapSliceToArrayPointer(v)
```

Mapper methods:

```go
func (p *ssaMapper) mapChangeType(v *ssa.ChangeType) values.Value {
	return goast.Node("ssa-change-type",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapConvert(v *ssa.Convert) values.Value {
	return goast.Node("ssa-convert",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapChangeInterface(v *ssa.ChangeInterface) values.Value {
	return goast.Node("ssa-change-interface",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapSliceToArrayPointer(v *ssa.SliceToArrayPointer) values.Value {
	return goast.Node("ssa-slice-to-array-ptr",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/... -run TestMapTypeConversions`
Expected: PASS (all 3 subtests).

**Step 5: Commit**

```
feat(goastssa): map type conversions (ChangeType, Convert, ChangeInterface, SliceToArrayPointer)

All four follow the same shape: name, x, type, operands=[x].
ChangeType covers value-preserving changes (channel direction, named types).
Convert covers numeric/string/unsafe conversions.
```

---

### Task 6: Interfaces and closures (MakeInterface, TypeAssert, MakeClosure, MultiConvert, DebugRef)

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing tests**

```go
func TestMapMakeInterface(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func ToInterface(x int) interface{} {
	return x
}
`, "ToInterface")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	mi := findNodeByTag(result, "ssa-make-interface")
	c.Assert(mi, qt.IsNotNil, qt.Commentf("expected ssa-make-interface"))
}

func TestMapTypeAssert(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func FromInterface(x interface{}) int {
	return x.(int)
}
`, "FromInterface")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	ta := findNodeByTag(result, "ssa-type-assert")
	c.Assert(ta, qt.IsNotNil, qt.Commentf("expected ssa-type-assert"))

	asserted, ok := goast.GetField(ta.(*values.Pair).Cdr(), "asserted-type")
	c.Assert(ok, qt.IsTrue)
	c.Assert(asserted.(*values.String).Value, qt.Equals, "int")
}

func TestMapMakeClosure(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func MakeClosure(x int) func() int {
	return func() int {
		return x
	}
}
`, "MakeClosure")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	cl := findNodeByTag(result, "ssa-make-closure")
	c.Assert(cl, qt.IsNotNil, qt.Commentf("expected ssa-make-closure"))

	bindings, ok := goast.GetField(cl.(*values.Pair).Cdr(), "bindings")
	c.Assert(ok, qt.IsTrue)
	// x is captured, so at least one binding.
	c.Assert(listLength(bindings) >= 1, qt.IsTrue)
}
```

**Note on MultiConvert test:** MultiConvert appears only in generic (type-parameterized) code. The test below uses a generic function to trigger it. If the SSA compiler optimizes it to a regular Convert, verify by dumping SSA with `ssa.WriteFunction` and adjust the test source or skip the assertion.

**Note on DebugRef test:** DebugRef is a pseudo-instruction that only appears when SSA is built with `ssa.GlobalDebug`. The test below uses a dedicated `buildSSAFromSourceDebug` helper that adds this flag.

```go
func TestMapMultiConvert(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func ToInt64[T ~int | ~float64](x T) int64 {
	return int64(x)
}
`, "ToInt64")

	// MultiConvert may or may not appear depending on SSA compiler decisions.
	// If it does, verify the tag; if not, the instruction falls through to
	// ssa-unknown or ssa-convert, which is acceptable.
	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	mc := findNodeByTag(result, "ssa-multi-convert")
	if mc == nil {
		// SSA compiler may have lowered this to a regular Convert.
		cv := findNodeByTag(result, "ssa-convert")
		c.Assert(cv, qt.IsNotNil,
			qt.Commentf("expected either ssa-multi-convert or ssa-convert"))
		return
	}
	xField, ok := goast.GetField(mc.(*values.Pair).Cdr(), "x")
	c.Assert(ok, qt.IsTrue)
	c.Assert(xField, qt.Not(qt.Equals), values.FalseValue)
}

// buildSSAFromSourceDebug is like buildSSAFromSource but builds with
// ssa.GlobalDebug to produce DebugRef instructions.
func buildSSAFromSourceDebug(t *testing.T, dir, source, funcName string) *ssa.Function {
	t.Helper()
	c := qt.New(t)
	writeTestPackage(t, dir, source)

	fset := token.NewFileSet()
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedSyntax |
			packages.NeedTypes | packages.NeedTypesInfo |
			packages.NeedImports | packages.NeedDeps,
		Fset: fset,
		Dir:  dir,
	}
	pkgs, err := packages.Load(cfg, ".")
	c.Assert(err, qt.IsNil)
	c.Assert(len(pkgs), qt.Not(qt.Equals), 0)

	_, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions|ssa.GlobalDebug)
	for _, p := range ssaPkgs {
		if p != nil {
			p.Build()
		}
	}

	fn := ssaPkgs[0].Func(funcName)
	c.Assert(fn, qt.IsNotNil, qt.Commentf("function %s not found", funcName))
	return fn
}

func TestMapDebugRef(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSourceDebug(t, dir, `
package testpkg

func UseDebug(x int) int {
	y := x + 1
	return y
}
`, "UseDebug")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	dr := findNodeByTag(result, "ssa-debug-ref")
	c.Assert(dr, qt.IsNotNil, qt.Commentf("expected ssa-debug-ref with GlobalDebug"))
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastssa/... -run 'TestMapMakeInterface|TestMapTypeAssert|TestMapMakeClosure|TestMapMultiConvert|TestMapDebugRef'`

**Step 3: Add cases and implement mappers**

In the type switch:

```go
case *ssa.MakeInterface:
	return p.mapMakeInterface(v)
case *ssa.TypeAssert:
	return p.mapTypeAssert(v)
case *ssa.MakeClosure:
	return p.mapMakeClosure(v)
case *ssa.MultiConvert:
	return p.mapMultiConvert(v)
case *ssa.DebugRef:
	return p.mapDebugRef(v)
```

Mapper methods:

```go
func (p *ssaMapper) mapMakeInterface(v *ssa.MakeInterface) values.Value {
	return goast.Node("ssa-make-interface",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapTypeAssert(v *ssa.TypeAssert) values.Value {
	return goast.Node("ssa-type-assert",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("asserted-type", goast.Str(types.TypeString(v.AssertedType, nil))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("comma-ok", values.BoolToBoolean(v.CommaOk)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapMakeClosure(v *ssa.MakeClosure) values.Value {
	bindings := make([]values.Value, len(v.Bindings))
	operands := make([]values.Value, len(v.Bindings))
	for i, b := range v.Bindings {
		bindings[i] = valName(b)
		operands[i] = valName(b)
	}
	return goast.Node("ssa-make-closure",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("fn", valName(v.Fn)),
		goast.Field("bindings", goast.ValueList(bindings)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList(operands)),
	)
}

// mapMultiConvert handles generic multi-type conversions (same shape as Convert).
func (p *ssaMapper) mapMultiConvert(v *ssa.MultiConvert) values.Value {
	return goast.Node("ssa-multi-convert",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

// mapDebugRef handles source-level debug annotations.
// Only appears when SSA is built with ssa.GlobalDebug; included for completeness.
func (p *ssaMapper) mapDebugRef(v *ssa.DebugRef) values.Value {
	return goast.Node("ssa-debug-ref",
		goast.Field("x", valName(v.X)),
		goast.Field("is-addr", values.BoolToBoolean(v.IsAddr)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/... -run 'TestMapMakeInterface|TestMapTypeAssert|TestMapMakeClosure|TestMapMultiConvert|TestMapDebugRef'`
Expected: PASS.

**Step 5: Run full suite + lint + covercheck**

Run: `make lint && go test ./extensions/goastssa/... && make covercheck`
Expected: All pass, coverage ≥ 80%.

**Step 6: Commit**

```
feat(goastssa): map interfaces, closures, and extras (MakeInterface, TypeAssert, MakeClosure, MultiConvert, DebugRef)

MakeClosure includes bindings (free variable captures) as operands
for closure data-flow analysis. TypeAssert includes asserted-type
and comma-ok for both panicking and non-panicking assertions.
MultiConvert and DebugRef are included for completeness; the former
appears in generic code, the latter only in debug builds.
```

---

### Task 7: Full verification and plan update

**Step 1: Run full test suite**

Run: `make lint && make test`
Expected: All packages pass.

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: `extensions/goastssa` ≥ 80%.

**Step 3: Update plan status**

In `plans/GO-STATIC-ANALYSIS.md`, mark both sub-phases complete:

```
#### Sub-phase 1B: Collections + concurrency ✓ Complete
#### Sub-phase 1C: Type operations + closures ✓ Complete
```

**Step 4: Commit**

```
docs: mark GO-STATIC-ANALYSIS Phase 1B+1C complete
```

---

## Post-implementation checklist

- [x] All 6 task commits on branch
- [x] `make lint` clean
- [x] `make test` passes
- [x] `make covercheck` passes
- [x] `plans/GO-STATIC-ANALYSIS.md` updated: Phase 1B + 1C → "Complete"
- [x] All 20 new instruction types have named mapper methods (no anonymous fallthrough)
- [x] ssa-unknown with go-type diagnostic still works for any future instruction types

## Instruction type coverage after Phase 1BC

Phase 1A: BinOp, UnOp, Alloc, Call, Store, FieldAddr, Field, IndexAddr, Index, Phi, If, Jump, Return
Phase 1B: MakeMap, MapUpdate, Lookup, Extract, MakeSlice, Slice, MakeChan, Send, Select, Go, Defer, RunDefers, Range, Next, Panic
Phase 1C: ChangeType, Convert, ChangeInterface, SliceToArrayPointer, MakeInterface, TypeAssert, MakeClosure, MultiConvert, DebugRef

Total: 37 instruction types explicitly mapped.
