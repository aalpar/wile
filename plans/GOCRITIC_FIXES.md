# gocritic Linter Fixes

This document tracks the fixes for 50 gocritic warnings from golangci-lint.

## Summary

| Category | Count | Priority | Risk | Status |
|----------|-------|----------|------|--------|
| ifElseChain | 19 | Low | Low | Remaining (case-by-case review needed) |
| sprintfQuotedString | 9 | Low | None | ✅ Fixed |
| evalOrder | 8 | Medium | Low | ✅ Fixed |
| underef | 5 | Low | None | ✅ Fixed |
| appendCombine | 4 | Low | None | ✅ Fixed |
| sloppyTypeAssert | 2 | Low | None | ✅ Fixed |
| singleCaseSwitch | 1 | Low | None | ✅ Fixed |
| deprecatedComment | 1 | Low | None | ✅ Fixed |
| deferInLoop | 1 | High | Medium | ✅ Fixed |

**Progress**: 31 of 50 warnings fixed (62% reduction). Remaining 19 are all ifElseChain warnings.

## 1. deferInLoop (1 warning) - High Priority

**Issue**: Defer in loop causes resource leak - file handles accumulate until loop completes.

**Location**: `go/machine/compile_time_continuation.go:236`

**Current Code**:
```go
for _, path := range paths {
    file, err := os.Open(path)
    if err != nil { ... }
    defer file.Close() //nolint:errcheck
    // ... process file
}
```

**Fix**: Move defer outside loop by using a closure:
```go
for _, path := range paths {
    err := func() error {
        file, err := os.Open(path)
        if err != nil {
            return err
        }
        defer file.Close()
        // ... process file
        return nil
    }()
    if err != nil { ... }
}
```

**Impact**: Prevents resource leak when processing multiple files.

## 2. evalOrder (8 warnings) - Medium Priority

**Issue**: Error method called on return value before being assigned. While semantically correct, evaluation order is unclear.

**Locations**:
- `go/machine/operation_apply.go:57, 90, 95, 113, 125`
- `go/machine/operation_make_case_lambda_closure.go:43`
- `go/machine/operation_push_wind.go:45, 52`

**Current Pattern**:
```go
return mc, mc.Error("message")
```

**Fix**:
```go
err := mc.Error("message")
return mc, err
```

**Impact**: Improves code clarity with no semantic change.

## 3. sprintfQuotedString (9 warnings) - Low Priority

**Issue**: Using `"%s"` for quoted strings instead of `%q` which auto-quotes and escapes.

**Locations**: All in `go/registry/core/prim_*_test.go` files:
- `prim_delete_load_test.go` (7 instances)
- `prim_open_binary_input_file_test.go` (1)
- `prim_open_binary_output_file_test.go` (1)

**Current Pattern**:
```go
code := fmt.Sprintf(`(load "%s")`, filename)
```

**Fix**:
```go
code := fmt.Sprintf(`(load %q)`, filename)
```

**Impact**: Cleaner code, handles escaping automatically. No semantic change since these are test files with known-good filenames.

## 4. appendCombine (4 warnings) - Low Priority

**Issue**: Multiple sequential appends can be combined into one.

**Locations**:
- `go/machine/machine_context_test.go:514, 532`
- `go/machine/machine_continuation_test.go:188`
- `go/machine/operation_test.go:848`

**Current Pattern**:
```go
tpl.operations = append(tpl.operations, NewOperationLoadVoid())
tpl.operations = append(tpl.operations, NewOperationReturn())
```

**Fix**:
```go
tpl.operations = append(tpl.operations,
    NewOperationLoadVoid(),
    NewOperationReturn())
```

**Impact**: Minor efficiency improvement, clearer intent.

## 5. underef (5 warnings) - Low Priority

**Issue**: Unnecessary pointer dereference - Go auto-dereferences for method calls.

**Locations**:
- `go/machine/stack_test.go:287, 291, 300`
- `go/values/byte_vector.go:39, 67`

**Current Pattern**:
```go
(*s).IsVoid()
(*vs[i]).Value
```

**Fix**:
```go
s.IsVoid()
vs[i].Value
```

**Impact**: Cleaner code, no semantic change.

## 6. sloppyTypeAssert (2 warnings) - Low Priority

**Issue**: Type assertion where source and target types are identical.

**Locations**:
- `go/match/syntax_adapter.go:716`
- `go/parser/parser.go:614`

**Example**:
```go
car = carVal.(values.Value)  // carVal is already values.Value
```

**Fix**: Remove the type assertion - just use the value directly.

**Impact**: Cleaner code, removes unnecessary assertion.

## 7. ifElseChain (19 warnings) - Low Priority

**Issue**: Long if-else-if chains that could be switch statements.

**Locations**:
- Production: `go/extensions/gointerop/prim_gointerop.go:98`, `go/extensions/math/prim_math.go:1332`, `go/match/match.go:287,517,721`, `go/tokenizer/tokenizer.go:1741`, `go/values/array_list.go:65`, `go/values/pair.go:255`
- Test: `go/parser/parser_test.go:1843,1852,2283,2336`, `go/registry/core/prim_identifier_test.go:161`, `go/values/numeric_tower_coverage_test.go:105,131,157,183,242,293`

**Approach**: Review each case individually. Some if-else chains are clearer than switch (e.g., type checks with different operations). Only convert where switch improves readability.

**Impact**: Case-by-case analysis needed. Some conversions improve clarity, others may not.

## 8. singleCaseSwitch (1 warning) - Low Priority

**Issue**: Switch with only one case should be an if statement.

**Location**: `go/syntax/utils.go:104`

**Current**:
```go
switch pr := v.(type) {
case *SyntaxPair:
    // ... handle
}
```

**Fix**:
```go
if pr, ok := v.(*SyntaxPair); ok {
    // ... handle
}
```

**Impact**: Cleaner code for single-case scenario.

## 9. deprecatedComment (1 warning) - Low Priority

**Issue**: Deprecated notice should be in a separate paragraph.

**Location**: `go/syntax/syntax_value.go:70`

**Current**:
```go
// Deprecated: Use NewSyntaxEmptyList instead. This function exists for backward
// compatibility.
```

**Fix**:
```go
// Deprecated: Use NewSyntaxEmptyList instead.
//
// This function exists for backward compatibility.
```

**Impact**: Better doc formatting.

## Implementation Order

1. **deferInLoop** (high priority, resource leak)
2. **evalOrder** (medium priority, clarity improvement)
3. **underef** (simple, no risk)
4. **appendCombine** (simple, no risk)
5. **sprintfQuotedString** (simple, test files only)
6. **sloppyTypeAssert** (simple, remove unnecessary code)
7. **singleCaseSwitch** (simple, clarity)
8. **deprecatedComment** (trivial)
9. **ifElseChain** (case-by-case, lower priority)

## Testing

- All fixes should pass `make test` with no regressions
- Verify `make lint` reduces gocritic count from 50 to 0
- Focus initial testing on deferInLoop and evalOrder changes (highest risk)
