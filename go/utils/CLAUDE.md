# CLAUDE.md

Package `utils` provides conversion utilities between syntax and datum values.

## Purpose

Bridge between:
- **Datum**: Raw Scheme values without metadata
- **Syntax**: Values with source location and scope information
- **Go types**: Boolean conversions

## Key Functions

**SyntaxValueToDatum(sv values.Value) values.Value**
- Converts syntax objects back to raw datums
- Strips source location and scope information
- Uses iteration (not recursion) for lists to avoid stack overflow

**DatumToSyntaxValue(sctx *SourceContext, o values.Value) syntax.SyntaxValue**
- Wraps raw datums in syntax objects
- Attaches source context for location/scope tracking
- Returns existing SyntaxValue unchanged (no double-wrapping)

**AsList(items []values.Value) *values.Pair**
- Converts Go slices to Scheme linked lists
- Returns `nil` for empty slices (not EmptyList)

**BoolToBoolean(b bool) *values.Boolean**
- Go bool to Scheme boolean singleton

**ValueToBool(v values.Value) bool**
- Scheme value to Go bool using Scheme semantics
- Returns `false` only if v is `#f`, `true` for everything else
- Use for truthiness checks in control flow

**ValueToBoolean(v values.Value) *values.Boolean**
- Scheme value to Scheme boolean using Scheme semantics
- Returns `FalseValue` only if v is `#f`, `TrueValue` for everything else
- Equivalent to `BoolToBoolean(ValueToBool(v))`

**IsSyntaxComment(v values.Value) bool**
- Identifies SyntaxComment or SyntaxDatumComment types

## Gotchas

- **Stack overflow prevention**: `SyntaxValueToDatum` uses iteration for lists
- **AsList returns nil**: Empty slice returns `nil`, not `EmptyList`
- **Improper list handling**: Uses ForEach iterator, returns remaining tail
- **SyntaxValue pass-through**: Already-wrapped values returned unchanged
- **Box contents wrapped**: Boxing wraps contents in syntax too
- **Boolean conversion choice**:
  - `BoolToBoolean(goCondition)` - for predicates returning based on Go conditions
  - `ValueToBool(schemeValue)` - for checking Scheme value truthiness in Go control flow
  - `ValueToBoolean(schemeValue)` - for coercing Scheme value to Scheme boolean

## Testing

Uses quicktest with suite-based tests covering conversions, round-trips, and edge cases.
