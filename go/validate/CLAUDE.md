# CLAUDE.md

Package `validate` validates Scheme syntax and produces typed expressions.

## Purpose

- Validates syntax expressions against Scheme semantics
- Converts `syntax.SyntaxValue` into `ValidatedExpr` types
- Collects multiple errors without short-circuiting
- Registry-based architecture for special form validators

## Key Types

**ValidatedExpr** - Interface for validated expressions:
- `FormName()` - Form identifier for dispatch
- `Source()` - Source context

**Concrete Types**:
- `ValidatedIf`, `ValidatedDefine`, `ValidatedLambda`
- `ValidatedCaseLambda`, `ValidatedSetBang`, `ValidatedQuote`
- `ValidatedQuasiquote`, `ValidatedBegin`, `ValidatedCall`
- `ValidatedSymbol`, `ValidatedLiteral`

**ValidatedParams** - Parameter list:
- `Required` - Required parameters
- `Rest` - Optional rest parameter

**ValidationResult** - Accumulates errors:
- `Expr` - Validated expression (if successful)
- `Errors` - List of validation errors

## Key Files

| File | Purpose |
|------|---------|
| `validate.go` | Main dispatcher |
| `validated_forms.go` | Type definitions |
| `errors.go` | Error handling |
| `register.go` | Form registration (22 validators) |
| `validate_*.go` | Form-specific validators |

## Gotchas

- **Error accumulation**: Continues validating after errors; all errors collected
- **Parameter validation complexity**: Handles proper lists, improper lists, single symbol
- **Duplicate detection**: Checks for duplicate parameter names including rest
- **Passthrough forms**: Many forms return `ValidatedLiteral` for compiler handling
- **Quasiquote deferred**: Template validation happens at compile time
- **Form name prefix**: Synthetic forms use "@" prefix (@call, @literal, @symbol)

## Testing

Uses quicktest with table-driven tests covering all forms and edge cases.
