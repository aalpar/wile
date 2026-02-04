# R7RS Semantic Differences

This document catalogs differences between the current implementation and the R7RS-small specification. These are semantic differences where the implementation produces results but may not match R7RS behavior for certain inputs.

**Reference:** [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)

**Last Updated:** 2026-01-30

---

## Summary

No known semantic differences remain. All previously documented issues have been fixed.

---

## Extensions Beyond R7RS

These are Wile-specific features that extend R7RS. They do not conflict with R7RS behavior — standard Scheme programs behave identically. These extensions use reader prefixes in the `#` dispatch space that R7RS leaves implementation-defined.

### Arbitrary-Precision Number Literals

Wile provides reader syntax for explicitly constructing arbitrary-precision numbers. These are not part of any Scheme standard (R5RS, R6RS, R7RS, or SRFIs).

| Prefix | Type | Exactness | Backed by | Examples |
|--------|------|-----------|-----------|----------|
| `#z` | BigInteger | exact | `math/big.Int` | `#z12345678901234567890`, `#z-42`, `#z+7` |
| `#m` | BigFloat | inexact | `math/big.Float` (256-bit) | `#m3.14159265358979323846`, `#m1.5e-10`, `#m.5` |

Both prefixes are case-insensitive (`#Z`, `#M` also work), following R7RS §7.1.1 conventions.

**BigInteger (`#z`)** supports radix prefixes: `#z#b101` (binary), `#z#o77` (octal), `#z#x1F` (hex).

**BigFloat (`#m`)** supports optional sign, decimal point, and exponent markers (`e`, `s`, `f`, `d`, `l`).

**Note:** R7RS requires implementations to support arbitrarily large exact integers (§6.2.3). Wile satisfies this via automatic overflow promotion from `Integer` (int64) to `BigInteger` — the `#z` prefix is a convenience for explicit construction, not a conformance requirement. Standard R7RS programs never need `#z` or `#m`.

