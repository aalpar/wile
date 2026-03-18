# NaN-Boxing

You just profiled an interpreter and discovered that 60% of wall time is the garbage collector scanning and collecting heap objects. The #1 allocated type is `EnvironmentFrame` — a struct that holds pointers to your value type. The #2 is a `[]Binding` slice copied on every function call. Your value type is a Go interface:

```go
type Value interface {
    SchemeString() string
    IsVoid() bool
    EqualTo(Value) bool
}
```

Every integer, boolean, character, and symbol is a heap-allocated struct behind that interface. Every time the VM pushes `42` onto the eval stack, it's pushing a 16-byte interface value (type pointer + data pointer) that points to a heap-allocated `Integer` struct. The GC must trace every one of those pointers.

What if you could fit the most common values — integers, booleans, characters, small pointers — *inside* a single 64-bit word, with no heap allocation at all?

That's NaN-boxing.

## The Problem

A dynamically-typed language needs tagged values. Every slot that holds "a Scheme value" must be able to hold an integer, a float, a boolean, a pair, a closure, or any of dozens of other types. In Go, the idiomatic way to do this is interfaces. In C, it's typically a tagged union:

```c
struct Value {
    enum { TAG_INT, TAG_FLOAT, TAG_BOOL, TAG_PAIR, ... } tag;
    union {
        int64_t  integer;
        double   floating;
        bool     boolean;
        Pair*    pair;
    } data;
};
```

This tagged union is 16 bytes (8 for the union, 4 for the tag, 4 padding). A Go interface is also 16 bytes (type descriptor pointer + data pointer). Both work, both are clean, and both have the same problem: **every value is at least 16 bytes, and pointer-containing values force GC scanning.**

In a tight loop like `(fib 30)`, the VM executes millions of operations, each pushing and popping interface values. The integer `42` is represented as a pointer to a heap-allocated `Integer{Value: 42}`. Even with a small-integer cache (Wile caches -32768 to 32767), every value still occupies a 16-byte interface slot, and the GC must scan every pointer in every stack, continuation, and environment frame.

Can we do better than 16 bytes per value?

## The Key Insight

Here's a fact about IEEE 754 floating-point that most people never think about: a 64-bit `double` has *a lot* of bit patterns that represent NaN (Not a Number).

A double is laid out like this:

```
 63  62       52 51                                    0
┌───┬───────────┬──────────────────────────────────────┐
│ S │ Exponent  │            Mantissa                  │
│ 1 │  11 bits  │            52 bits                   │
└───┴───────────┴──────────────────────────────────────┘
```

A NaN is any value where all 11 exponent bits are 1 *and* the mantissa is non-zero. The sign bit and 52 mantissa bits can be anything. That gives us:

- 1 sign bit × 2^52 - 1 mantissa patterns = about **2^53 distinct NaN bit patterns**

IEEE 754 hardware only ever produces one specific NaN (the "canonical" or "quiet" NaN). That means roughly **2^53 - 1 bit patterns** are NaN values that no legitimate floating-point operation will ever generate. They're free real estate.

The insight: **stuff your non-float values into the unused NaN bit patterns.** A 64-bit word can now be:
- A legitimate `double` (any non-NaN pattern), or
- A tagged non-float value, disguised as a NaN

You get tagged values in 8 bytes instead of 16 — with zero heap allocation for common types.

## How It Works

The quiet NaN pattern has exponent bits all-1 and bit 51 set (the "quiet" flag). That leaves the sign bit and bits 50-0 (51 bits) to encode whatever you want. A typical encoding:

```
Floating-point double (any non-NaN value):
┌────────────────────────────────────────────────────────────────┐
│                    Normal IEEE 754 double                      │
└────────────────────────────────────────────────────────────────┘

NaN-boxed value:
 63  62    52  51  50  48 47                                   0
┌───┬────────┬───┬───────┬─────────────────────────────────────┐
│ 1 │1111111│ 1  │  Tag  │          48-bit Payload             │
│   │1111   │    │ 3 bits│                                     │
└───┴────────┴───┴───────┴─────────────────────────────────────┘
     exponent  quiet
     all 1s    NaN
```

The sign bit is set to 1 (to distinguish from the canonical NaN). The 3-bit tag identifies the type. The 48-bit payload carries the actual data.

With 3 tag bits, you get 8 non-float types:

| Tag | Type | Payload |
|-----|------|---------|
| 000 | Integer | 48-bit signed integer (±140 trillion) |
| 001 | Boolean | 0 or 1 |
| 010 | Character | Unicode code point (21 bits used) |
| 011 | Heap pointer | 48-bit pointer to heap object |
| 100 | Symbol ID | 48-bit interned symbol index |
| 101 | Empty list | (no payload needed) |
| 110 | Void | (no payload needed) |
| 111 | (reserved) | |

The heap pointer tag is the escape hatch: any value too large or complex to fit in 48 bits (pairs, vectors, closures, bignums) gets heap-allocated and the NaN-boxed word holds a pointer to it.

Here's the critical part: **48 bits is enough for pointers on every current architecture.** x86-64 uses 48-bit virtual addresses (with sign extension to 64 bits). ARM64 uses 48 or 52 bits. So a heap pointer fits in the payload with room to spare.

## Why It Matters for Interpreters

The performance impact is dramatic. Consider what changes when every value is 8 bytes instead of 16:

**1. No heap allocation for common types.** Pushing integer `42` onto the eval stack means writing one 8-byte word — not allocating a struct on the heap, not creating an interface value with two pointers. The integer *is* the word.

**2. Half the memory per value slot.** Stacks, environment bindings, continuation frames — everything that holds values shrinks by 50%. This means better cache utilization (more values fit in L1/L2) and less GC work (fewer bytes to scan).

**3. No pointer tracing for boxed primitives.** An integer, boolean, or character NaN-boxed value contains zero pointers. The GC doesn't need to trace it. Only heap-pointer-tagged words need tracing. In a typical Scheme program, the majority of values in flight are integers and booleans — so the GC scan set shrinks dramatically.

**4. Type checks become bit masking.** Instead of a pointer dereference to read a type descriptor (Go interface) or a memory load of a tag field (tagged union), you check the type by masking the upper bits of the word. This is a single AND instruction — no memory access, no cache miss.

To put numbers on it: LuaJIT (which NaN-boxes) and JavaScriptCore (which uses a variant called "JSValue") report 2-4x speedups over pointer-tagged representations on numeric benchmarks. The improvement comes from both the allocation elimination and the cache effects.

## The Subtle Parts

**Floats pass through unchanged.** Any legitimate `double` — including positive and negative zero, infinities, and the canonical NaN — is stored directly. The encoding is designed so that the NaN-boxing tag patterns are all *non-canonical* NaNs that floating-point hardware never produces. This means float operations need zero encoding/decoding overhead.

**Pointer tagging requires cooperation.** The 48-bit payload can hold a pointer, but only if the pointer's upper 16 bits are predictable (typically all zeros or all ones, depending on address space layout). On most operating systems this is guaranteed for userspace addresses. But it means you're making an assumption about the platform's virtual memory layout.

**Integer range is limited.** A 48-bit signed integer covers ±140 trillion — more than enough for loop counters and indices, but not for arbitrary-precision arithmetic. When an integer exceeds 48 bits, it must be promoted to a heap-allocated bignum, and the NaN-boxed word switches to a heap pointer. The promotion boundary is different from the native `int64` boundary, which adds complexity to overflow checks.

**Alignment constraints.** Heap-allocated objects must be aligned to at least 8 bytes so that the lowest 3 bits of their address are always zero. Some implementations steal those low bits for additional tag bits, giving more type tags but requiring masking on every pointer dereference.

## Why Go Can't Do This

NaN-boxing is fundamentally a `unsafe` technique. You're reinterpreting the bits of a `float64` as something that isn't a float — a pointer, an integer, a type tag. In C, this is normal (union types, pointer casts). In Go, it requires `unsafe.Pointer` and `math.Float64frombits` / `math.Float64bits` for the bit conversions, plus raw pointer arithmetic to recover heap pointers from 48-bit payloads.

But the deeper problem is the **garbage collector.** Go's GC determines liveness by following pointers. It knows that a `*Integer` field in a struct is a pointer because the type system says so. A NaN-boxed value is an opaque `uint64` — from the GC's perspective, it contains no pointers. If you store a heap pointer inside a NaN-boxed word, the GC doesn't know it's there. The pointed-to object looks unreachable and gets collected. Your program crashes.

There are three possible workarounds, none of them clean:

1. **Manual root tracking.** Maintain a separate set of all live heap-allocated objects so the GC can find them. This is essentially writing your own garbage collector on top of Go's GC — defeating the purpose of using Go.

2. **`runtime.KeepAlive` / pinning.** Pin every heap object referenced by a NaN-boxed value so the GC won't collect it. This eliminates the GC's ability to reclaim memory, causing unbounded growth.

3. **Custom allocator with `mmap`.** Allocate all Scheme heap objects from a manually-managed arena that Go's GC doesn't touch, then implement your own mark-and-sweep. This works (CPython does something similar) but means abandoning Go's memory safety guarantees and losing the benefit of Go's concurrent GC.

All three approaches use `unsafe` and fight the language's memory model. For a project where the design constraint is "pure Go, no `unsafe`" — which is Wile's constraint — NaN-boxing is off the table entirely.

## What Interpreters Actually Do

Different languages handle this differently, roughly correlated with how much control they have over memory layout:

| Interpreter | Value representation | Why |
|---|---|---|
| **LuaJIT** (C) | NaN-boxing | Full control over memory; custom GC |
| **JavaScriptCore** (C++) | NaN-boxing variant ("JSValue") | Same; plus JIT can exploit the encoding |
| **CPython** (C) | Tagged pointer (3-bit tag) | Custom reference-counting GC; `PyObject*` |
| **V8** (C++) | Tagged pointer ("Smi" for small ints) | Pointer tagging, not NaN-boxing; custom GC |
| **GHC** (Haskell) | Tagged pointer | Unboxed types in compiled code |
| **Chez Scheme** (C) | Tagged fixnum + heap objects | Custom GC with precise pointer maps |
| **Go interpreters** | Interface values | Must cooperate with Go's GC |

The common thread: NaN-boxing and pointer tagging require either a custom GC or `unsafe` memory manipulation. Languages hosted on a managed runtime (Go, Java, C#) are limited to whatever value representation the host GC can trace.

## What This Means for Wile

Wile uses Go interfaces for values. Every `values.Value` is a 16-byte interface (type pointer + data pointer). The profiling data shows the cost:

- 55.5M pool misses for `EnvironmentFrame` allocation
- 54.4M `[]Binding` slice copies (each binding holds a `values.Value` interface)
- 60% of wall time in GC on allocation-heavy benchmarks

NaN-boxing would eliminate most of these allocations. Integers, booleans, characters, and symbols would be inline 8-byte words. Environment bindings would be `[N]uint64` arrays instead of `[N]*Binding` with interface indirection. The GC scan set would shrink dramatically.

But the `unsafe` constraint makes this a hard no. The productive direction is reducing allocations *within* Go's type system: inline small arrays to avoid slice allocation, copy-on-write to defer unnecessary copies, and better pool strategies to survive GC drains. These won't achieve the 2-4x that NaN-boxing provides, but they're architecturally sound within the constraint.

> **Aside**: Some Go projects use a "tagged uint64" approach with a custom arena allocator (e.g., Vitess's SQL evaluator). This is architecturally similar to NaN-boxing but uses explicit tag bits instead of IEEE 754 NaN patterns. It still requires `unsafe` for pointer recovery. Wile's constraint rules this out too.
