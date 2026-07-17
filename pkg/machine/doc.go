// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Package machine implements the Scheme virtual machine, compiler, and macro expander.
//
// The machine package is the core execution engine for Wile, providing:
//
// # Compilation Pipeline
//
// Scheme source undergoes three phases:
//  1. Expansion: macro-expand via [ExpanderTimeContinuation]
//  2. Compilation: generate bytecode via [CompileTimeContinuation]
//  3. Execution: run bytecode via [MachineContext]
//
// # Virtual Machine
//
// The VM is a stack-based bytecode virtual machine with:
//   - [MachineContext]: execution state (value register, eval stack, environment, PC)
//   - [MachineContext.Run]: main execution loop
//   - [NativeTemplate]: compiled bytecode container
//   - [MachineClosure]: first-class procedure representation
//
// # Bytecode Assembly
//
// [NewNativeTemplate], [NativeTemplate.AppendInstruction], [NativeTemplate.AppendOperations],
// [NativeTemplate.AppendSideTableOp] and [NativeTemplate.PatchInstructionArg] are a
// trusted-producer surface, exported for the compiler and for test fixtures. The instruction
// stream is taken as well-formed: an out-of-range Arg (a local-binding index, a branch offset,
// a side-table index) is not validated and reaches the dispatch loop as an unchecked slice
// index.
//
// The resulting panic is contained only on entry via [MachineContext.RunResumable], which
// recovers at the VM boundary and returns it as an error. [MachineContext.Run] and
// [MachineContext.RunWithinBoundary] do not recover, so a malformed template assembled through
// this surface unwinds into the host goroutine.
//
// # Continuations
//
//   - [MachineContinuation]: captured continuation state for call/cc
//   - [ComposableContinuation]: delimited continuations for prompts
//   - dynamic-wind support via wind/unwind stacks
//
// # Macro System
//
// Implements R7RS hygienic macros with Flatt's "sets of scopes" model:
//   - [ExpanderTimeContinuation]: macro expansion driver
//   - syntax-rules pattern matching via internal/match
//   - syntax-case with fenders and procedural macros
//
// # Foreign Functions
//
// Go functions are exposed as [ForeignFunction] implementations:
//
//	type ForeignFunction func(CallContext) error
//
// [CallContext] is the narrow interface (7 methods) that extensions depend on.
// The [ForeignClosure] type wraps these for the VM.
package machine
