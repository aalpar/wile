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

// Package algebragraph exposes the Go-side graph kernels in extensions/algebra/graph
// as Scheme primitives. It is the FFI surface that lets (wile algebra graph)
// route bigint-carrier semiring queries through the in-place monotone kernel
// and SCC condensation, instead of the all-Scheme worklist fallback.
//
// Two primitives are registered:
//
//   - count-paths-in-dag — wraps graph.CountPathsInDAG (DAG-only).
//   - count-paths-cyclic — wraps graph.CountPathsCyclic (any directed graph;
//     SCC-condenses cyclic input and reports per-SCC counts with non-trivial
//     SCC flagging).
//
// Both primitives are intended for internal use by (wile algebra graph) under
// the soft-dispatch fast path; they are not part of the user-facing R7RS
// surface. The Scheme library probes for them at load time and falls back to
// pure-Scheme dispatch if the extension is not loaded.
//
// See memory/2026-05-26-scc-condensation.md for the design.
package algebragraph
