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

// Package sat ships a CDCL SAT solver as a Wile extension.
//
// The solver implements watched-literal unit propagation, 1-UIP conflict
// analysis with clause learning, VSIDS-style activity branching, Luby
// restarts, and activity-based clause-database cleanup. It targets
// MiniSat-class competence on instances up to roughly 10k variables and
// 100k clauses.
//
// The Scheme front-end at (wile algebra sat) wraps these primitives with a
// Tseitin transform and exposes sat?, sat-cnf?, boolean-decide-sat?, and
// boolean-decide-equivalent?. See memory/2026-05-30-sat-solver-design.local.md.
package sat
