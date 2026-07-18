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

// Package gointerop provides Go concurrency primitive wrappers.
//
// # Read-Write Mutexes
//
//   - make-rw-mutex, rw-mutex?
//   - rw-mutex-lock!, rw-mutex-unlock!
//   - rw-mutex-rlock!, rw-mutex-runlock!
//
// # Once
//
//   - make-once, once?
//   - once-do!
//
// # Atomics
//
//   - make-atomic, atomic?
//   - atomic-load, atomic-store!, atomic-swap!, atomic-cas!
//   - make-atomic-int64, atomic-int64?
//   - atomic-int64-load, atomic-int64-store!, atomic-int64-add!
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package gointerop
