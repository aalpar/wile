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

// Package threads provides SRFI-18 multithreading primitives.
//
// # Threads
//
//   - make-thread, thread?
//   - thread-name, thread-specific, thread-specific-set!
//   - thread-start!, thread-yield!, thread-sleep!
//   - thread-terminate!, thread-join!
//   - current-thread
//
// # Mutexes
//
//   - make-mutex, mutex?
//   - mutex-name, mutex-specific, mutex-specific-set!
//   - mutex-state
//   - mutex-lock!, mutex-unlock!
//
// # Condition Variables
//
//   - make-condition-variable, condition-variable?
//   - condition-variable-name
//   - condition-variable-specific, condition-variable-specific-set!
//   - condition-variable-signal!, condition-variable-broadcast!
//
// # Time
//
//   - current-time, time?, time->seconds, seconds->time
//
// # Exceptions
//
//   - join-timeout-exception?, abandoned-mutex-exception?
//   - terminated-thread-exception?, uncaught-exception?
//   - uncaught-exception-reason
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package threads
