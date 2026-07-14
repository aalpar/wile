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

package werr

// RecoverAsError normalizes the value returned by recover() into an error
// carrying the caller's sentinel.
//
// A nil r (no panic in flight) yields a nil error, so a deferred closure may
// assign the result unconditionally. Every other value acquires the sentinel:
// an r that is already an error is chained as the cause, and any other value
// (panic("...") / panic(42)) has its text preserved in the message.
//
// The sentinel must reach an error-typed r, not just a string one. A Go runtime
// fault inside foreign code — nil deref, index out of range, nil-map write —
// arrives here as a runtime.Error, which satisfies error. Returning it unchanged
// would strip the site's sentinel from precisely the panics that indicate host
// memory corruption, leaving them indistinguishable at the VM boundary from a
// deliberate (error "...") raised by a well-behaved primitive.
//
// Chaining as a cause rather than returning unchanged does not disturb callers
// that route on the panic's identity: ForeignError.Is/As traverse the cause, so
// the VM's foreign-call bridge still matches prompt-abort, exception-escape,
// timer-interrupt, and continuation-resume signals through the wrap. No site
// type-asserts those signals directly.
//
// The sentinel is a parameter rather than a fixed ErrInternal because each
// recover site's sentinel is part of that site's contract: the VM boundary
// reports ErrInternal, a thread root reports ErrThreadPanic, a foreign call
// reports ErrPanicRecovery.
//
// site names the recovering site and prefixes the message, e.g. "thread \"w\""
// or "foreign function call". Pass "" when the caller wraps the result again with
// its own prefix, so the site name does not appear twice.
func RecoverAsError(r any, sentinel error, site string) error {
	if r == nil {
		return nil
	}
	err, ok := r.(error)
	if ok {
		if site == "" {
			return WrapForeignErrorWithCause(sentinel, err, "panic recovery")
		}
		return WrapForeignErrorWithCause(sentinel, err, "%s: panic recovery", site)
	}
	if site == "" {
		return WrapForeignErrorf(sentinel, "non-error panic value: %v", r)
	}
	return WrapForeignErrorf(sentinel, "%s: non-error panic value: %v", site, r)
}
