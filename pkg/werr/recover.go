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

// RecoverAsError normalizes the value returned by recover() into an error.
//
// A nil r (no panic in flight) yields a nil error, so a deferred closure may
// assign the result unconditionally. An r that is already an error is returned
// unchanged: callers that inspect it with errors.Is/errors.As — the VM's
// foreign-call bridge matches prompt-abort, exception-escape, and timer-interrupt
// signals this way — must see the value the panic carried, not a fresh identity
// layered on top. Any other value (panic("...") / panic(42)) is wrapped under the
// caller's sentinel with its text preserved in the message.
//
// The sentinel is a parameter rather than a fixed ErrInternal because each
// recover site's sentinel is part of that site's contract: the VM boundary
// reports ErrInternal, a thread root reports ErrThreadPanic, a foreign call
// reports ErrPanicRecovery. Only the error/non-error switch is shared, and that
// switch is the whole of what this function owns.
//
// context names the recovering site and prefixes the message, e.g. "thread \"w\""
// or "foreign function call". Pass "" when the caller wraps the result again with
// its own prefix, so the site name does not appear twice.
func RecoverAsError(r any, sentinel error, context string) error {
	if r == nil {
		return nil
	}
	err, ok := r.(error)
	if ok {
		return err
	}
	if context == "" {
		return WrapForeignErrorf(sentinel, "non-error panic value: %v", r)
	}
	return WrapForeignErrorf(sentinel, "%s: non-error panic value: %v", context, r)
}
