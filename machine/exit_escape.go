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

package machine

import "github.com/aalpar/wile/values"

// ExitTag is an opaque identity type for call-with-exit escape closures.
// Each call-with-exit invocation creates a unique *ExitTag; pointer equality
// identifies which escape closure belongs to which invocation. ExitTag has no
// exported fields or methods — it exists solely as a unique pointer identity.
//
// Inspired by S7 Scheme's call-with-exit.
type ExitTag struct{}

// ErrExitEscape is returned by call-with-exit escape closures to signal an
// upward escape from the dynamic extent of call-with-exit. Like ErrPromptAbort,
// it propagates through the call stack without being wrapped as a Scheme exception
// (applyForeign passes it through unchanged).
//
// PrimCallWithExit matches it by comparing the tag pointer. The validity check
// (*atomic.Bool) in the escape closure ensures stale tags are never emitted.
type ErrExitEscape struct {
	tag   *ExitTag
	Value values.Value
}

func (e *ErrExitEscape) Error() string {
	return "exit escape"
}

// Tag returns the opaque tag identifying which call-with-exit invocation this
// escape belongs to. PrimCallWithExit uses pointer comparison to match its own tag.
func (e *ErrExitEscape) Tag() *ExitTag {
	return e.tag
}

// NewErrExitEscape creates an ErrExitEscape with the given tag and escape value.
// Only the call-with-exit invocation that created the matching *ExitTag will catch it.
func NewErrExitEscape(tag *ExitTag, val values.Value) *ErrExitEscape {
	return &ErrExitEscape{tag: tag, Value: val}
}

// NewExitTag creates a new unique exit tag for one call-with-exit invocation.
func NewExitTag() *ExitTag {
	return &ExitTag{}
}
