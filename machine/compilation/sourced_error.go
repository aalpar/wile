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

package compilation

import (
	"github.com/aalpar/wile/internal/syntax"
)

// SourcedError wraps a compilation error with the source location where it
// occurred. The compiler tracks source context via pushSource/popSource;
// wrapCompilationError attaches the current source to errors so that callers
// (especially the public Engine API) can report file:line:col.
//
// Use errors.As to extract the source from an error chain:
//
//	var se *compilation.SourcedError
//	if errors.As(err, &se) && se.Source != nil { ... }
type SourcedError struct {
	Source *syntax.SourceContext
	Cause  error
}

func (p *SourcedError) Error() string {
	loc := p.Source.Location()
	if loc != "" {
		return loc + ": " + p.Cause.Error()
	}
	return p.Cause.Error()
}

func (p *SourcedError) Unwrap() error {
	return p.Cause
}

// wrapSourcedError attaches source location to an error.
// For use in standalone functions that lack a CompileTimeContinuation receiver.
// If src is nil, returns the error unchanged.
func wrapSourcedError(src *syntax.SourceContext, err error) error {
	if src == nil {
		return err
	}
	return &SourcedError{Source: src, Cause: err}
}
