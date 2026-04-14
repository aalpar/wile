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
	"fmt"

	"github.com/aalpar/wile/internal/syntax"
)

// SourcedError wraps a compilation error with the source location where it
// occurred. The compiler tracks source context via pushSource/popSource;
// wrapCompileError attaches the current source to errors so that callers
// (especially the public Engine API) can report file:line:col.
//
// Use errors.As to extract the source from an error chain:
//
//	var se *compilation.SourcedError
//	if errors.As(err, &se) && se.Source != nil { ... }
type SourcedError struct {
	Source *syntax.SourceContext
	Err    error
}

func (p *SourcedError) Error() string {
	if p.Source != nil && p.Source.File != "" {
		return fmt.Sprintf("%s:%d:%d: %s",
			p.Source.File,
			p.Source.Start.Line(),
			p.Source.Start.Column(),
			p.Err.Error())
	}
	return p.Err.Error()
}

func (p *SourcedError) Unwrap() error {
	return p.Err
}
