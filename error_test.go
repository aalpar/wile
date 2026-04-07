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

package wile

import (
	"errors"
	"fmt"
	"io"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestIsIncompleteInput(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		err  error
		want bool
	}{
		{
			name: "nil error",
			err:  nil,
			want: false,
		},
		{
			name: "plain io.EOF",
			err:  io.EOF,
			want: false,
		},
		{
			name: "wrapped io.EOF via fmt.Errorf",
			err:  fmt.Errorf("parse failed: %w", io.EOF),
			want: true,
		},
		{
			name: "CompilationError wrapping io.EOF",
			err: &CompilationError{
				Message: "parse error",
				Cause:   io.EOF,
			},
			want: true,
		},
		{
			name: "unterminated string literal",
			err:  errors.New("unterminated string literal"),
			want: true,
		},
		{
			name: "unclosed block comment",
			err:  errors.New("unclosed block comment"),
			want: true,
		},
		{
			name: "unknown token type",
			err:  errors.New("unknown token type: StringStart"),
			want: true,
		},
		{
			name: "CompilationError wrapping unterminated",
			err: &CompilationError{
				Message: "parse error",
				Cause:   errors.New("unterminated string"),
			},
			want: true,
		},
		{
			name: "plain error",
			err:  errors.New("undefined variable"),
			want: false,
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			c.Assert(IsIncompleteInput(tc.err), qt.Equals, tc.want)
		})
	}
}
