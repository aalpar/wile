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

	tests := []struct {
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
			name: "unexpected EOF in list",
			err:  errors.New("unexpected EOF in list"),
			want: true,
		},
		{
			name: "unterminated string literal",
			err:  errors.New("unterminated string literal"),
			want: true,
		},
		{
			name: "unclosed parenthesis",
			err:  errors.New("unclosed parenthesis"),
			want: true,
		},
		{
			name: "plain error",
			err:  errors.New("undefined variable"),
			want: false,
		},
		{
			name: "wrapped error containing unexpected EOF",
			err:  fmt.Errorf("parse failed: %w", errors.New("unexpected EOF")),
			want: true,
		},
		{
			name: "CompilationError wrapping incomplete error",
			err: &CompilationError{
				Message: "compile",
				Cause:   errors.New("unterminated string"),
			},
			want: true,
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(IsIncompleteInput(tt.err), qt.Equals, tt.want)
		})
	}
}
