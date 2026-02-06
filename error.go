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
	"io"
)

// Error represents a Wile engine error.
type Error struct {
	Message string
	Cause   error
}

func (p *Error) Error() string {
	if p.Cause != nil {
		return p.Message + ": " + p.Cause.Error()
	}
	return p.Message
}

func (p *Error) Unwrap() error {
	return p.Cause
}

// isEOF checks if an error represents end of input.
func isEOF(err error) bool {
	return errors.Is(err, io.EOF)
}
