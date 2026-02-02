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

import (
	"fmt"
	"sync/atomic"

	"github.com/aalpar/wile/go/values"
)

// PromptTag is an opaque identity value for delimited continuation prompts.
// Equality is pointer identity (each tag is unique). Follows the Racket model
// of continuation prompt tags.
//
// See: Flatt, Yu, Findler, Felleisen "Adding Delimited and Composable Control
// to a Production Programming Environment" (ICFP 2007).
type PromptTag struct {
	id   uint64
	name string
}

var nextPromptTagID uint64

// NewPromptTag creates a new prompt tag with an optional name for debugging.
func NewPromptTag(name string) *PromptTag {
	id := atomic.AddUint64(&nextPromptTagID, 1)
	q := &PromptTag{
		id:   id,
		name: name,
	}
	return q
}

// DefaultPromptTag is the default prompt tag installed at the top of execution.
var DefaultPromptTag = NewPromptTag("default")

func (p *PromptTag) SchemeString() string {
	if p.name != "" {
		return fmt.Sprintf("#<continuation-prompt-tag:%s>", p.name)
	}
	return fmt.Sprintf("#<continuation-prompt-tag:%d>", p.id)
}

func (p *PromptTag) IsVoid() bool { return p == nil }

func (p *PromptTag) EqualTo(o values.Value) bool {
	v, ok := o.(*PromptTag)
	if !ok {
		return false
	}
	return p == v
}
