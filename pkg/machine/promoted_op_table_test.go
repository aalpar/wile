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
	"testing"
)

// TestPromotedOpsCoverPromotedOpcodes pins the promotedOps registry against the
// opcode range it describes. A descriptor that exists but is left off the list
// would otherwise be invisible to promotedOpForName, silently disabling
// promotion for that primitive while every other test still passed.
func TestPromotedOpsCoverPromotedOpcodes(t *testing.T) {
	covered := make(map[OpCode]string)
	for _, op := range promotedOps {
		for _, code := range []OpCode{op.nonTail, op.tail} {
			prev, dup := covered[code]
			if dup {
				t.Errorf("opcode %s claimed by both %q and %q", code, prev, op.name)
			}
			covered[code] = op.name
		}
	}
	for code := OpEqQ; code <= OpSetCdrTail; code++ {
		_, ok := covered[code]
		if !ok {
			t.Errorf("opcode %s is in the promoted range but no promotedOps entry claims it", code)
		}
	}
	if len(covered) != int(OpSetCdrTail-OpEqQ+1) {
		t.Errorf("promotedOps covers %d opcodes, promoted range holds %d",
			len(covered), OpSetCdrTail-OpEqQ+1)
	}
}

// TestPromotedOpForNameMatchesDescriptors pins that the name lookup the peephole
// optimizer uses agrees with each descriptor, and that a non-promoted name
// resolves to OpInvalid.
func TestPromotedOpForNameMatchesDescriptors(t *testing.T) {
	for _, op := range promotedOps {
		nonTail, tail, arity := promotedOpForName(op.name)
		if nonTail != op.nonTail || tail != op.tail || arity != op.arity {
			t.Errorf("promotedOpForName(%q) = (%s, %s, %d), want (%s, %s, %d)",
				op.name, nonTail, tail, arity, op.nonTail, op.tail, op.arity)
		}
	}
	nonTail, tail, arity := promotedOpForName("vector-set!")
	if nonTail != OpInvalid || tail != OpInvalid || arity != 0 {
		t.Errorf("promotedOpForName(non-promoted) = (%s, %s, %d), want (OpInvalid, OpInvalid, 0)",
			nonTail, tail, arity)
	}
}
