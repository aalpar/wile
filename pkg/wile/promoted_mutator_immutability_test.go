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
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	qt "github.com/frankban/quicktest"
)

// promotedMutatorPrograms pairs each promoted mutator with a tail-position and a
// non-tail-position use on a quoted literal. The two arms must AGREE.
//
// Agreement is the durable property, and disagreement was the actual defect. The
// peephole emits OpReleaseEnvFrame immediately ahead of a promoted tail op;
// releaseEnvFrame runs EnvironmentFrame.ResetForPool, which zeroes the frame's
// namespace back-pointer while mc.env still points at it, and the promoted op
// then runs on the same context. The immutability guard resolved through that
// namespace, got nil, and answered false — so the guard was present, ran, and
// passed. At 003b3353 the tail arm returned #!void and left the literal as
// (1 . 9) while the non-tail arm raised: the same program, two answers,
// decided by an optimizer pass.
//
// wantErr is what BOTH arms must now do. It is false for set-cdr! because
// pair-literal immutability was dropped when the side set was deleted — a flag
// word is ~25% growth on the 32-byte cons cell, measured against a path this
// very optimizer promotes. The tail arm's old answer is now the specified one
// for both, which is why the table records an expectation rather than being
// deleted along with the guard: a promoted VECTOR mutator added later lands
// here with wantErr true, and a promoted mutator with no program at all still
// fails loudly.
//
// This must run on a full Engine, not on the testhelpers pipeline. Promotion
// needs the peephole (tpl.Optimize) AND stable base primitives, which only
// WithImmutableTopLevel stamps; without both, no promoted opcode is emitted, no
// OpReleaseEnvFrame precedes it, and every arm passes for the wrong reason.
var promotedMutatorPrograms = map[string]struct {
	tail    string
	nonTail string
	wantErr bool
}{
	"set-cdr!": {
		tail:    `(define (m p v) (set-cdr! p v)) (m '(1 2) 9)`,
		nonTail: `(define (m p v) (begin (set-cdr! p v) 'done)) (m '(1 2) 9)`,
		wantErr: false,
	},
}

// TestPromotedMutatorsRespectImmutableLiterals is the durable half of the
// released-frame fix: without it, the next promoted mutator reopens the class.
// The table is keyed off machine.PromotedMutatorNames, so a newly classified
// mutator with no program here fails rather than being skipped.
//
// It is the behavioural arm. The structural one is
// TestOpReleaseEnvFrameKeepsEngineStateReachable in pkg/machine, and it is not
// redundant: an aggregate that carries its own immutability flag has a guard
// that never reads mc.env, so a future promoted vector mutator would pass this
// test without touching the namespace-loss class at all.
func TestPromotedMutatorsRespectImmutableLiterals(t *testing.T) {
	ctx := context.Background()
	names := machine.PromotedMutatorNames()
	qt.Assert(t, len(names) > 0, qt.IsTrue)
	for _, name := range names {
		progs, ok := promotedMutatorPrograms[name]
		if !ok {
			t.Errorf("promoted mutator %q has no immutability program; add one", name)
			continue
		}
		arms := []struct {
			label string
			code  string
		}{
			{"tail", progs.tail},
			{"non-tail", progs.nonTail},
		}
		for _, arm := range arms {
			t.Run(name+"/"+arm.label, func(t *testing.T) {
				eng, err := NewEngine(ctx, WithProfile(KitchenSink))
				qt.Assert(t, err, qt.IsNil)
				_, err = eng.EvalMultipleWithSource(ctx, arm.code, "promoted-mutator.scm")
				if progs.wantErr {
					qt.Assert(t, err, qt.IsNotNil,
						qt.Commentf("%s/%s must refuse to mutate a literal", name, arm.label))
					return
				}
				qt.Assert(t, err, qt.IsNil,
					qt.Commentf("%s/%s must agree with its sibling arm; a difference here "+
						"means an optimizer pass decided the answer", name, arm.label))
			})
		}
	}
}
