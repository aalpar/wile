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

package wile_test

import (
	"context"
	"errors"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestControlInACompileTimeBody pins that a macro transformer body and a library
// body have a driver: a continuation captured there is resumed there, and a
// guard's exit is resolved there. Both ran under a bare Run before, so only a
// tail call/cc worked, and only because it applied its procedure under a fresh
// driver of its own; a non-tail call/cc or a guard failed the expansion. The
// first row guards the case that worked.
func TestControlInACompileTimeBody(t *testing.T) {
	fsys := fstest.MapFS{
		"probe/control.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe control)
  (import (scheme base))
  (export x y z)
  (begin
    (define x (call/cc (lambda (k) (k 1))))
    (define y (+ 1 (call/cc (lambda (k) (k 1)))))
    (define z (guard (e (#t 'guarded)) (raise 'boom)))))`),
		},
		"probe/box.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe box)
  (import (scheme base))
  (export cell)
  (begin (define cell (vector #f))))`),
		},
		"probe/user.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe user)
  (import (scheme base) (probe box))
  (export used)
  (begin
    (define used 'lib-body-finished)
    ((vector-ref cell 0) 'from-library-body)))`),
		},
		"probe/ab.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe ab)
  (import (scheme base))
  (export my-prompt)
  (begin
    (define (my-prompt thunk)
      (call-with-continuation-prompt thunk (default-continuation-prompt-tag) (lambda args 'handler)))))`),
		},
	}
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "tail call/cc in a transformer body",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (er-macro-transformer (lambda (f r c) (call/cc (lambda (k) (k 42))))))
			      (m)`,
			want: "42",
		},
		{
			name: "operand call/cc in a transformer body",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (er-macro-transformer (lambda (f r c) (+ 1 (call/cc (lambda (k) (k 41)))))))
			      (m)`,
			want: "42",
		},
		{
			name: "guard in a transformer body",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (er-macro-transformer (lambda (f r c) (guard (e (#t 42)) (raise 'boom)))))
			      (m)`,
			want: "42",
		},
		{
			name: "call/cc and guard in a library body",
			src:  `(import (probe control)) (list x y z)`,
			want: "(1 2 guarded)",
		},
		{
			// Each begin-for-syntax body runs on a root of its own at expansion
			// time. A continuation captured in one body and invoked in a later one
			// has a released expansion-time root; the later root resolves it, as a
			// runtime root would a stale continuation of its own, instead of letting
			// the signal escape. (get-n) reads the phase-1 n: 10 if the continuation
			// was resumed, 1 if the resume was dropped.
			name: "a continuation from an earlier compile-time body resumes at the current body's root",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define saved #f) (define n 0))
			      (begin-for-syntax (set! n (+ n (call/cc (lambda (k) (set! saved k) 1)))))
			      (begin-for-syntax (if (< n 5) (saved 10)))
			      (define-syntax get-n (er-macro-transformer (lambda (f r c) n)))
			      (get-n)`,
			want: "10",
		},
		{
			// A continuation captured in one transformer's body and invoked in
			// another's is the same shape: the invoking transformer's root resolves
			// it, so m1's remainder (n, now 10) becomes m2's expansion. Racket raises
			// a continuation barrier here; Wile replays, as it does for a stale
			// continuation at a runtime root.
			name: "a continuation from an earlier transformer body resumes at the current one's root",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define saved #f) (define n 0))
			      (define-syntax m1 (er-macro-transformer (lambda (f r c) (set! n (+ n (call/cc (lambda (k) (set! saved k) 1)))) n)))
			      (define-syntax m2 (er-macro-transformer (lambda (f r c) (if (< n 5) (saved 10)) (list 'quote (list 'm2 n)))))
			      (list (m1) (m2))`,
			want: "(1 10)",
		},
		{
			// A stale continuation's captured winding is re-entered when a later
			// compile-time body resumes it; the body's root, whose winding started
			// empty, leaves those extents when its run ends, as a runtime root does.
			// Without that completion out1 never ran the second time.
			name: "a compile-time root leaves the extents a stale continuation re-entered",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define saved #f) (define log '()) (define (note x) (set! log (cons x log))))
			      (begin-for-syntax
			        (dynamic-wind (lambda () (note 'in1))
			                      (lambda () (call/cc (lambda (k0)
			                        (dynamic-wind (lambda () #f)
			                                      (lambda () (k0 'esc))
			                                      (lambda () (call/cc (lambda (k) (set! saved k) 'x)))))))
			                      (lambda () (note 'out1))))
			      (begin-for-syntax (note 'body2-start) (if saved (let ((k saved)) (set! saved #f) (k 'again))) (note 'body2-end))
			      (define-syntax get-log (er-macro-transformer (lambda (f r c) (list 'quote (reverse log)))))
			      (get-log)`,
			want: "(in1 out1 body2-start in1 out1 body2-end)",
		},
		{
			// The expander keeps m1's context alive while it expands m1's output, so
			// m1's continuation is stale by then although its context is not yet
			// released: the token closes when the root's run ends, and m2's root
			// resolves it (m1's remainder expands to (m2) again, now with n = 10).
			name: "a continuation from a transformer body resumes when invoked from that transformer's output",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define saved #f) (define n 0))
			      (define-syntax m2 (er-macro-transformer (lambda (f r c) (if (< n 5) (saved 10)) (list 'quote (list 'm2 n)))))
			      (define-syntax m1 (lambda (stx) (let ((v (call/cc (lambda (k) (set! saved k) 1)))) (set! n (+ n v))) #'(m2)))
			      (list (m1) 'after)`,
			want: "((m2 11) after)",
		},
		{
			// The winding at the (k v) site inside a transformer is the transformer
			// root's, which started empty; the runtime driver that resumes a runtime
			// continuation passed up from there reconciles from its own winding, so
			// the extent live at the eval site is neither re-entered (in twice) nor,
			// for a stale continuation, left unexited. The transformer's own extent
			// around the invocation is left on the way out (p-in p-out).
			name: "a runtime continuation passed up from a transformer reconciles the runtime's extents, not the transformer's",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define plog '()))
			      (define-syntax m (lambda (stx)
			        (let ((k (cadr (syntax->datum stx))))
			          (dynamic-wind (lambda () (set! plog (cons 'p-in plog)))
			                        (lambda () (k 'from-macro))
			                        (lambda () (set! plog (cons 'p-out plog)))))
			        #''ok))
			      (define-syntax get-plog (er-macro-transformer (lambda (f r c) (list 'quote (reverse plog)))))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (dynamic-wind
			        (lambda () (note 'in))
			        (lambda ()
			          (let ((r (call/cc (lambda (k) k))))
			            (vector-set! st 1 (+ 1 (vector-ref st 1)))
			            (note (list 'r (if (procedure? r) 'k r)))
			            (if (= (vector-ref st 1) 1)
			                (note (list 'eval-returned (eval (list 'm r) (interaction-environment)))))))
			        (lambda () (note 'out)))
			      (list (reverse (vector-ref st 0)) (eval '(get-plog) (interaction-environment)))`,
			want: "((in (r k) (r from-macro) out) (p-in p-out))",
		},
		{
			name: "a stale runtime continuation passed up from a transformer leaves the extent the eval sat in",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 'from-macro)) #''ok))
			      (define st (vector '() #f))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (note (list 'got (call/cc (lambda (k) (vector-set! st 1 k) 'first))))
			      (if (vector-ref st 1)
			          (let ((k (vector-ref st 1)))
			            (vector-set! st 1 #f)
			            (dynamic-wind (lambda () (note 'in))
			                          (lambda () (note (list 'eval-returned (eval (list 'm k) (interaction-environment)))))
			                          (lambda () (note 'out)))))
			      (reverse (vector-ref st 0))`,
			want: "((got first) in out (got from-macro))",
		},
		{
			// A transformer's root that owns a with-timeout passes a runtime
			// continuation up and leaves its own extents on the way; an after thunk
			// that waits past the deadline fails with the cancelled wait, which is the
			// timer's interrupt, so the handler's expansion is the result. Returned
			// raw, the expansion failed with "thread-sleep!: cancelled". with-timeout
			// has no phase-1 library, so it arrives as a value, like k.
			name: "an after thunk's wait cancelled while a transformer passes a resume up runs the timeout handler",
			src: `(import (for-syntax (scheme base) (scheme cxr)))
			      (define-syntax m (lambda (stx)
			        (let* ((d (syntax->datum stx)) (k (cadr d)) (with-timeout (caddr d)) (sleep (cadddr d)))
			          (with-timeout 50 (lambda (resumable) #''timed-out)
			            (lambda ()
			              (dynamic-wind (lambda () #f)
			                            (lambda () (k 'from-macro))
			                            (lambda () (sleep 10))))))))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ((r (call/cc (lambda (k) k))))
			        (vector-set! st 1 (+ 1 (vector-ref st 1)))
			        (note (list 'r (if (procedure? r) 'k r)))
			        (if (= (vector-ref st 1) 1)
			            (note (list 'eval-returned (eval (list 'm r with-timeout thread-sleep!) (interaction-environment)))))
			        (reverse (vector-ref st 0)))`,
			want: "((r k) (eval-returned timed-out))",
		},
		{
			// A default-tag prompt inside the transformer around the invocation does
			// not capture a runtime continuation either: the runtime's remainder is
			// never run at phase 1.
			name: "a prompt inside a transformer does not resume a runtime continuation",
			src: `(import (for-syntax (scheme base) (probe ab)))
			      (define-syntax m (lambda (stx)
			        (let ((k (cadr (syntax->datum stx)))) (my-prompt (lambda () (k 'from-macro))))
			        #''ok))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ()
			        (let ((r (call/cc (lambda (k) k))))
			          (vector-set! st 1 (+ 1 (vector-ref st 1)))
			          (note (list 'r (if (procedure? r) 'k r) (vector-ref st 1)))
			          (if (= (vector-ref st 1) 1)
			              (note (list 'eval-returned (eval (list 'm r) (interaction-environment)))))
			          (note 'end)
			          (reverse (vector-ref st 0))))`,
			want: "((r k 1) (r from-macro 2) end)",
		},
		{
			// A transformer that runs eval on its own context is the first driver
			// above a nested transformer's expansion-time root: it rebases the
			// winding too, so its own extent is left (o-out) on the way to the
			// runtime driver. Skipping it unwound from an empty stack and dropped
			// the extent unexited: (o-in).
			name: "a runtime continuation passed up through a nested transformer leaves the outer transformer's extent",
			src: `(import (for-syntax (scheme base) (scheme eval) (scheme repl)))
			      (begin-for-syntax (define plog '()))
			      (define-syntax inner (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 'from-inner)) #''ok))
			      (define-syntax outer (lambda (stx)
			        (let ((k (cadr (syntax->datum stx))))
			          (dynamic-wind (lambda () (set! plog (cons 'o-in plog)))
			                        (lambda () (eval (list 'inner k) (interaction-environment)))
			                        (lambda () (set! plog (cons 'o-out plog)))))
			        #''ok))
			      (define-syntax get-plog (er-macro-transformer (lambda (f r c) (list 'quote (reverse plog)))))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ((r (call/cc (lambda (k) k))))
			        (vector-set! st 1 (+ 1 (vector-ref st 1)))
			        (note (list 'r (if (procedure? r) 'k r)))
			        (if (= (vector-ref st 1) 1)
			            (note (list 'eval-returned (eval (list 'outer r) (interaction-environment))))))
			      (list (reverse (vector-ref st 0)) (eval '(get-plog) (interaction-environment)))`,
			want: "(((r k) (r from-inner)) (o-in o-out))",
		},
		{
			// The same nesting with the outer transformer's own continuation: the
			// outer root resolves it from its rebased winding, inside the extent, so
			// the before thunk does not run again: (in first resumed out), as Racket.
			name: "a transformer resolves its own continuation invoked from a nested transformer without re-entering its extent",
			src: `(import (for-syntax (scheme base) (scheme eval) (scheme repl)))
			      (begin-for-syntax (define plog '()) (define saved #f))
			      (define-syntax inner2 (lambda (stx) (saved 'resumed) #''ok))
			      (define-syntax outer2 (lambda (stx)
			        (dynamic-wind (lambda () (set! plog (cons 'in plog)))
			                      (lambda () (let ((r (call/cc (lambda (k) (set! saved k) 'first))))
			                                   (set! plog (cons r plog))
			                                   (if (eq? r 'first) (eval '(inner2) (interaction-environment)))))
			                      (lambda () (set! plog (cons 'out plog))))
			        #''ok))
			      (define-syntax get-plog (er-macro-transformer (lambda (f r c) (list 'quote (reverse plog)))))
			      (outer2)
			      (get-plog)`,
			want: "(in first resumed out)",
		},
		{
			// The rebase at a sub-context: the transformer is expanded by an eval
			// that itself runs inside an eval'd dynamic-wind (in2/out2), so the first
			// driver above the expansion is that eval's sub-context, whose winding
			// (in, in2) is the one the runtime root reconciles from: out2 runs, in
			// does not run again.
			name: "a runtime continuation passed up from a transformer is rebased at the sub-context above the expansion",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 'from-macro)) #''ok))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (dynamic-wind
			        (lambda () (note 'in))
			        (lambda ()
			          (let ((r (call/cc (lambda (k) k))))
			            (vector-set! st 1 (+ 1 (vector-ref st 1)))
			            (note (list 'r (if (procedure? r) 'k r)))
			            (if (= (vector-ref st 1) 1)
			                (eval ` + "`" + `(dynamic-wind (lambda () (note 'in2))
			                                     (lambda () (eval (list 'm ,r) (interaction-environment)))
			                                     (lambda () (note 'out2)))
			                      (interaction-environment)))))
			        (lambda () (note 'out)))
			      (reverse (vector-ref st 0))`,
			want: "(in (r k) in2 out2 (r from-macro) out)",
		},
		{
			// The runtime guard reads the root of the invoking chain, not the
			// invoking context: a sub-context under the transformer (an eval of
			// phase-0 code inside it) with a default-tag prompt on its chain still
			// passes a runtime continuation up. Judged by the sub-context alone, the
			// prompt captured it and the rest of the program ran twice.
			name: "a prompt in a sub-context under a transformer does not resume a runtime continuation",
			src: `(import (probe box) (probe ab) (for-syntax (scheme base) (scheme eval) (scheme repl)))
			      (define-syntax m (lambda (stx)
			        (eval '(my-prompt (lambda () ((vector-ref cell 0) 'from-macro))) (interaction-environment))
			        #''ok))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ()
			        (let ((r (call/cc (lambda (k) (vector-set! cell 0 k) k))))
			          (vector-set! st 1 (+ 1 (vector-ref st 1)))
			          (note (list 'r (if (procedure? r) 'k r) (vector-ref st 1)))
			          (if (= (vector-ref st 1) 1)
			              (note (list 'eval-returned (eval '(m) (interaction-environment)))))
			          (note 'end)
			          (reverse (vector-ref st 0))))`,
			want: "((r k 1) (r from-macro 2) end)",
		},
		{
			// Leaving the transformer's extents on the way out can itself transfer
			// control: an after thunk that raises replaces the jump, and the
			// transformer's own guard catches it. Returned raw, it was "no prompt
			// found for tag exit".
			name: "a raise from a transformer's after thunk during the pass-up is caught in the transformer",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx)
			        (let ((k (cadr (syntax->datum stx))))
			          (datum->syntax stx
			            (list 'quote (guard (e (#t (list 'caught e)))
			                           (dynamic-wind (lambda () #f)
			                                         (lambda () (k 'from-macro))
			                                         (lambda () (raise 'after-raise)))))))))
			      (define st (vector '() 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ((r (call/cc (lambda (k) k))))
			        (vector-set! st 1 (+ 1 (vector-ref st 1)))
			        (note (list 'r (if (procedure? r) 'k r)))
			        (if (= (vector-ref st 1) 1)
			            (note (list 'eval-returned (eval (list 'm r) (interaction-environment))))))
			      (reverse (vector-ref st 0))`,
			want: "((r k) (eval-returned (caught after-raise)))",
		},
		{
			// The own-root rule: a frame-delimited continuation (no boundary token)
			// invoked in the same transformer after its prompt has returned has only
			// its root token to go by, and the transformer root resolves it.
			name: "a transformer resolves its own frame-delimited continuation after the prompt returned",
			src: `(import (for-syntax (scheme base) (probe ab)))
			      (begin-for-syntax (define saved #f))
			      (define-syntax m (er-macro-transformer (lambda (f r c)
			        (let ((v (my-prompt (lambda () (+ 100 (call/cc (lambda (k) (set! saved k) 1)))))))
			          (if (< v 105) (saved 5) v)))))
			      (m)`,
			want: "105",
		},
		{
			// The normal-return completion of an expansion-time root: the re-entered
			// extent is left when the body's run ends through the template's end,
			// not only through a delivered empty continuation (the tail form above).
			name: "a compile-time root leaves the extents a stale continuation re-entered, on a normal return",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define saved #f) (define log '()) (define (note x) (set! log (cons x log))))
			      (begin-for-syntax
			        (dynamic-wind (lambda () (note 'in1))
			                      (lambda () (call/cc (lambda (k0)
			                        (dynamic-wind (lambda () #f)
			                                      (lambda () (k0 'esc))
			                                      (lambda () (call/cc (lambda (k) (set! saved k))) (note 'after-rest))))))
			                      (lambda () (note 'out1))))
			      (begin-for-syntax (note 'body2-start) (if saved (let ((k saved)) (set! saved #f) (k 'again))) (note 'body2-end))
			      (define-syntax get-log (er-macro-transformer (lambda (f r c) (list 'quote (reverse log)))))
			      (get-log)`,
			want: "(in1 after-rest out1 body2-start in1 after-rest out1 body2-end)",
		},
		{
			// A stale RUNTIME continuation (its form has ended) handed to a
			// transformer is not the transformer root's to replay: it belongs to the
			// runtime root above the expansion, which replays it, abandoning the
			// form that ran the eval. Replayed at the transformer's root instead, the
			// phase-0 code runs on the phase-1 root and the expansion has no result.
			name: "a stale runtime continuation invoked in a transformer is replayed by the runtime root",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 'from-macro)) #''ok))
			      (define st (vector '() #f))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (note (list 'got (call/cc (lambda (k) (vector-set! st 1 k) 'first))))
			      (note (list 'eval-returned (eval (list 'm (vector-ref st 1)) (interaction-environment))))
			      (reverse (vector-ref st 0))`,
			want: "((got first) (got from-macro))",
		},
		{
			// A live runtime continuation shared across phases through a library
			// and invoked from a begin-for-syntax body under a runtime eval: the
			// body's root passes it up to the runtime driver, which resumes the
			// program once. Resolved at the body's root, the rest of the program ran
			// inside the eval and again after it.
			name: "a live runtime continuation invoked in a compile-time body under eval resumes the runtime program",
			src: `(import (probe box) (for-syntax (scheme base) (probe box)))
			      (define st (vector '()))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ((r (call/cc (lambda (k) (vector-set! cell 0 k) 'first))))
			        (note r)
			        (if (eq? r 'first)
			            (note (list 'eval-returned
			                        (call-with-values (lambda () (eval '(begin-for-syntax ((vector-ref cell 0) 'second)) (interaction-environment))) list))))
			        (note 'end))
			      (reverse (vector-ref st 0))`,
			want: "(first second end)",
		},
		{
			// A phase-1 continuation delimited at a default-tag prompt FRAME carries
			// no boundary token, only its root's. Invoked from a later compile-time
			// body under a runtime eval, that body's root resolves it (the capturing
			// expansion-time root is released); passed up instead, the runtime root
			// reinstalled the phase-1 segment on the phase-0 chain and the program
			// ended with 10.
			name: "a frame-delimited compile-time continuation below a runtime eval stays at the compile-time root",
			src: `(import (for-syntax (scheme base) (probe ab)))
			      (begin-for-syntax (define saved #f) (define n 0))
			      (begin-for-syntax (my-prompt (lambda () (set! n (+ n (call/cc (lambda (k) (set! saved k) 1)))) n)))
			      (define-syntax get-n (er-macro-transformer (lambda (f r c) n)))
			      (define probe
			        (list 'before
			              (call-with-values (lambda () (eval '(begin-for-syntax (if (< n 5) (saved 10))) (interaction-environment))) list)
			              'after))
			      (list probe (get-n))`,
			want: "((before () after) 10)",
		},
		{
			// A live runtime continuation handed to a transformer (through eval and
			// 3-D syntax) and invoked there belongs to the runtime driver above the
			// expansion, not to the transformer's root: it passes up and the runtime
			// program resumes, as in Chez and Racket. The transformer's root tells
			// the two apart by the token: this one's boundary is still open.
			name: "a live runtime continuation invoked in a transformer resumes the runtime program",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 'from-macro)) #''ok))
			      (define st (vector '() #f 0))
			      (define (note x) (vector-set! st 0 (cons x (vector-ref st 0))))
			      (let ()
			        (let ((r (call/cc (lambda (k) (vector-set! st 1 k) 'first))))
			          (vector-set! st 2 (+ 1 (vector-ref st 2)))
			          (note (list 'r r (vector-ref st 2)))
			          (if (= (vector-ref st 2) 1)
			              (note (list 'eval-returned (eval (list 'm (vector-ref st 1)) (interaction-environment)))))
			          (note 'end)
			          (reverse (vector-ref st 0))))`,
			want: "((r first 1) (r from-macro 2) end)",
		},
		{
			// The same below a runtime eval: the signal must not escape the
			// compile-time root into the runtime driver, which would reinstall a
			// phase-1 segment on the phase-0 chain and drop the rest of the program.
			// (get-n) sits in a later form so it expands after the eval has run.
			name: "a compile-time resume below a runtime eval stays at the compile-time root",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define saved #f) (define n 0))
			      (begin-for-syntax (set! n (+ n (call/cc (lambda (k) (set! saved k) 1)))))
			      (define-syntax get-n (er-macro-transformer (lambda (f r c) n)))
			      (define probe
			        (list 'before
			              (call-with-values (lambda () (eval '(begin-for-syntax (if (< n 5) (saved 10))) (interaction-environment))) list)
			              'after))
			      (list probe (get-n))`,
			want: "((before () after) 10)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithSourceFS(fsys),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			v, err := eng.EvalMultiple(context.Background(), tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestStaleRuntimeContinuationAtTopLevelExpansionIsAnError pins the other half
// of the stale-runtime rule: with no runtime driver above the expansion (a
// top-level form, not one under eval), an expansion-time root passes the resume
// up and nothing resolves it, so the form fails to expand with a continuation
// barrier error instead of replaying phase-0 code on a phase-1 root; the
// control signal itself does not reach the embedder. Under eval the runtime root
// replays it, as the table row above pins. The error carries the innermost
// location the cut chain held: for a macro use nested in forms that stamp their
// own, the use, not the outermost form.
func TestStaleRuntimeContinuationAtTopLevelExpansionIsAnError(t *testing.T) {
	tcs := []struct {
		name       string
		src        string
		wantSource string
	}{
		{
			name: "begin-for-syntax",
			src: `(import (probe box) (for-syntax (scheme base) (probe box)))
(vector-set! cell 0 (call/cc (lambda (k) k)))
(begin-for-syntax ((vector-ref cell 0) 'from-phase-1))
'unreached`,
			wantSource: ":3:1",
		},
		{
			name: "macro use nested in lets",
			src: `(import (probe box) (for-syntax (scheme base) (probe box)))
(vector-set! cell 0 (call/cc (lambda (k) k)))
(define-syntax m
  (lambda (stx) ((vector-ref cell 0) 'from-phase-1)))
(let ()
  (let ()
    (m)))
'unreached`,
			wantSource: ":7:5",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithSourceFS(fstest.MapFS{
					"probe/box.sld": &fstest.MapFile{
						Data: []byte(`(define-library (probe box) (import (scheme base)) (export cell) (begin (define cell (vector #f))))`),
					},
				}),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			_, err = eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrContinuationBarrier), qt.IsTrue, qt.Commentf("%v", err))
			var resumeErr *machine.ErrResumeContinuation
			qt.Assert(t, errors.As(err, &resumeErr), qt.IsFalse)
			// This entry's conversion, not any barrier error, and with the location the
			// cut chain would otherwise have lost.
			qt.Assert(t, err.Error(), qt.Contains, "no runtime driver to resume it")
			var ce *wile.CompilationError
			qt.Assert(t, errors.As(err, &ce), qt.IsTrue)
			qt.Assert(t, ce.Source, qt.Equals, tc.wantSource)
		})
	}
}

// TestLibraryBodyThatExitedThroughAContinuationIsNotReplayedByLaterImports pins
// what a library remembers of a body that invoked a runtime continuation. The
// first instantiation, run by a runtime form, hands the resume to that form's
// driver, which resumes the continuation (count 2). The memoized failure a later
// import receives is a continuation barrier error, not the signal: remembered as
// is, every later import resumed the continuation again (count 3, 4, ...).
func TestLibraryBodyThatExitedThroughAContinuationIsNotReplayedByLaterImports(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fstest.MapFS{
			"probe/box.sld": &fstest.MapFile{
				Data: []byte(`(define-library (probe box) (import (scheme base)) (export cell) (begin (define cell (vector #f 0))))`),
			},
			"probe/user.sld": &fstest.MapFile{
				Data: []byte(`(define-library (probe user)
  (import (scheme base) (probe box))
  (export used)
  (begin (define used 'lib-body-finished) ((vector-ref cell 0) 'from-library-body)))`),
			},
		}),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	_, err = eng.EvalMultiple(ctx, `(import (probe box))
	                                (let ()
	                                  (let ((r (call/cc (lambda (k) (vector-set! cell 0 k) 'first))))
	                                    (vector-set! cell 1 (+ 1 (vector-ref cell 1)))
	                                    (if (< (vector-ref cell 1) 4) (environment '(probe user)))
	                                    (vector-ref cell 1)))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrContinuationBarrier), qt.IsTrue, qt.Commentf("%v", err))
	var resumeErr *machine.ErrResumeContinuation
	qt.Assert(t, errors.As(err, &resumeErr), qt.IsFalse)
	qt.Assert(t, err.Error(), qt.Contains, "exited through a continuation")
	// The barrier error keeps the library's location reachable, as the first
	// importer's error does: the cut chain must not take it along.
	var se *compilation.SourcedError
	qt.Assert(t, errors.As(err, &se), qt.IsTrue, qt.Commentf("%v", err))
	qt.Assert(t, se.Source.Location(), qt.Equals, "probe/user.sld:1:0")
	cell, ok := eng.Get("cell")
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, cell.SchemeString(), qt.Contains, " 2)")
}

// TestAbortAtTopLevelExpansionIsAnError is the abort form of the stale-resume
// test: a phase-1 abort to a tag no expansion-time chain holds reaches the
// compile entry, which reports "no prompt found" as a plain error rather than
// letting the abort signal surface to the embedder.
func TestAbortAtTopLevelExpansionIsAnError(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fstest.MapFS{
			"probe/abort.sld": &fstest.MapFile{
				Data: []byte(`(define-library (probe abort) (import (scheme base)) (export my-abort)
  (begin (define (my-abort) (abort-current-continuation (make-continuation-prompt-tag 'nowhere) 'v))))`),
			},
		}),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	_, err = eng.EvalMultiple(ctx, `(import (for-syntax (scheme base) (probe abort)))
	                                (begin-for-syntax (my-abort))
	                                'unreached`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue, qt.Commentf("%v", err))
	var abortErr *machine.ErrPromptAbort
	qt.Assert(t, errors.As(err, &abortErr), qt.IsFalse)
}

// TestBootstrapBodyHasADriver pins the third expansion-time root: a bootstrap
// source runs under RunWithinBoundary, so a guard in it works and a call/cc in
// it resumes. Under a bare Run both failed the load.
func TestBootstrapBodyHasADriver(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	err = compilation.LoadBootstrapSources(ctx, eng.Environment(),
		[]string{`(define bootstrap-probe (list (guard (e (#t 'guarded)) (raise 'boom)) (+ 1 (call/cc (lambda (k) (k 1))))))`},
		resolver.NewEmbedFileResolver(stdlib.FS), "probe")
	qt.Assert(t, err, qt.IsNil)
	v, err := eng.EvalMultiple(ctx, `bootstrap-probe`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(guarded 2)")
}
