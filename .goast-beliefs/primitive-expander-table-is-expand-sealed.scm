(import (wile goast belief))

;; Belief: every function that constructs a primitive expander writes it through
;; SealedWriteViewAt(PhaseExpand) — and there is exactly one such function,
;; registerPrimitiveExpandersWithout.
;;
;; WHAT THIS IS AND IS NOT. The PHASE half of that claim is already a CI-gating Go
;; ratchet: TestPrimitiveExpandersLandAtExactPhaseOne
;; (pkg/machine/compilation/primitive_expanders_registry_test.go) asserts that
;; five form names resolve at (phase 1, sealed) and NOT at exact phase 0, and all
;; five of its subtests go red — along with dozens of macro and hygiene tests —
;; when the taproot is flipped to PhaseRuntime. Measured 2026-10-03. So this file
;; is NOT the pin for the phase, and must not be cited as one.
;;
;; What it adds that no Go test does is the SITE COUNT and the write-target
;; SHAPE: that the expander table has exactly one builder, and that the builder
;; reaches its target through a sealed-write view rather than by some other route.
;; A second builder is the shape the D7 residual is filed against (TODO.md, "The
;; D7 residual: a primitive expander above phase 1"), because
;; LookupPrimitiveExpander reads env.Expand() — an ABSOLUTE phase 1 — which is
;; CORRECT only while the table is single-instance. D7 proposed NextPhase() there;
;; measured, that one symbol breaks bootstrap loading, and D7 is WITHDRAWN. The
;; conditional that stays filed is: if a dialect ever installs a primitive
;; expander elsewhere, the absolute read becomes wrong at that moment.
;;
;; Site set: functions whose body contains a NewPrimitiveExpander call. Measured
;; 2026-10-03 over NON-TEST sources: exactly ONE,
;; registerPrimitiveExpandersWithout. The qualifier matters — a run that loaded
;; test files would see four sites in this package alone
;; (phase_registry_test.go:47,95,122 and primitive_expander_test.go), three of
;; them reporting 'no-sealed-write, for ratio 0.25. check_beliefs excludes tests
;; by default, which is what makes the count 1.
;;
;; The site predicate is an AST walk, so it reaches the call through the FuncLit
;; RegisterPhaseBindings is handed; an SSA-level predicate would not, because
;; go-ssa-build never emits anonymous functions.
;;
;; Assertion mechanism. The checker collects the first argument of every
;; SealedWriteViewAt call in the site body, deduplicates with the library's own `unique`, and reports
;; 'expand-sealed (the only target is PhaseExpand), 'no-sealed-write (no sealed
;; taproot at all), or 'other-target (any other phase, a second distinct target,
;; or a computed argument). Reading the ARGUMENT is why this is a custom checker
;; and not contains-call, which matches the callee name and would score a table
;; built at PhaseRuntime as adherent. The dedupe is deliberate: splitting the one
;; registration into two writes to the SAME coordinate is not a regression, and an
;; undeduplicated comparison against (list "PhaseExpand") called it one.
;;
;; THE LIMIT THAT MATTERS, because it decides what this file is good for: at
;; total = 1 this belief's STATUS CANNOT GO RED. run-beliefs computes the ratio as
;; the majority category's share, which is 1.0 for every possible return value
;; when there is one site, so `status`, `ratio` and `deviations` are constant
;; functions of the code — verified twice, by flipping the production taproot
;; (only `pattern` moved, 'expand-sealed -> 'other-target, status stayed 'strong)
;; and by a probe checker returning #f unconditionally (also 'strong). This is
;; NOT the ordinary consensus caveat that a correlated change fools a majority
;; vote; at one site there is no vote. A reader must read `pattern` and `total`.
;; Nothing in Makefile or .github/workflows runs .goast-beliefs at all, so there
;; is no automated consumer to read either: this is an on-demand observation, not
;; a gate.
;;
;; What it DOES still answer automatically is site disappearance — zero sites
;; reports 'no-sites rather than passing — and that is the one thing a Go test
;; keyed on the table's contents cannot see.
;;
;; The helper is INTERNAL to the checker, not a top-level define: run-beliefs
;; loads every file in this directory into ONE top level, and the engine's top
;; level is immutable by default, so a top-level helper would raise "cannot
;; redefine immutable top-level binding" the second time the corpus is loaded in a
;; session, and would collide with a same-named helper in a sibling belief. (The
;; first draft of this file did exactly that and reported belief_count: 0.) No
;; other file in this directory defines one.
(define-belief "primitive-expander-table-is-expand-sealed"
  (sites (functions-matching (contains-call "NewPrimitiveExpander")))
  (expect (custom
            (lambda (site ctx)
              ;; First argument of a SealedWriteViewAt call, as the source spells
              ;; it ("PhaseExpand"); #f for any other node, so walk collects only
              ;; the hits. unique is the library's own order-preserving dedupe.
              (define (sealed-write-target n)
                (and (eq? (car n) 'call-expr)
                     (let ((fn (nf n 'fun)))
                       (and (pair? fn)
                            (eq? (car fn) 'selector-expr)
                            (equal? (nf fn 'sel) "SealedWriteViewAt")
                            (let ((args (nf n 'args)))
                              (if (pair? args)
                                  (or (nf (car args) 'sel)
                                      (nf (car args) 'name)
                                      "«computed»")
                                  "«no-argument»"))))))
              (let ((targets (unique (walk (or (nf site 'body) '()) sealed-write-target))))
                (cond ((null? targets) 'no-sealed-write)
                      ((equal? targets (list "PhaseExpand")) 'expand-sealed)
                      (else 'other-target))))))
  (threshold 1.0 1))
