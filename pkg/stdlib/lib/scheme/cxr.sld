(define-library (scheme cxr)
  (description "Compositions of car and cdr up to four deep: caar, cadr, ..., cddddr.")
  ;; These procedures are defined ambiently in registry/core/bootstrap_procedures.scm
  ;; (core procedures such as assoc and list? depend on them, so they must exist
  ;; before any library loads). This library is a re-export manifest only: it
  ;; surfaces those ambient bindings under (scheme cxr) without redefining them.
  ;; Defining them a second time here duplicated the definitions and, under
  ;; WithImmutableTopLevel, made (import (scheme cxr)) collide with the already-
  ;; stable ambient binding. See plans/2026-06-13-layered-environment-architecture.md.
  (export
    caar cadr cdar cddr
    caaar caadr cadar caddr
    cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr))
