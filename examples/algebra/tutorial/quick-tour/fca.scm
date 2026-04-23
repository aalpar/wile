;; quick-tour: (wile algebra fca)
;;
;; Formal Concept Analysis: discover the "concepts" hidden in an
;; object-attribute table. A concept is a maximally extended
;; extent/intent pair: every object in the extent has every attribute
;; in the intent, and adding more of either would break that property.
;; Pick it up when you have a sparse binary table and want the
;; lattice of regularities.

(import (scheme base) (wile algebra fca) (wile algebra lattice))
(include "../lib/check.scm")

;; -- A tiny context: 4 objects x 3 attributes ----------------------

(define ctx
  (context-from-alist
    '(("lion"    "mammal" "carnivore")
      ("tiger"   "mammal" "carnivore")
      ("deer"    "mammal")
      ("eagle"   "carnivore"))))

(check-true (fca-context? ctx)                   "context built")

(check-true (member "lion"     (context-objects ctx))     "lion is an object")
(check-true (member "mammal"   (context-attributes ctx))  "mammal is an attribute")

;; -- Derivation operators (intent and extent) ----------------------

;; Intent: attributes shared by an object set.
(check= (intent ctx '("lion" "tiger"))
        '("carnivore" "mammal")
        "lion + tiger share mammal and carnivore")

(check= (intent ctx '("lion" "tiger" "deer"))
        '("mammal")
        "lion + tiger + deer share only mammal")

;; Extent: objects sharing all given attributes.
(check= (extent ctx '("mammal"))
        '("deer" "lion" "tiger")
        "mammals: deer, lion, tiger")

(check= (extent ctx '("mammal" "carnivore"))
        '("lion" "tiger")
        "mammal AND carnivore: lion, tiger")

;; -- The concept lattice -------------------------------------------

(define concepts (concept-lattice ctx))
(check-true (list? concepts)                     "concept-lattice returns a list")
(check-true (>= (length concepts) 2)             "at least 2 concepts")

;; Each concept is (extent . intent).
(define a-concept (car concepts))
(check-true (pair? a-concept)                    "concept is a pair")

;; -- Project the concepts into the algebra's <lattice> ------------

(define CL (concept-lattice->algebra-lattice ctx concepts))
(check-true (lattice? CL)                        "concepts form a lattice")

(display "fca tour complete") (newline)
