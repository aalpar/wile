(define-library (wile algebra category)
  (description "Categories: morphism composition with identity and associativity.")
  (export make-category category?
          category-compose category-identity category-equiv?
          category->endomorphism-monoid
          procedure-category
          validate-category
          with-category)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra monoid))
  (include "category.scm"))
