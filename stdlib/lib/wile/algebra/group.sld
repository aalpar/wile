(define-library (wile algebra group)
  (export make-group group?
          group-op group-identity group-inverse
          group->monoid
          validate-group
          with-group)
  (import (scheme base)
          (wile algebra monoid))
  (include "group.scm"))
