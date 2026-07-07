;; Mutation-free vector-map / string-map — the NO-MUTATION bootstrap fragment.
;;
;; A dialect that removes the mutation primitives (vector-set!, string-set!)
;; swaps this fragment in for bootstrap_maps_mutable.scm. It builds the result
;; functionally — list->vector / list->string over map — instead of filling a
;; fresh object in place, so it depends on no mutation primitive. Iteration is
;; delegated to map (itself capturable Scheme), so call/cc inside the callback
;; still works. The extra intermediate list is the price of purity; only the
;; no-mutation engine pays it, never the default.
;;
;; R7RS multi-argument semantics (result length = shortest input) come from map's
;; own shortest-list behaviour, matching the mutating version's explicit min.

(define vector-map
  (case-lambda
    ((f v)
     "Apply F to each element of vector V, returning a new vector\nof results. With multiple vectors, F receives one element from\neach vector per call. The result length is the minimum of all\ninput lengths.\n\nParameters:\n  f : procedure\n  v : vector\nReturns: vector\nCategory: vectors\n\nSee also: `map', `vector-for-each'."
     (list->vector (map f (vector->list v))))
    ((f v1 . rest)
     (list->vector (apply map f (map vector->list (cons v1 rest)))))))

(define string-map
  (case-lambda
    ((f s)
     "Apply F to each character of string S, returning a new string\nof results. F must return a character. With multiple strings,\nF receives one character from each string per call. The result\nlength is the minimum of all input lengths.\n\nParameters:\n  f : procedure\n  s : string\nReturns: string\nCategory: strings\n\nSee also: `map', `string-for-each'."
     (list->string (map f (string->list s))))
    ((f s1 . rest)
     (list->string (apply map f (map string->list (cons s1 rest)))))))
