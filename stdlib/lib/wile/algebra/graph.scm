;;; (wile algebra graph) — Semiring-parameterized graph algorithms
;;;
;;; Lazy single-source Bellman-Ford parameterized by semiring.
;;; Boolean semiring = reachability, tropical = shortest path,
;;; counting = path count.
;;;
;;; Adjacency is an alist: ((node . ((neighbor . edge-data) ...)) ...)
;;; Weight function maps edge-data to semiring values; #f for unit weights.

;; --- Local utilities ---

(define (ga-filter pred lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (loop (cdr xs)
            (if (pred (car xs)) (cons (car xs) acc) acc)))))

;; --- Record type ---

(define-record-type <graph-analysis>
  (make-graph-analysis* semiring adjacency weight-fn cache)
  graph-analysis?
  (semiring   ga-semiring)
  (adjacency  ga-adjacency)
  (weight-fn  ga-weight-fn)
  (cache      ga-cache set-ga-cache!))

;; --- Constructor ---

(define (make-graph-analysis semiring adjacency weight-fn)
  "Construct a graph analysis from a semiring, adjacency alist, and weight function.\nADJACENCY is an alist: ((node . ((neighbor . edge-data) ...)) ...).\nWEIGHT-FN receives edge-data and returns a semiring value.\nPass #f for unit weights (each edge = semiring-one).\n\nExamples:\n  (make-graph-analysis (boolean-semiring)\n    '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n    #f)\n\nParameters:\n  semiring : any\n  adjacency : list\n  weight-fn : procedure-or-false\nReturns: graph-analysis\nCategory: algebra\n\nSee also: `graph-query', `graph-query-all'."
  (let ((wfn (or weight-fn (lambda (_) (semiring-one semiring)))))
    (make-graph-analysis* semiring adjacency wfn '())))

;; --- Single-source computation ---

;; Compute distances from source using worklist Bellman-Ford.
;; Returns alist ((name . value) ...) for all reachable nodes.
(define (compute-single-source ga source)
  (let ((S   (ga-semiring ga))
        (adj (ga-adjacency ga))
        (wfn (ga-weight-fn ga)))
    (let loop ((worklist (list source))
               (dist (list (cons source (semiring-one S)))))
      (if (null? worklist) dist
          (let* ((node (car worklist))
                 (rest (cdr worklist))
                 (node-dist (cdr (assoc node dist))))
            ;; Get outgoing edges for this node
            (let ((entry (assoc node adj)))
              (if (not entry)
                  (loop rest dist)
                  (let edge-loop ((edges (cdr entry))
                                  (wl rest)
                                  (d dist))
                    (if (null? edges)
                        (loop wl d)
                        (let* ((neighbor-name (caar edges))
                               (edge-data (cdar edges))
                               (w (wfn edge-data))
                               (candidate (semiring-times S node-dist w))
                               (old-entry (assoc neighbor-name d))
                               (old-val (if old-entry (cdr old-entry) (semiring-zero S)))
                               (merged (semiring-plus S old-val candidate)))
                          (if (equal? merged old-val)
                              (edge-loop (cdr edges) wl d)
                              (let ((new-d (cons (cons neighbor-name merged)
                                                 (if old-entry
                                                     (ga-filter (lambda (p) (not (equal? (car p) neighbor-name))) d)
                                                     d))))
                                (edge-loop (cdr edges)
                                           (if (member neighbor-name wl) wl (cons neighbor-name wl))
                                           new-d)))))))))))))

;; --- Cache layer ---

(define (get-or-compute ga source)
  (let ((cached (assoc source (ga-cache ga))))
    (if cached (cdr cached)
        (let ((result (compute-single-source ga source)))
          (set-ga-cache! ga (cons (cons source result) (ga-cache ga)))
          result))))

;; --- Public API ---

(define (graph-query ga source target)
  "Query the semiring value between source and target nodes.\nReturns semiring-zero if target is unreachable. Lazily computes\nand caches single-source distances on first query per source.\n\nExamples:\n  (let ((ga (make-graph-analysis (boolean-semiring)\n              '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n              #f)))\n    (graph-query ga \"A\" \"B\"))  => #t\n\nParameters:\n  ga : graph-analysis\n  source : any\n  target : any\nReturns: any\nCategory: algebra\n\nSee also: `graph-query-all', `make-graph-analysis'."
  (let* ((dist (get-or-compute ga source))
         (entry (assoc target dist)))
    (if entry (cdr entry) (semiring-zero (ga-semiring ga)))))

(define (graph-query-all ga source)
  "Return distance alist for all reachable nodes from source.\nEach entry is (name . semiring-value). Lazily computed and cached.\n\nExamples:\n  (let ((ga (make-graph-analysis (tropical-semiring)\n              '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n              (lambda (e) e))))\n    (graph-query-all ga \"A\"))  => ((\"A\" . 0) (\"B\" . 1))\n\nParameters:\n  ga : graph-analysis\n  source : any\nReturns: list\nCategory: algebra\n\nSee also: `graph-query', `make-graph-analysis'."
  (get-or-compute ga source))
