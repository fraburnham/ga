#lang racket

(require racket/random)

;; the operators need to have information attached to them like arity
(struct op (arity fn)
  #:transparent) ; do fixed arity for this test run, multi-arity could come later

(define operators (list (op 2 '+)
                        (op 2 'expt)
                        (op 2 '*)
                        (op 2 '-)))

(define variables '(x))

(define (fn->evalable fn x)
  (list (list 'lambda '(x) fn) x))

;; how to determine distance from fitness? Linear? Exponential?
(define (fitness fn)
  (let ((input-range (range 1 100)))
    (/
     (apply
      +
      (map ; perhaps I'll be able to pmap this w/ futures since it'll all be math that also keeps it out of the ga/gp control flows and in the caller's flow. I like that.
       (lambda (x)
         ;; this will need some exception handling to mark the error as very high
         ;; when division by zero happens (perhaps the largest num as that value)
         (abs
          (- (- (* 15 x) (expt x 2))
             (eval (fn->evalable fn x)))))
       input-range))
     (length input-range))))

(define (random-operator-or-variable)
  (random-ref (append operators variables)))

(define (random-variable-or-constant)
  (if (> (random 100) 50)
      (random-ref variables)
      (random 100)))

(define (generate max-depth)
  (let ((o (random-ref operators)))
    (if (= max-depth 1)
        (cons (op-fn o) (map (thunk* (random-variable-or-constant)) (range (op-arity o))))
        (if (= 1 (op-arity o))
            (cons (op-fn o) (generate (sub1 max-depth)))
            ;; all these probabilities to do one thing or another will need to be exposed for tweaking.
            (let ((args (cons (generate (sub1 max-depth)) (map (thunk* (if (> (random 100) 75) ; sometimes make the arg a new op otherwise a const/var
                                                                           (generate (sub1 max-depth))
                                                                           (random-variable-or-constant)))
                                                               (range (sub1 (op-arity o)))))))
              (cons (op-fn o) (random-sample args (length args) #:replacement? #f)))))))

;; WHOOP! now how to handle stuff like crossover?
;; these are lists, so it _should_ be a simple matter of recursion

;; create offspring (single) by picking a point on a and picking a point on b
;; all things below that point go to the child?
;; hmm, the point needs to be a place where reconnecting them makes sense
;; so '(+ (* 1 3) (expt x 2))
;;        ^ point
;; and '(* 1 (+ x 2))
;;           ^ point
;; would become
;; '(+ (+ x 2) (expt x 2))
;; and
;; '(* 1 (* 1 3))
;; cut _out_ from a at a's point
;; splice in from b at b's point
;; work on a makes a hole
;; work on b fills that hole
;; some names, call everything between root and point `root`
;; call everything _after/below_ point `stem`
;; get stem-a and stem-b and do swapsies
;; ex 2
;; '(* (expt (- 68 x) x) 4)
;;     ^ point
;; '(+ (expt (* x x) x) (- (expt 65 x) 49))
;;                               ^ point
;; become
;; '(* 65 4)
;; '(+ (expt (* x x) x) (- (expt (expt (- 68 x) x) 49)))
;; should the parents be dropped back in population? the potential for change is pretty dramatic
;; there may be less correlation between parent fitness and child fitness (should track this somehow...)
;; will need to return coords to the point will depth and offset work?
;; (+ 1 (* 2 x))
;; depth 0 offset 1 => (* 2 x)
;; (+ (expt (* x x) 1) (- (expt 65 x) 49))
;; depth 2 offset 1 => ; this doesn't work which branch do I go down on the way to depth 2 to get a consistent offset?
;; => 1 || x
;; so a tree path has to be a list of offsets for a depth first walk
;; go over n then go down
;; (+ (expt (* x x) 1) (- (expt 65 x) 49))
;; (1 1 1) => x ; first go over to (expt (* x x) 1) then (* x x) then x
;; (0) => +
;; (1 0) => expt
;; will have to avoid taking 0? or have 0 return the _whole_ expression
;; (1 0) => (expt (* x x) 1)
;; do that ^

;; where Node is a (Listof (U op Leaf))
;; and Leaf is a (U var const)
;; then the check here could be node? instead of list?
(define (random-tree-point tree)
  (let ((p (if (list? tree)
               (random (length tree))
               0)))
    (if (zero? p)
        '()
        (cons p (random-tree-point (list-ref tree p))))))

(define (get-at-point tree point)
  (if (empty? point)
      tree
      (get-at-point (list-ref tree (first point)) (rest point))))

(define (insert-at-point tree point subtree)
  (if (empty? point)
      subtree
      (let ((p (first point)))
        (append
         (take tree p)
         (list (insert-at-point (list-ref tree p) (rest point) subtree))
         (drop tree (add1 p))))))

(define (crossover a b)
  (let ((a-point (random-tree-point a))
        (b-point (random-tree-point b)))
    (list (insert-at-point a a-point (get-at-point b b-point))
          (insert-at-point b b-point (get-at-point a a-point)))))

;; now a breeding setup
;; can I reuse the stuff from ga (pull it here if it needs any refactoring)

;; I think I can reuse it!
;; ugh the same ick that fitness has to return an Exact-Positive-Integer
;; Can I fix that?
