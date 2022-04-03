#lang racket

(require racket/random
         "ga.rkt")

;; the operators need to have information attached to them like arity
(struct op (arity fn)
  #:transparent) ; do fixed arity for this test run, multi-arity could come later

(define operators (list (op 2 '+)
                        (op 2 '*)
                        (op 2 '-)))

(define variables '(x))

(define (fn->evalable fn x)
  (list (list 'lambda '(x) fn) x))

(define (cap n #:max (m 4294967087))
  (if (> n m)
      m
      n))

(define (invert n #:max (m 4294967087))
  (- m n))

;; how to determine distance from fitness? Linear? Exponential?
(define (fitness fn)
  (let ((input-range (range -50 50)))
    (round
     (/
      (apply
       +
       (map ; perhaps I'll be able to pmap this w/ futures since it'll all be math that also keeps it out of the ga/gp control flows and in the caller's flow. I like that.
        (lambda (x)
          (abs
           (- (- (* 15 x) (+ x 2))
              ;; '(+ (+ (* (+ 1 10) x) (+ x x)) x)
              ;; '(+ 14 (+ x (- (- (* x 14) x) 14)))
              ;; '(* 14 x)
              ;; '(+ (+ (+ x (- (- (* x 14) 14) x)) (- (+ (- x 14) 14) x)) 14)
              ;; '(* 14 x)
              ;; '(+ 9 (- (+ (* 14 x) x) x))
              ;; '(+ (+ x (+ (* (+ x 13) (* 1 1)) (- (* 12 x) x))) x)
              ;; '(+ x (+ (- (+ (+ (- (- (+ (- (- (- (- (- (+ (* x 11) (+ x x)) 5) 1) x) 11) (+ 11 1)) (+ (+ x x) 11)) (+ x x)) (+ x x)) (+ (+ 5 x) 11)) (+ x x)) x) x))
              ;; ^ that one has zero error over the input range!
              ;; vvv All in the below list have error < 10
              ;; '((+ (+ (+ (- (+ 11 (+ x (- (- (- (- (* x 14) 5) x) (+ (+ x x) x)) 11))) x) (+ x x)) x) x)
              ;;   (+ (+ (+ (+ (- (- (- (- (- (+ (* x 11) (+ x x)) 5) 1) x) 11) (+ 5 x)) x) 11) x) x)
              ;;   (- (+ (- (+ (- (- (- (- (- (+ (* x 11) (+ x x)) 5) 1) x) 11) (+ 11 1)) (+ (+ x x) 11)) (+ x x)) (+ (+ (+ x x) 7) x)) x)
              ;;   (+ x (+ (- (+ (+ (- (- (+ (- (- (- (- (- (+ (* x 11) (+ x x)) 5) 1) x) 11) (+ 11 1)) (+ (+ x x) 11)) (+ x x)) (+ x x)) (+ (+ 5 x) 11)) (+ x x)) x) x))
              ;;   (- (* x 14) 11)
              ;;   (+ (- (+ (- (- (- (- (- (+ (* x 11) (+ x x)) 5) 1) x) 11) x) (+ (+ x x) 11)) (+ x x)) (+ (+ (+ x x) 7) x))
              ;;   (- (+ (- (+ (- (- (- (- (- (+ (* x 11) (+ x x)) 5) 1) x) 11) (+ 11 1)) (+ (+ x x) 11)) (+ x x)) (+ (+ (+ x x) 7) x)) x))
              ;; vvvv same again
              ;; '((+ (- (+ (+ (- (* (* x 5) 3) (- x 3)) (- 3 7)) 4) 5) (- 3 3))
              ;;   (+ (+ (- (* (* x 5) 3) (- x 3)) (- 3 7)) 3)
              ;;   (+ (- (+ (- (* (* x 5) 3) 3) 3) x) (- 3 7))
              ;;   (+ (- (* (* x 5) 3) (- x 3)) (- 3 7))
              ;;   (+ (- (+ (+ (+ (- (* (* x 5) 3) (- x 3)) (- 3 7)) 7) (- 3 7)) 3) (- 3 (- 3 7)))
              ;;   (- (+ (- (+ (- (* (* x 5) 3) 3) 3) x) (- 3 7)) 3)
              ;;   (- (+ (+ (+ (- (* (* x 5) 3) (- x 3)) (- 3 7)) 7) 3) 7)
              ;;   (- (+ (- (+ (- (* (* x 5) 3) 3) 3) x) (- 3 3)) 3))
              ;; '(+ (- (+ (+ (- (* (* x 5) 3) (- x 3)) (- 3 7)) 4) 5) (- 3 3))
              ;; ^^ another zero error over the input range solution
              ;; vvv few more decent approximations error < 10
              ;; '((+ (* x 13) x)
              ;;   (- (+ x (+ x (- (- (+ (* x 14) x) x) x))) x)
              ;;   (- (+ (* x 14) x) x)
              ;;   (- (+ (* x 14) 14) 14)
              ;;   (+ 14 (- (- (+ (* x 14) x) x) 14))
              ;;   (- (+ x (+ x (- (- (* x 13) 12) (- (- (- (+ x x) x) 13) x)))) x)
              ;;   (+ 14 (- (+ 14 (- (- (+ (* x 14) x) x) 14)) 14))
              ;;   (- (+ 14 (- (- (+ (* x 14) x) x) 14)) (- x x)))
              ;; '(+ (- (- (* 14 x) 0) 18) 16) => (- (* 14 x) 2)
              ;; ^ another zero, but this time not in the final population
              ;; leave it to the computer to realize that (- (* 15 x) (+ x 2)) is (- (* 14 x) 2)
              (with-handlers ((exn:fail? 0))
                (eval (fn->evalable fn x))))))
        input-range))
      (length input-range)))))

(define (scaled-fitness pop-size)
  (lambda (fn)
    ;; A smaller cap keeps a larger chunk of unfit citizens from reproducing (due to the invert and weighted random; the max gets inverted to 0 and has no chance of breeding)
    ;; could set a minimum to keep diversity a _tiny_ bit higher (very rarely a poor performer would be able to give genetic material)
    (let ((max 250))
      (invert
       (cap
        (fitness fn)
        #:max max)
       #:max max))))

(define (random-operator-or-variable)
  (random-ref (append operators variables)))

(define (random-variable-or-constant)
  (if (> (random 100) 50)
      (random-ref variables)
      (random 20)))

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
  ;; adjust to allow parents back into population
  ;; use some struct to also store the fitness on the citizen
  (let ((a-point (random-tree-point a))
        (b-point (random-tree-point b)))
    (list (insert-at-point a a-point (get-at-point b b-point))
          (insert-at-point b b-point (get-at-point a a-point)))))

(define (run)
  (let ((pop-size 100))
    (evolve
     (make-breed (scaled-fitness pop-size) crossover)
     (map (thunk* (generate 3)) (range pop-size))
     50
     #:grapher (lambda (pop)
                 (let* ((fitnesses (map fitness pop))
                        (min-sort (sort fitnesses <)))
                   (displayln "New generation!")
                   (displayln (format "Max error: ~a" (first (sort fitnesses >))))
                   (displayln (format "Min error: ~a" (first min-sort)))
                   (when (= 0 (first min-sort))
                     (displayln
                      (map
                       car
                       (filter
                        (lambda (p)
                          (= (cdr p) 0))
                        (map cons pop fitnesses)))))
                   (displayln (format "Mean error: ~a" (/ (apply + fitnesses) (length fitnesses))))
                   (displayln "---------------"))))))
