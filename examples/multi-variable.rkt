#lang typed/racket ; #:with-refinements

(require typed/racket/random
         "../ga.rkt")

(require/typed racket/vector
  (vector-empty? (All (a) (-> (Vectorof a) Boolean))))

(define-type Binary-Digit (U Zero One))
(define-type Binary-List (Listof Binary-Digit))
;; this needs a refinement to be exactly 16 elements
(define-type Chromasome (Vectorof Binary-Digit))

(struct citizen ((x : Real)
                 (y : Real)
                 (chromasome : Chromasome))
  #:transparent)

(define-type Population (Listof citizen))

(: chromasome-bits (Refine (i : Integer)
                           (= 16 i)))
(define chromasome-bits 16)

(: natural->binary-vector (-> Natural (Vectorof Binary-Digit)))
(define (natural->binary-vector n)
  (list->vector
   (map
    (lambda ((sn : Char)) : Binary-Digit
      (cond
        ((char=? sn #\1) 1)
        ((char=? sn #\0) 0)
        (else 0)))
    (string->list (format "~b" n)))))

;; TODO: Refine this to be a vector of 8 elements
(: vector->real (-> (Vectorof Binary-Digit) Real))
(define (vector->real c)
  (if (vector-empty? c)
      0
      (+
       (* (expt 2 (sub1 (vector-length c))) (vector-ref c 0))
       (vector->real (vector-drop c 1)))))

(: binary-list->chromasome (-> Binary-List Chromasome))
(define (binary-list->chromasome l)
  (list->vector l))

(: chromasome->binary-list (-> Chromasome Binary-List))
(define (chromasome->binary-list v)
  (vector->list v))

;; TODO(later): Convince the typechecker about chromasomes
(: vector->chromasome (-> (Vectorof Binary-Digit) Chromasome))
(define (vector->chromasome v)
  (let ((chrom : Chromasome (make-vector chromasome-bits (ann 0 Binary-Digit))))
    ;; copy v into chrom shifting right if needed (so bits 15 align in both)
    (vector-copy! chrom
                  (- chromasome-bits (vector-length v))
                  v
                  0
                  (vector-length v))
    chrom))

(: interpolate (-> (Pairof Real Real) (Pairof Real Real) Real Real))
(define (interpolate in out n)
  (let ((in-min (car in))
        (in-max (cdr in))
        (out-min (car out))
        (out-max (cdr out)))
    (+ (* (/ n (- in-max in-min))
          (- out-max out-min))
       out-min)))

(: chromasome->x (-> Chromasome Real))
(define (chromasome->x chrom)
  (interpolate
   (cons 0 255) (cons -3 3)
   (vector->real (vector-take chrom 8))))

(: chromasome->y (-> Chromasome Real))
(define (chromasome->y chrom)
  (interpolate
   (cons 0 255) (cons -3 3)
   (vector->real (vector-drop chrom 8))))

(: random-population (-> Natural Population))
(define (random-population size)
    (if (= size 0)
      '()
      (cons
       ;; TODO: probably natural->binary-vector should be natural->chromasome
       (let ((chrom (vector->chromasome (natural->binary-vector (random (expt 2 16))))))
         (citizen (chromasome->x chrom)
                  (chromasome->y chrom)
                  chrom))
       (random-population (sub1 size)))))

(: mutate (-> citizen citizen))
(define (mutate c)
  (let* ((chrom : Chromasome (citizen-chromasome c))
         (point : Integer (random (vector-length chrom)))
         (gene : Binary-Digit (vector-ref chrom point))
         (new-gene : Binary-Digit (if (= 1 gene) 0 1)))
    (vector-set! chrom point new-gene)
    (citizen (chromasome->x chrom)
             (chromasome->y chrom)
             chrom)))

(: crossover (-> citizen citizen (Listof citizen)))
(define (crossover a b)
  (let* ((a-chrom (citizen-chromasome a))
         (b-chrom (citizen-chromasome b))
         (pivot (random chromasome-bits))
         (a-chrom : Chromasome (vector-append (vector-take a-chrom pivot) (vector-drop b-chrom pivot)))
         (b-chrom : Chromasome (vector-append (vector-take b-chrom pivot) (vector-drop a-chrom pivot))))
    (list (citizen (chromasome->x a-chrom) (chromasome->y b-chrom) a-chrom)
          (citizen (chromasome->x a-chrom) (chromasome->y b-chrom) b-chrom))))

;; TODO: fix weighted random selection to not rely on positive integers
(: fitness (-> citizen Exact-Positive-Integer))
(define (fitness c)
  (let* ((e 2.71828)
         (x (citizen-x c))
         (y (citizen-y c))
         (raw (- (* (expt e (- (- (expt x 2))
                               (expt (+ y 1) 2)))
                    (expt (- 1 x) 2))
                 (* (expt e (- (- (expt x 2))
                               (expt y 2)))
                    (- x
                       (expt x 3)
                       (expt y 3)))))
         (interpolated (exact-truncate (interpolate (cons -1 1) (cons 1 100) raw))))
    (if (exact-positive-integer? interpolated)
        interpolated
        1)))

(: run (-> Natural Natural Population))
(define (run pop-size generations)
  (evolve
   (make-breed fitness (make-crossover crossover (make-mutate mutate)))
   (random-population pop-size)
   generations
   (lambda (_) (void))))


