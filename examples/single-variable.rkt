#lang typed/racket

(require typed/racket/random
         "../ga.rkt")

(define-type Binary-Digit (Refine (n : Integer) (or (= n 1) (= n 0))))
(define-type Binary-List (Listof Binary-Digit))
(define-type Binary-Vector (Vectorof Binary-Digit))

(struct citizen ((value : Number) ; this is gonna be an issue, need to constrain these to integers
                 (chromasome : Binary-List))
  #:transparent)

(define-type Population (Listof citizen))

(: natural->binary-list (-> Natural Binary-List))
(define (natural->binary-list n)
  (map
   (lambda ((sn : Char)) : Binary-Digit
     (cond
       ((char=? sn #\1) 1)
       ((char=? sn #\0) 0)
       (else 0)))
   (string->list (format "~b" n))))

;; this should constrain to Natural...
(: binary-list->number (-> Binary-List Number))
(define (binary-list->number bl)
  (apply
   +
   (foldr
    (lambda ((d : Binary-Digit) (l : (Listof Number))) : (Listof Number)
      (cons (* (expt 2 (length l)) d) l))
    '()
    bl)))

(: binary-list->binary-vector (-> Binary-List Binary-Vector))
(define (binary-list->binary-vector l)
  (list->vector l))

(: binary-vector->binary-list (-> Binary-Vector Binary-List))
(define (binary-vector->binary-list v)
  (vector->list v))

(: fitness- (-> Integer Exact-Positive-Integer))
(define (fitness- x)
  (let ((fitness (- (* 15 x) (expt x 2))))
    (if (exact-positive-integer? fitness)
        fitness
        1)))

(: fitness (-> citizen Exact-Positive-Integer))
(define (fitness c)
  (let* ((value : Number (citizen-value c))
         ;; this seems wrong...
         (value : Integer (if (exact-integer? value) value 1)))
    (fitness- value)))

(: random-population (-> Natural Population))
(define (random-population size)
  (if (= size 0)
      '()
      (cons
       (let ((n (random 50)))
         (citizen n (natural->binary-list n)))
       (random-population (sub1 size)))))

(: mutate (-> citizen citizen))
(define (mutate c)
  (let* ((chrom : Binary-Vector (binary-list->binary-vector (citizen-chromasome c)))
         (point : Integer (random (vector-length chrom)))
         (gene : Binary-Digit (vector-ref chrom point))
         (new-gene : Binary-Digit (if (= 1 gene) 0 1)))
    (vector-set! chrom point new-gene)
    (let ((chrom : Binary-List (binary-vector->binary-list chrom)))
      (citizen (binary-list->number chrom)
               chrom))))

(: crossover (-> citizen citizen (Listof citizen)))
(define (crossover a b)
  (let* ((a-chrom (citizen-chromasome a))
         (b-chrom (citizen-chromasome b))
         (point (random (length (if (> (length a-chrom) (length b-chrom)) b-chrom a-chrom))))
         (a-chrom (append (take a-chrom point) (drop b-chrom point)))
         (b-chrom (append (take b-chrom point) (drop a-chrom point))))
    (list (citizen (binary-list->number a-chrom) a-chrom)
          (citizen (binary-list->number b-chrom) b-chrom))))

(: run (-> Population))
(define (run)
  (evolve
   (make-breed fitness (make-crossover crossover (make-mutate mutate)))
   (random-population 200)
   10
   (lambda (_) (void))))
