#lang typed/racket

(require typed/racket/random)

(define-type Binary-Digit (Refine (n : Integer) (or (= n 1) (= n 0))))
(define-type Binary-List (Listof Binary-Digit))
(define-type Binary-Vector (Vectorof Binary-Digit))
(define-type Probability (Refine (n : Integer) (<= n 100)))

(struct citizen ((value : Number) ; this is gonna be an issue, need to constrain these to integers
                 (chromasome : Binary-List)
                 (fitness : Integer))
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

(: fitness (-> Integer Integer))
(define (fitness x)
  (- (* 15 x) (expt x 2)))

(: assign-fitness (-> citizen citizen))
(define (assign-fitness c)
  (let* ((value : Number (citizen-value c))
         ;; this seems wrong...
         (value : Integer (if (exact-integer? value) value 0)))
    (struct-copy citizen
                 c
                 (fitness (fitness value)))))

(: mutate (-> citizen (#:probability Probability) citizen))
(define (mutate c #:probability (p 1))
  (if (> (random 101) p)
      c
      (let* ((chrom : Binary-Vector (binary-list->binary-vector (citizen-chromasome c)))
             (point : Integer (random (vector-length chrom)))
             (gene : Binary-Digit (vector-ref chrom point))
             (new-gene : Binary-Digit (if (= 1 gene) 0 1)))
        (vector-set! chrom point new-gene)
        (struct-copy citizen
                     c
                     (chromasome (binary-vector->binary-list chrom))))))

(: crossover (-> citizen citizen (#:probability Probability) (Listof citizen)))
(define (crossover a b #:probability (p 70))
  (let* ((a-chrom (citizen-chromasome a))
         (b-chrom (citizen-chromasome b))
         (crossover? : Boolean (> (random 101) p))
         (point (random (length (if (> (length a-chrom) (length b-chrom)) b-chrom a-chrom)))))
    (map (compose assign-fitness mutate)
         (if crossover?
             (let ((a-chrom (append (take a-chrom point) (drop b-chrom point)))
                   (b-chrom (append (take b-chrom point) (drop a-chrom point))))
               (list (citizen (binary-list->number a-chrom) a-chrom 0)
                     (citizen (binary-list->number b-chrom) b-chrom 0)))
             (list a b)))))

(: build-breeder-index-list (-> Population (#:index Integer) (Listof Integer)))
(define (build-breeder-index-list pop #:index (index 0))
  (if (empty? pop)
      '()
      (let* ((c : citizen (first pop))
             (fitness (citizen-fitness c))
             (fitness : Integer (if (< fitness 0) 1 fitness)))
        (append
         (build-list fitness (const index))
         (build-breeder-index-list (rest pop) #:index (add1 index))))))

(: select-indexes (-> (Listof Integer) (Values Integer Integer)))
(define (select-indexes is)
  (let ((a (random-ref is)))
    (: rec (-> Integer))
    (define (rec)
      (let ((b (random-ref is)))
        (if (= a b)
            (rec)
            b)))
    (values a (rec))))

(: breed (-> Population Population))
(define (breed pop)
  (let ((breeder-index-list (build-breeder-index-list pop))
        (pop (list->vector pop)))
    (: rec (-> Population Population))
    (define (rec new-pop)
      (if (= (length new-pop) (vector-length pop))
          new-pop
          (let-values (((a b) (select-indexes breeder-index-list)))
            (rec
             (append
              (crossover (vector-ref pop a) (vector-ref pop b))
              new-pop)))))
    (rec '())))

(: random-population (-> Natural Population))
(define (random-population size)
  (if (= size 0)
      '()
      (cons
       (let ((n (random 50)))
         (citizen n (natural->binary-list n) (fitness n)))
       (random-population (sub1 size)))))

;; TODO (later): evolution stops when the average fitness is sufficiently close to the max fitness (local maximum found)
(: evolve (-> Natural (#:population-size Natural) Population))
(define (evolve generations #:population-size (pop-size 50))
  (if (zero? generations)
      (random-population pop-size)
      (breed (evolve (sub1 generations)))))

;; TODO (next): factor things like fitness and crossover to be high order so they can take in
;; fns to handle the details
;; the idea being this becomes a package that can be used by war-racket to handle evolution
;; in a generic way (this may mean handling fitness only in breed?)
