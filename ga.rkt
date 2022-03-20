#lang typed/racket

(provide Probability
         Breed
         Crossover
         Fitness
         Mutate
         make-mutate
         make-crossover
         make-breed
         evolve)

(require typed/racket/random)

(define-type Probability (Refine (n : Integer) (<= n 100)))

(define-type Breed (All (citizen) (-> (Listof citizen) (Listof citizen))))
(define-type Crossover (All (citizen) (-> citizen citizen (Listof citizen))))
(define-type Fitness (All (citizen) (-> citizen Exact-Positive-Integer)))
(define-type Mutate (All (citizen) (-> citizen citizen)))

(: make-mutate (All (citizen) (-> (Mutate citizen) (#:probability Probability) (Mutate citizen))))
(define (make-mutate mutate #:probability (p 1))
  (lambda ((c : citizen)) : citizen
    (if (> (random 101) p)
        c
        (mutate c))))

(: make-crossover (All (citizen) (-> (Crossover citizen) (Mutate citizen) (#:probability Probability) (Crossover citizen))))
(define (make-crossover crossover mutate #:probability (p 70))
  (lambda ((a : citizen) (b : citizen) ) : (Listof citizen)
    (let* ((crossover? : Boolean (> (random 101) p)))
      (map mutate
           (if crossover?
               (crossover a b)
               (list a b))))))

(: build-breeder-index-list (All (citizen) (-> (Fitness citizen) (Listof citizen) (Listof Integer))))
(define (build-breeder-index-list fitness pop)
  (: rec (-> (Listof citizen) Integer (Listof Integer)))
  (define (rec pop index)
    (if (empty? pop)
        '()
        (let* ((c : citizen (first pop))
               (fitness : Exact-Positive-Integer (fitness c)))
          (append
           (build-list fitness (const index))
           (rec (rest pop) (add1 index))))))
  (rec pop 0))

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

(: make-breed (All (citizen) (-> (Fitness citizen) (Crossover citizen) (Breed citizen))))
(define (make-breed fitness crossover)
  (lambda ((pop : (Listof citizen))) : (Listof citizen)
    (let ((breeder-index-list (build-breeder-index-list fitness pop))
          (pop (list->vector pop)))
      (: rec (-> (Listof citizen) (Listof citizen)))
      (define (rec new-pop)
        (if (= (length new-pop) (vector-length pop))
            new-pop
            (let-values (((a b) (select-indexes breeder-index-list)))
              (rec
               (append
                (crossover (vector-ref pop a) (vector-ref pop b))
                new-pop)))))
      (rec '()))))

;; TODO (later): evolution stops when the average fitness is sufficiently close to the max fitness (local maximum found)
(: evolve (All (citizen) (-> (Breed citizen) (Listof citizen) Natural (Listof citizen))))
(define (evolve breed initial-population generations)
  (: rec (-> Natural (Listof citizen)))
  (define (rec generations)
    (if (zero? generations)
        initial-population
        (breed (rec (sub1 generations)))))
  (rec generations))

;; TODO (next): factor things like fitness and crossover to be high order so they can take in
;; fns to handle the details
;; the idea being this becomes a package that can be used by war-racket to handle evolution
;; in a generic way (this may mean handling fitness only in breed?)
