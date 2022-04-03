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

(define-type Probability (Refine (n : Integer) (and (<= n 100) (>= n 0))))

(define-type Breed (All (citizen) (-> (Listof citizen) (Listof citizen))))
(define-type Crossover (All (citizen) (-> citizen citizen (Listof citizen))))
(define-type Fitness (All (citizen) (-> citizen Integer)))
(define-type Mutate (All (citizen) (-> citizen citizen)))
(define-type Grapher (All (citizen) (-> (Listof citizen) Void)))

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

(: build-weighted-selector (All (citizen) (-> (Fitness citizen) (Listof citizen) (Pairof Integer (-> Integer citizen)))))
(define (build-weighted-selector fitness pop)
  (foldl
   (lambda ((fitness-weight : Integer) (c : citizen) (ret : (Pairof Integer (-> Integer citizen))))
     (let* ((offset : Integer (car ret))
            (upper-bound (+ offset fitness-weight))
            (next-handler : (-> Integer citizen) (cdr ret)))
       (cons (+ offset fitness-weight)
             (lambda ((i : Integer)) : citizen
               (if (and (>= i offset) (< i upper-bound))
                   c
                   (next-handler i))))))
   (cons 0 (lambda ((i : Integer)) : citizen (first pop)))
   (map (lambda ((c : citizen)) : Integer (fitness c)) pop)
   pop))

(: make-breed (All (citizen) (-> (Fitness citizen) (Crossover citizen) (Breed citizen))))
(define (make-breed fitness crossover)
  (lambda ((pop : (Listof citizen))) : (Listof citizen)
    (let* ((weighted-selector-info : (Pairof Integer (-> Integer citizen)) (build-weighted-selector fitness pop))
           (max : Integer (car weighted-selector-info))
           (selector : (-> Integer citizen) (cdr weighted-selector-info)))
      (: rec (-> (Listof citizen) (Listof citizen)))
      (define (rec new-pop)
        (if (= (length new-pop) (length pop))
            new-pop
            (let ((a : citizen (selector (random max)))
                  (b : citizen (selector (random max))))
              (rec
               (append
                (crossover a b)
                new-pop)))))
      (rec '()))))

;; TODO (later): evolution stops when the average fitness is sufficiently close to the max fitness (local maximum found)
(: evolve (All (citizen) (-> (Breed citizen) (Listof citizen) Natural (#:grapher (Grapher citizen)) (Listof citizen))))
(define (evolve breed initial-population generations #:grapher (grapher (lambda ((population : (Listof citizen))) (void))))
  (: rec (-> Natural (Listof citizen)))
  (define (rec generations)
    (if (zero? generations)
        (begin
          (grapher initial-population)
          initial-population)
        (let ((new-pop (breed (rec (sub1 generations)))))
          (grapher new-pop)
          new-pop)))
  (rec generations))
