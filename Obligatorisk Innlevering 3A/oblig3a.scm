;Gruppe: John Vadsan(vadsanjs), Marius Warlo(mariusiw) og Tobias Mortensen(tobiashm)
(load "prekode3a.scm")

;Oppgave 1
;a og b)
(define mem
  (let ((procedure-table (make-table)))
    (lambda (m f)
      (cond ((eq? m 'memoize)
             (let* ((table (make-table))
                    (memorized
                     (lambda x
                       (or (lookup x table)
                           (let ((result (apply f x)))
                             (insert! x result table)
                             result)))))
               (insert! memorized f procedure-table)
               memorized))
            ((eq? m 'unmemoize)
             (lookup f procedure-table))))))

(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)

(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)

;c)
#|

Problemet her er at vi binder den memoiserte prosedyren til variabelen mem-fib,
og ikke til variabelen fib. Da overskriver vi ikke fib-kallet i den originale
prosedyren fib, og den ville ikke rekursivt kalle en memoisert versjon av fib,
men heller den originale. Dermed lagres kun verdien av kallet (mem-fib 3), men
ikke verdiene av alle de andre kallene i evaluasjonen (dvs. (fib 2) kalles,
ikke (mem-fib 2), og dermed er ikke verdien av (mem-fib 2) lagret.

|#
;Oppgave 2
;a)
;konvertering av lister til strømmer
(define (list-to-stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list-to-stream cdr list))))

(list-to-stream '(1 2 3 4 5))

;konvertering av strømmer til lister
(define (stream-to-list stream . n)
  (define (stl-rec stream i)
    (if (or (stream-null? stream) (= i 0))
            '()
            (cons (stream-car stream) (stl-rec (stream-cdr stream) (- i 1)))))
    (stl-rec stream (if (null? n) -1 (car n))))

(stream-to-list (stream-interval 10 20))
(stream-to-list nats 10)
(show-stream nats 15)

;b)
(define (stream-take n stream)
  (if (or (stream-null? stream) (= n 0))
      the-empty-stream
      (cons-stream (stream-car stream)
                   (stream-take (- n 1) (stream-cdr stream)))))

(define foo (stream-take 10 nats))
foo

(show-stream foo 5)

(show-stream foo 20)

(show-stream (stream-take 15 nats) 10)
;c)
#|
Et potensielt problem vil være å bytte null? med stream-null?. I Petter Smart sitt forslag til endring vil funksjonen remove-duplicate aldri terminere for uendelige strømmer,
siden det ikke er noen endelig sluttbetingelse. Å sjekke med null? gjøres for å avgrense rekursjonen og sørge for at den stopper en gang.
Siden Petter Smart foreslår å bytte ut null? med stream-null? vil man få uendelig rekrusjon og en ugenerert uendelig liste.
|#

;d)
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter
                     (lambda (x)
                       (not (eq? x (stream-car stream))))
                     (stream-cdr stream))))))