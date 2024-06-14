#|

Oppgave 1f
(0 42 #t bar)

(car (cdr '(0 42 #t bar)))

Bruker da cdr for å hoppe ut av det forste elementet for saa hente ut
42 ved bruk av car. 

Oppgave 1g
((0 42) (#t bar))

(car (cdr (car ((0 42) (#t bar)))))

Bruker car til aa hente ut (0 42). For saa hente ut det andre elementet
ogsaa car til å hente ut 42

Oppgave 1h
((0) (42 #t) (bar))

(car (car (cdr ((0) (42 #t) (bar)))))

Bruker da cdr for aa hente ut to siste delene for saa bruke car for aa
hente ut den forste delen ogsaa bruke car igjen til aa hente ut det
forste elementet som er da 42.

Oppgave 1i
(cons (cons 0 42) (cons #t bar))
(list (list 0 42) (list #t bar))
|#

;;Oppgave 2a
(define (take n items)
  (if (or (null? items) (zero? n))
      '()
      (cons (car items) (take (- n 1) (cdr items)))))

(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())
(newline)

;;Oppgave 2b
(define (take-b n items)
  (define (take-hale-rek n items ut)
    (if (or (null? items) (zero? n))
        (reverse ut)
        (take-hale-rek (- n 1) (cdr items) (cons (car items) ut))))
  (take-hale-rek n items '()))
        

(take-b 3 '(a b c d e f))
(take-b 1 '(a b c d e f))
(take-b 4 '(a b))
(take-b 4 '())
(newline)

;;Oppgave 2c
(define (take-while pred items)
  (define (hjelpe-pros resultat items)
    (if (null? items)
        (reverse resultat)
        (if (pred (car items))
            (hjelpe-pros (cons (car items) resultat) (cdr items))
            (reverse resultat))))
  (hjelpe-pros '() items))

(take-while even? '(2 34 42 75 88 103 250))
(take-while odd? '(2 34 42 75 88 103 250))
(take-while (lambda (x) (< x 100)) '(2 34 42 75 88 103 250))
(newline)
        

;;Oppgave 2d
(define (map2 proc list1 list2)
  (if (or (null? list1) (null? list2))
  '()
  (cons (proc (car list1) (car list2))
        (map2 proc (cdr list1) (cdr list2)))))

(map2 + '(1 2 3 4) '(3 4 5))
(newline)

;;Oppgave 2e
;;lambda uttrykket tar i mot to argumenter for å beregne snittet, legge sammen
;;og dele det på to.

(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))


