;Gruppe: John Vadsan(vadsanjs), Marius Warlo(mariusiw) og Tobias Mortensen(tobiashm)

(load "evaluator.scm")

;Oppgave 1a
#|
(foo 2 square)
Her er det slik at cond blir knyttet til verdien 2 hos foo og det første uttrykket vil da kunne
returnere 0.

(foo 4 square)
Her blir vil cond falle videre til else. og den har verdien 4 knyttet til seg og ganges da videre
med 4 og vil gi returverdien 16.

(cond ((= cond 2) 0)
      (else (else 4)))
conden som er i kroppen blir knyttet til verdien 3. Siden 2 ikke er lik 3 så vil den gå videre til neste else
som vil ta et tall og dele den på 2. og videre som vil gi oss verdien 2. 
|#

;Oppgave 2a
;Ligger inne i evaluator.scm. 
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+ (lambda (x) (+ x 1)))
        (list '1- (lambda (x) (- x 1)))))

;Oppgave 2b
(set! the-global-environment (setup-environment))

(define (install-primitive! navn proc)
  (let ((inp (list 'primitive proc)))
    (define-variable! navn inp the-global-environment)))

(install-primitive! 'square (lambda (x) (* x x)))
(mc-eval '(square 4) the-global-environment)


;Oppgave 3a
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ;La til and og or til special forms som skal evalueres
        ((and? exp) (eval-and exp))
        ((or? exp) (eval-or exp))
        ((cond? exp) (mc-eval (cond->if exp) env))))

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;La til and og or som special forms
        ((and? exp) #t)
        ((or? exp) #t)
        (else #f)))

;Sjekker om and ligger i exp
(define (and? exp) (tagged-list? exp 'and))

;Sjekker om or er i exp
(define (or? exp) (tagged-list? exp 'or))

;Evaluerer and
(define (eval-and exp)
  ;Definerer en indre funksjon så jeg får fjernet and fra exp
  (define (and-evaluator exp)
    (cond ((null? exp) #t)
          ((null? (cdr exp)) (car exp))
          (else (if (car exp) (and-evaluator (cdr exp)) #f))))
  (and-evaluator (cdr exp)))

;Evaluerer or
(define (eval-or exp)
  ;Definerer en indre funksjon så jeg får fjernet or fra exp
  (define (or-evaluator exp)
    (cond ((null? exp) #f)
          ((null? (cdr exp)) (car exp))
          (else (if (car exp) #t (eval-or (cdr exp))))))
  (or-evaluator (cdr exp)))


;Oppgave 3b
(define (if? exp) (tagged-list? exp 'if))
(define (elsif? exp) (tagged-list? exp 'elsif))
(define (else? exp) (tagged-list? exp 'else))
(define (predicate exp) (cadr exp))
(define (consequent exp) (cadddr exp))
(define (alternative exp) (cddddr exp))

(define (eval-if exp env)
  (cond ((if? exp) (mc-eval (predicate exp) env))
        ((elsif? exp) (mc-eval (consequent exp) env))
        ((else? exp) (mc-eval (alternative exp) env))
        (else (eval-if (alternative exp) env))))


;Oppgave 3c



;Oppgave 3d


(read-eval-print-loop)