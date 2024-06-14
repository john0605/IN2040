;Gruppe: John Vadsan(vadsanjs), Marius Warlo(mariusiw) og Tobias Mortensen(tobiashm)
;Oppgave 1
;a)
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c1)
(c1)
count
(c2)

;b)
; Ligger i pdf-en som er levert ved siden av

;Oppgave 2
;a)
(define (make-stack elements)
  (let ((stack elements))
    (lambda (m . arguments)
      (cond
        ((eq? m 'pop!) (if (null? stack)
                           (display "")
                           (set! stack (cdr stack))))
        ((eq? m 'push!) (set! stack (append (reverse arguments) stack)))
        ((eq? m 'stack) stack)))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

;b)
(define (push! stackobj . arguments)
  (define (pushone elements)
    (stackobj 'push! (car elements))
    (if (null? (cdr elements))
        (display "")
        (pushone (cdr elements))))
  (pushone arguments))

(define (pop! stackobj)
  (stackobj 'pop!))

(define (stack stackobj)
  (stackobj 'stack))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)


;Oppgave 3
;a)
;Oppgaven ligger i en pdf fil ved siden av.

;b)
;Oppgaven ligger i en pdf fil ved siden av.

;c)

(define (cycle? list)
  (let ((teller_node '()))
    (let loop ((node list))
      (cond
        ((null? node) #f)
        ((memq node teller_node) #t)
        (else
          (set! teller_node (cons node teller_node))
          (loop (cdr node)))))))

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))

(display (cycle? '(hey ho))) ; Resultat: #f
(newline)

(display (cycle? '(la la la))) ; Resultat: #f
(newline)

(display (cycle? bah)) ; Resultat: #f
(newline)

(display (cycle? bar)) ; Resultat: #t
(newline)


;d)
#|

Naar det kommer til (list? bar) saa vil den returnere false. Dette er nemlig fordi at dette ikke er en
ekte liste. I aa med at set-cdr blir brukt her saa vil den bli satt opp som sirkulaer. Her sjekkes det
ikke om det faktisk har en ende. Det pekes tilbake til cdr bar. Saa det gaar det til aa vaere syklisk.
Noe som gjor det ikke til aa vaere en ekte liste. 

Hos (list? bah) saa vil det returneres true, dette er fordi at det blir brukt set-car. Her er det slik at
det ikke blir til en sirkulaer struktur noe som gjor at det faktisk blir til en ekte liste. Og da vil det
returneres true. 

|#












