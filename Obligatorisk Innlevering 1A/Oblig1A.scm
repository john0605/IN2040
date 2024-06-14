;; Oppgave 1A
;;(* (+ 4 2) 5)
;; Følgende uttrykk vil kunne gi oss verdien 30. Noe som tilsvarer det som er forventet. Dette er fordi at
;; (4+2)5 vil kunne gi oss svaret 30. 

;; Oppgave 1B
;; (* (+ 4 2) (5))
;; Dette vil kunne gi oss feil. Det er fordi at dette ikke er en prosedyre. Vi ser at tallet 5 ligger mellom
;; i parantesen, der skal det egentlig ikke være en ekstra parantes. 

;; Oppgave 1C
;; (* (4 + 2) 5)
;; Følgende uttrykk vil også kunne gi oss feil, her er det slik at første elementet er operandene altså
;; argumentene. Men her skal egentlig operatøren komme først.

;; Oppgave 1D
;; (define bar (/ 44 2))
;; bar
;; Følgende uttrykk vil kunne gi meg svaret 22. Noe som er forventet. Og den parantesbaserte prefiksnotasjonen
;; blir ivaretatt.

;; Oppgave 1E
;; (- bar 11)
;; Følgende uttrykk gir oss svaret 11. Dette er også noe som er forventet. For 44 delt på 2 gir oss 22 og når
;; vi trekker 11 fra dette så får vi riktig svar.

;; Oppgave 1F
;; (/ (* bar 3 4 1) bar)
;; Følgende uttrykk gir oss svaret 12 noe som er riktig. Prefiksnotasjonene her er også riktig.

;; Oppgave 2A
(or (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))
#|
Første uttrykket gir oss true med "paff" med engang i å med at dette er deres special behavior. I å med at den ene ikke
er false, så bare gir den hele uttrykket true.
|#


(and (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))
#|
Andre uttrykket jeg oss false. Dette er riktig i å med at dersom det er en false i uttrykket så vil hele bli
satt til false, altså hele uttrykket. 
|#


(if (positive? 42)
    "poff!"
    (i-am-undefined))
#|
Tredje uttrykket gir oss svaret poff!. Det skjer dersom 42 er postiv så vil kunne vi få poff! som svare. Og det
er nettop det som skjer. I å med at 42 er positiv, og gir ikke videre til else setningen. 
|#

;; Oppgave 2B
;; if

(define (sign x)
  (if (positive? x)
     1
     (if (negative? x)
        -1
        0)))

;; cond
(define (sign x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

;; Oppgave 2C
(define (sign x)
  (or (and positive? x)
      (and negative? x)
      (and zero? x)))


;; Oppgave 3A

(define (leggtil x)
  (+ x 1))

(define (trekkfra x)
  (- x 1))

;; Oppgave 3B

(define(plusRec x y)
  (if (zero? y)
      x
      (plusRec (leggtil x) (trekkfra y))))

;; Oppgave 3C

;; Oppgave 3D

;; Oppgave 3E


