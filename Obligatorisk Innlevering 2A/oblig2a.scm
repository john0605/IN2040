(load "huffman.scm")

;;Oppgave 2A
;;Halerekursiv versjon av decode

(define (decode-halerek bits tree)
  (define (decode-x bits current-branch out)
    (if (null? bits)
        (reverse out)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-x (cdr bits) tree (cons (symbol-leaf next-branch) out))
              (decode-x (cdr bits) next-branch out)))))
  (decode-x bits tree '()))

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))

;;Oppgave 2B
(decode-halerek sample-code sample-tree)

#|
resultatet av å kalle prosedyren decode blir da:
samurais fight ninjas by night
|#

;;Oppgave 2C

(define (encode melding tre)
  (if (null? melding)
      '()
      (append (encode-symbol (car melding) tre)
              (encode (cdr melding) tre))))

(define (encode-symbol sym tre)
  (if (leaf? tre)
      '()
      (let* ((left (symbols (left-branch tre)))
             (bit (if (memq sym left) 0 1)))
        (cons bit (encode-symbol sym (choose-branch bit tre))))))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)