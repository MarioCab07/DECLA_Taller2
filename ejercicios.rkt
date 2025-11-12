#lang racket

(define (contar-positivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))

(displayln "1) contar-positivos:")
(display (contar-positivos '(3 -2 7 0 -5 9)))
(display " elementos positivos")
(newline)
(newline)

(define (cuadrados-pares lista)
  (map (lambda (x) (expt x 2))
       (filter (lambda (x) (even? x)) lista)))

(displayln "2) cuadrados-pares:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8)))
(newline)


(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


(displayln "3) factorial:")
(displayln (factorial 5))
(newline)


(define (elevar-cubo lista)
  (map (lambda (x) (expt x 3)) lista))

(displayln "4) elevar-cubo:")
(displayln (elevar-cubo '(2 3 4)) )
(newline)

(define (sumar-impares lista)
  (foldl + 0 (filter (lambda (x) (odd? x)) lista)))

(displayln "5) sumar-impares:")
(displayln (sumar-impares '(1 2 3 4 5 6 7)))
(newline)

(define (contiene-negativos lista)
  (ormap (lambda (x) (< x 0)) lista))

(displayln "6) contiene-negativos:")
(displayln (contiene-negativos '(5 9 -3 2)))
(newline)

(define (suma-acumulada lista)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lista)))

(displayln "7) suma-acumulada:")
(displayln (suma-acumulada '(1 2 3 4)))
(newline)

(define (concatenar-cadenas lista)
  (foldl (lambda (elem acc) (string-append acc elem))
         ""
         lista))

(displayln "8) concatenar-cadenas:")
(displayln (concatenar-cadenas '("Hola" " " "Mundo")))
(newline)

(define (dobles-mayores-que5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

(displayln "9) dobles-mayores-que5:")
(displayln (dobles-mayores-que5 '(3 6 8 2 10)))
(newline)


(define (invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

(displayln "10) invertir-lista:")
(displayln (invertir-lista '(1 2 3 4)))
(newline)


;; Función que eleva al cuadrado
(define (cuadrado x)
  (* x x))

;; Función que recibe una función y una lista
(define (aplicar-a-lista f lista)
  (map f lista))

(displayln "11) aplicar-a-lista con cuadrado:")
(displayln (aplicar-a-lista cuadrado '(1 2 3 4)))
(newline)

(define (promedio-mayores-a5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (suma (foldl + 0 mayores)))
    (/ suma (length mayores))))

(displayln "12) promedio-mayores-a5:")
(displayln (promedio-mayores-a5 '(3 8 10 4 9 2 7)))
(newline)
