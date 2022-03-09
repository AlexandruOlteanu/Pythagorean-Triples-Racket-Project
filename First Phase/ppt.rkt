#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (cond 
    [(null? X) 0]
    [else (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))] 
  )
)

; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (multiply-help M V null)
)


(define (multiply-help M V List)
    (cond 
      [(null? M) List]
      [else (multiply-help (cdr M) V (append List (list (dot-product (car M) V))))]
    )
)

; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

(define (max a b)
  (cond 
    [(>= a b) a]
    [(< a b) b]
  )
)

(define (my-power a b Ans)
  (cond 
    [(zero? b) Ans]
    [else (my-power a (- b 1) (* Ans a))]
  )
)

(define (find-level n Ans)
   (cond 
     [(>= (quotient (- (my-power 3 Ans 1) 1) 2) n) 0]
     [else (max Ans (find-level n (+ Ans 1)))]
   )
)

(define (calculate-mod a b)
  (cond 
    [(= (modulo a b) 0) b]
    [else (modulo a b)]
  )
)

(define (calculate-group a b)
  (cond 
    [(= (modulo a b) 0) (quotient a b)]
    [else (+ (quotient a b) 1)]
  )
)

(define (calculate-route n List Level)
   (cond
    [(= Level 0) List]
    [else (calculate-route (calculate-group n 3) (cons (calculate-mod n 3) List) (- Level 1))]
   )
)

(define (get-transformations n)
  (calculate-route (- n (quotient (- (my-power 3 (find-level n 0) 1) 1) 2)) null (find-level n 0))
)


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (cond 
    [(null? Ts) ppt]
    [(= (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt))]
    [(= (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt))]
    [(= (car Ts) 3) (apply-matrix-transformations (cdr Ts) (multiply T3 ppt))]
  )
)


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5))
)
