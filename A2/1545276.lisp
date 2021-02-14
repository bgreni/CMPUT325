(defun fl-interp (E P)
; just cause interp is shorter
  (interp E P)
)

(defun interp (E P)
  (cond
	((atom E)
    ; check if expression is true or false, otherwise return expressions as is 
    (cond
      ((equal E 'true) T)
      ((equal E 'false) NIL)
      (T E))
       )   ;this includes the case where E is nil or a number
    (T
      (let ( (f (first E))  (arg (rest E)) )
        (cond 
          ; handle built-in functions
          ((equal f 'first)           
            (first (interp (first arg) P)))

          ((equal f 'rest) 
            (rest (interp (first arg) P)))

          ((equal f 'if) 
            (if (interp (first arg) P) 
              (interp (second arg) P)
                (interp (third arg) P)))

          ((equal f 'null) 
            (null (interp (first arg) P)))

          ((equal f 'atom) 
            (atom (interp (first arg) P)))

          ((equal f 'eq)
            (eq (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'equal) 
            (equal (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'cons) 
            (cons (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'number) 
            (numberp (interp (first arg) P)))

          ((equal f '+) 
            (+ (interp (first arg) P) (interp (second arg) P)))

          ((equal f '-) 
            (- (interp (first arg) P) (interp (second arg) P)))

          ((equal f '*) 
            (* (interp (first arg) P) (interp (second arg) P)))

          ((equal f '>) 
            (> (interp (first arg) P) (interp (second arg) P)))

          ((equal f '<) 
            (< (interp (first arg) P) (interp (second arg) P)))

          ((equal f '=) 
            (= (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'and) 
            (and (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'or) 
            (or (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'not) 
            (not (interp (first arg) P)))


          (T
            (let ((def (user-defined f P)))
              (cond
                ((null def) 
                  E)
                (T 
                  (let ((pairs (make-pairs (second def) arg)) (bod (get-body def)))
                    (let ((replaced (reps pairs bod pairs)))
                      (interp replaced P))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)




	        ; if f is a user-defined function,
                ;    then evaluate the arguments 
                ;         and apply f to the evaluated arguments 
                ;             (applicative order reduction) 
                

                ; otherwise f is undefined (not intended to be a function),
                ; the E is returned, as if it is quoted in lisp 

(defun make-pairs (X Y)
  (if (null X)
    NIL
    (cons (cons (first X) (first Y)) (make-pairs (rest X) (rest Y)))
  )
)

(defun get-body (def)
  (cond
    ((null def) 
      NIL)
    ((equal (first def) '=)
      (first (rest def)))
    (T (get-body (rest def)))
  )
)

(defun find_pair (Y X)
  (if (null Y)
    NIL
      (if (equal (caar Y) X)
        (first Y)
        (find X (rest Y))
    )
  )
)


(defun reps (nv L fullnv)
  (cond
    ((null nv) 
      L)
    (T (reps (rest nv) (rep (caar nv) (cdar nv) L) fullnv))
  )
)


(defun rep (name val L)
  (cond
    ((not (atom (first L)))
      (cons (rep name val (first L)) (rep name val (rest L))))
    ((equal name (first L)) 
      (cons val (rep name val (rest L))))
    ((atom L) L)
    (T 
      (cons (first L) (rep name val (rest L))))
  )
)


(defun user-defined (f P)
  (cond 
    ((null P) NIL)
    ((equal f (caar P)) (first P))
    (T (user-defined f (rest P)))
  )


)

(defun xmember (X Y)
  (cond
    ((atom Y)
      NIL)
    ((equal X (car Y)) 
      T)
    ((null Y) 
      NIL)
    (T 
      (xmember X (cdr Y)))
  )
)




; TEST SUITE
;; ; PRIMITIVES
(assert (equal (interp '(first (8 5 16)) NIL) '8))
(assert (equal (interp '(rest (8 5 16)) NIL) '(5 16)))
(assert (equal (interp '(if false 2 3) NIL) '3))
(assert (equal (interp '(null ()) NIL) t))
(assert (equal (interp '(atom (3)) NIL) NIL))
(assert (equal (interp '(eq x x) nil) t))
(assert (equal (interp '(cons 6 3) nil) '(6 . 3)))
(assert (equal (interp '(number 354) nil) t))
(assert (equal (interp '(+ 10 5) nil) 15))
(assert (equal (interp '(- 12 8) nil) 4))
(assert (equal (interp '(* 5 9) nil) 45))
(assert (equal (interp '(> 2 3) nil) nil))
(assert (equal (interp '(< 1 131) nil) t))
(assert (equal (interp '(= 88 88) nil) t))
(assert (equal (interp '(and false true) nil) nil))
(assert (equal (interp '(or true false) nil) t))
(assert (equal (interp '(not true) nil) nil))
(assert (equal (interp '(equal (3 4 1) (3 4 1)) nil) t))

; Compound Primitives
(assert (equal (interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) 12))
(assert (equal (interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2))) (not (= 3 2))) nil) t))
(assert (equal (interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil) nil))
(assert (equal (interp '(if (not (null (first (a c e)))) (if (number (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) '((a c e) . d)))

; USER DEFINED
(assert (equal (interp '(greater 3 5) '((greater (x y) = (if (> x y) x (if (< x y) y nil))))) 5))
(assert (equal (interp '(square 4) '((square (x) = (* x x)))) 16))
(assert (equal (interp '(simpleinterest 4 2 5) '((simpleinterest (x y z) = (* x (* y z))))) 40))
(assert (equal (interp '(xor true false) '((xor (x y) = (if (equal x y) false true)))) t))
(assert (equal (interp '(cadr (5 1 2 7)) '((cadr(x) = (first (rest x))))) 1))

; COMPLEX USER DEFINED
(assert (equal (interp '(last (s u p)) '((last(x) = (if (null (rest x)) (first x) (last (rest x)))))) 'p))
(assert (equal (interp '(push (1 2 3) 4) '((push (x y) = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4)))
(assert (equal (interp '(pop (1 2 3)) '((pop(x) = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) '(1 2)))
(assert (equal (interp '(factorial 4) '((factorial(x) = (if (= x 1) 1 (* x (factorial (- x 1))))))) '24))
(assert (equal (interp '(divide 24 4) '((divide (x y) = (div x y 0)) (div (x y z) = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) '6))

; MY HELPERS
(assert (equal (user-defined 'greater '((greater (x y) = (if (> x y) x (if (< x y) y nil))))) '(greater (x y) = (if (> x y) x (if (< x y) y nil)))))
(assert (equal (user-defined 'greater '((nothing (x y) = (if (> x y) x (if (< x y) y nil))))) NIL))
(assert (equal (get-body '(greater (x y) = (if (> x y) x (if (< x y) y nil)))) '(if (> x y) x (if (< x y) y nil))))
(assert (equal (rep 'x 1 '(x y z x)) '(1 y z 1)))
(assert (equal (reps '((x . 1)) '(x x z y) '((x . 1))) '(1 1 z y)))
(assert (equal (reps '((x . 1) (y . 2) (z . 3)) '(x x z y) '((x . 1) (y . 2) (z . 3))) '(1 1 3 2)))
(assert (equal (reps '((x . 1) (y . 2) (z . 3)) '(x x z (x y)) '((x . 1) (y . 2) (z . 3))) '(1 1 3 (1 2))))
(assert (equal (find_pair '((x . 1) (y . 2)) 'x) '(x . 1)))
(assert (equal (make-pairs '(x y z) '(1 2 3)) '((x . 1) (y . 2) (z . 3))))
(terpri)