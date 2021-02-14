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


          (T E
            ;; (let (def (user-defined f P)
            ;;   (cond
            ;;     ((null def) E)
            ;;     (T E)
            ;;   ))
            ;; )
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

(defun get-body (def)
  (cond
    ((null def) 
      NIL)
    ((equal (first def) '=)
      (first (rest def)))
    (T (get-body (rest def)))
  )
)

;; (defun replace (f def)

;; )


(defun user-defined (f P)
  (cond 
    ((null P) NIL)
    ((xmember f (first P)) (first P))
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
; PRIMITIVES
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

(assert (equal (user-defined 'greater '((greater (x y) = (if (> x y) x (if (< x y) y nil))))) '(greater (x y) = (if (> x y) x (if (< x y) y nil)))))
(assert (equal (user-defined 'greater '((nothing (x y) = (if (> x y) x (if (< x y) y nil))))) NIL))
(assert (equal (get-body '(greater (x y) = (if (> x y) x (if (< x y) y nil)))) '(if (> x y) x (if (< x y) y nil))))

(terpri)