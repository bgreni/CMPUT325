(defun fl-interp (E P)
; just cause interp is shorter
  (interp E P)
)

(defun interp (E P)
  "
  Interprets and expresion E
  P is a list of user defined function definitions that my appear in the given expression
  "
  (cond
    ; handle case of atomic expression
    ((atom E)
      ; check if expression is true or false, otherwise return expressions as is 
      (cond
        ((equal E 'true) T)
        ((equal E 'false) NIL)
        (T E)
      )
    )
    (T
      ; split expression into function name and args
      (let ((f (first E))  (arg (rest E)))
        (cond 
          ; handle built-in functions
          ((equal f 'first)          
            ; fetch the car of args 
            (first (interp (first arg) P)))

          ((equal f 'rest) 
            ; fetch the cdr of args
            (rest (interp (first arg) P)))

          ; 
          ((equal f 'if) 
            ; if first arg evals to true
            ; then execute the second arg
            ; else execute third arg
            (if (interp (first arg) P) 
              (interp (second arg) P)
                (interp (third arg) P)))

          ((equal f 'null)
            ; returns T if the arg evals to NIL 
            (null (interp (first arg) P)))

          ((equal f 'atom) 
            ; returns T if arg evals to an atom
            (atom (interp (first arg) P)))

          ((equal f 'eq)
            ; returns T if first and second arg eval to equal objects
            (eq (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'equal)
            ; returns T if the first and second arg eval to be equal 
            (equal (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'cons) 
            ; return cons cell containing result of first and second arg
            (cons (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'number) 
            ; return T if the arg evals to a number
            (numberp (interp (first arg) P)))

          ((equal f '+) 
            ; returns sum of the first and second args
            (+ (interp (first arg) P) (interp (second arg) P)))

          ((equal f '-) 
            ; returns difference of the first and second args
            (- (interp (first arg) P) (interp (second arg) P)))

          ((equal f '*) 
            ; returns product of the first and second args
            (* (interp (first arg) P) (interp (second arg) P)))

          ((equal f '>) 
            ; returns T if the first arg is strictly larger than the second arg
            (> (interp (first arg) P) (interp (second arg) P)))

          ((equal f '<) 
            ; returns T if the first arg is strictly smaller than the second arg
            (< (interp (first arg) P) (interp (second arg) P)))

          ((equal f '=) 
            ; returns T if the first and second arg are equal number
            (= (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'and) 
            ; returns T if the first and second arg are both T
            (and (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'or) 
            ; returns T if first or second arg are T
            (or (interp (first arg) P) (interp (second arg) P)))

          ((equal f 'not)
            ; negates the result of the arg
            (not (interp (first arg) P)))


          (T
            ; handle user defined function
            (let ((def (user-defined f P)))
              (cond
                ; if def is null then this is not a function but a list
                ((null def) 
                  E)
                (T 
                  ; make pairs of arg symbols and their values for this function call
                  (let ((pairs (make-pairs (second def) arg)) (bod (get-body def)))
                    ; replace all instances of arg variable with their values
                    (let ((replaced (reps pairs bod)))
                      ; evaluate the function body
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

(defun make-pairs (X Y)
  "
  Makes (a . b) cons pairs out of two lists X and Y
  where X is a list of symbols, and Y is a list of numbers.
  Used for argument value replacement

  e.g: (make-pairs (x y z) (1 2 3)) -> ((x . 1) (y . 2) (z . 3))
  "
  (if (null X)
    NIL
    (cons (cons (first X) (first Y)) (make-pairs (rest X) (rest Y)))
  )
)

(defun get-body (def)
  "
  Returns the function body given a function definition

  e.g: (get-body '(greater (x y) = (if (> x y) x (if (< x y) y nil))))) -> '(if (> x y) x (if (< x y) y nil))
  "
  (cond
    ((null def) 
      NIL)
    ((equal (first def) '=)
      (first (rest def)))
    (T (get-body (rest def)))
  )
)

(defun reps (nv L)
  "
  replace all instances of argument variables in nv in an arg body L.

  Walks every arg, val pair in nv and replaces all instances of that arg with its val
  in the function body L

  e.g: (reps '((x . 1) (y . 2)) '(+ (* x y) x)) -> '(+ (* 1 2) 1)
  "
  (cond
    ((null nv) 
      L)
    (T
      ; Feed the result of replacing the current token into the next iteration 
      (reps (rest nv) (rep (caar nv) (cdar nv) L)))
  )
)


(defun rep (name val L)
  "
  Replaces all instances of the symbol denoted by 'name' in L with its val

  e.g: (rep x 1 '(+ x 3)) -> '(+ 1 3)
  "
  (cond
    ; this element is not an atom so we must enter the list to look for arg tokens
    ((not (atom (first L)))
      (cons (rep name val (first L)) (rep name val (rest L))))
    ; We have a match to our arg token, replace it with the value
    ((equal name (first L)) 
      (cons val (rep name val (rest L))))
    ; We reached the last element of L and its not a replaceable token
    ((atom L) 
      L)
    (T 
      ; Leave current token unchanged and progress through the list
      (cons (first L) (rep name val (rest L))))
  )
)


(defun user-defined (f P)
  "
  Search a list of user defined functions P for a function definition matching the name f
  and return the matching function definition, or NIL if non are found

  e.g: (user-defined 'sum '((sum (x y) = (+ x y)) (prod (x y) = (* x y)))) -> (sum (x y) = (+ x y))
  "
  (cond 
    ; we have exhausted available defs, return NIL
    ((null P) NIL)
    ; The function name matches the one we are looking for
    ((equal f (caar P)) (first P))
    (T (user-defined f (rest P)))
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
(assert (equal (interp '(count (5 4 66)) '((count (L) = (if (null L) 0 (+ 1 (count (rest L))))))) 3))

; MY HELPERS
(assert (equal (user-defined 'greater '((greater (x y) = (if (> x y) x (if (< x y) y nil))))) '(greater (x y) = (if (> x y) x (if (< x y) y nil)))))
(assert (equal (user-defined 'greater '((nothing (x y) = (if (> x y) x (if (< x y) y nil))))) NIL))
(assert (equal (get-body '(greater (x y) = (if (> x y) x (if (< x y) y nil)))) '(if (> x y) x (if (< x y) y nil))))
(assert (equal (rep 'x 1 '(x y z x)) '(1 y z 1)))
(assert (equal (reps '((x . 1)) '(x x z y)) '(1 1 z y)))
(assert (equal (reps '((x . 1) (y . 2) (z . 3)) '(x x z y)) '(1 1 3 2)))
(assert (equal (reps '((x . 1) (y . 2) (z . 3)) '(x x z (x y))) '(1 1 3 (1 2))))
(assert (equal (make-pairs '(x y z) '(1 2 3)) '((x . 1) (y . 2) (z . 3))))
(terpri)