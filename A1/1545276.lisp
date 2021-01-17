#| QUESTION 1

This function takes an atom X and a list Y and returns
T if Y contains X, and NIL otherwise

Works by checking if X is equal to the first item in Y, if so returns true
then checks if Y is empty, if so returns  NIL
finally it calls xmember recursively, removing the first item from Y
in order to check the next item in the list.
Once the list is empty, it will become NIL, so if a match has not been found yet,
NIL will be returned
|#
(defun xmember (X Y)
    (cond
      ((equal X (car Y)) T)
      ((null Y) NIL)
      (T (xmember X (cdr Y)))
    )
)

#| QUESTION 2
unests lists inside of a list

I don't fucking know
|#
(defun flatten (x)
    (cond 
        ((null x) NIL)
        ((atom (car x)) (cons (car x) (flatten (cdr x))))
        (T (append (flatten (car x)) (flatten (cdr x))))
    )
)

#| QUESTION 3
 Removes duplicate values from a list
|#
(defun remove-duplicate (x)
    (cond 
        ((null x) NIL)
        ((xmember (car x) (cdr x)) (remove-duplicate (cdr x)))
        (T (cons (car x ) (remove-duplicate (cdr x))))
    )
)

#| QUESTION 4

|#
(defun mix (L1 L2)
    (cond 
        ((null L1) L2)
        (T (cons (car L1) (mix L2 (cdr L1))))
    )
)

#| QUESTION 5
|#
(defun allsubsets (L)
     (gen-subsets (cons nil nil) L)
)

(defun gen-subsets (L1 L2)
    (cond
        ((null L1) L2)
        (T (cons (car L1) (gen-subsets (cdr L1) L2)))
    )
)

;; (print (xmember NIL '(NIL) ))
;; (print (flatten '(1 2 (2))))
;; (print (remove-duplicate '(a b c a d b)))
;; (print (mix'(1 2 3) nil))
(print (allsubsets '(a b)))
(terpri)
    
    