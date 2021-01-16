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

|#
(defun remove-duplicate (x)
    (cond 
        ((null x) NIL)
        ((xmember (car x) (cdr x)) (remove-duplicate (cdr x)))
        (T (cons (car x ) (remove-duplicate (cdr x))))
    )
)



;; (print (xmember NIL '(NIL) ))
;; (print (flatten '(1 2 (2))))
(print (remove-duplicate '(a b c a d b)))
(terpri)
    
    