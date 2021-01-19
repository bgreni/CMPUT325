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
    (cond
        ((null L) '(()))
        (T (gen-subsets (car L) (allsubsets (cdr L))))
    )

)

(defun gen-subsets (x y)
    (cond
       ((null y) NIL)
       (T (cons (car y) 
            (cons (cons x (car y)) 
                (gen-subsets x (cdr y)))))
    )
)

#| QUESTION 6
|#

(defun reached (x L)
    (cond 
       ((null L) NIL)
       ((equal x (caar L)) (cons (cadar L) (reached x (cdr L))))
       (T (reached x (cdr L)))
    )
)


(defun depair (L)
    (cond
        ((null L) NIL)
        (T (cons (caar L) (depair (cdr L))))
    )
)

(defun rank (S L)
    (depair (mySort (compileFrequencies S L)))
)

(defun len (L)
    (if (null L) 0
        (+ 1 (len (cdr L)))
    )
)

(defun referTo (x L)
    (cond
        ((null L) NIL)
        ((equal x (cadar L)) (cons (caar L) (referTo x (cdr L))))
        (T (referTo x (cdr L)))
    )
)

(defun compileFrequencies (S L)
    (cond
       ((null S) NIL)    
       (T (cons (cons (car S) (len (referTo (car S) L))) (compileFrequencies (cdr S) L)))
    )
)

(defun mySort (L)
    (sort L 'compareFunc)
)

(defun compareFunc (X Y)
    (> (cdr X) (cdr Y))
)

;; (print (xmember NIL '(NIL) ))
;; (print (flatten '(1 2 (2))))
;; (print (remove-duplicate '(a b c a d b)))
;; (print (mix'(1 2 3) '(4 5 6)))
;; (print (allsubsets '(a b)))
;; (print (reached 1 '((2 3) (1 3) (2 3))))
;; (print (len '(1 2 3 7)))
;; (print (compileFrequencies '(a b c d) '((a b) (a c) (b c) (d a))))
;;  (print (referTo 3 '((1 2) (1 3) (2 3))))
;; (print (equal 3 (cadar '((2 3)))))
;; (print (mySort '((a 2) (b 9) (s 4))))
;; (print (rank '(a b c d) '((a b) (a c) (b c) (d a) (d b))))
(terpri)
    
    