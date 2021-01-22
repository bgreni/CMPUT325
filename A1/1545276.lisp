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
This function unests any list inside of a list x

Test case:
(flatten '(a (b c))) => (a b c)

If checks if the list x is empty, and then return NIL,
then if the first item is an atom, that atom is cons'd with the flattened version of the rest of the list through recursion
otherwise, the flattened version of the first in the list is appended with the flattened version of the rest of the list
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

checks if list x is empty and returns NIL
then checks if the first item in the list is a member of the rest of the list, then makes a recursive call exlcuding the rest of the list
otherwise combines the first item in the list, with the de-duplicated version of the rest of the list using recursion
|#
(defun remove-duplicate (x)
    (cond 
        ((null x) NIL)
        ((xmember (car x) (cdr x)) (remove-duplicate (cdr x)))
        (T (cons (car x ) (remove-duplicate (cdr x))))
    )
)

#| QUESTION 4
Combines two lists by choosing elements from L1 and L2 alternatingly. If one of the lists are emtpy, then the rest of the
longer list is added

Checks if L1 is null, then returns the rest of L2
Checks if L2 is null, then returns the rest of L1
otherwise combines the first element of L1 with the rest of the mixed list using recursion, swapping the position of the arguments
L1 and L2, so that on the next recusive call, the first item in L2 will be choosen
|#
(defun mix (L1 L2)
    (cond 
        ((null L1) L2)
        ((null L2) L1)
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
Contains definitions for both the reached and ranked functions
|#

(defun reached (x L)
"
Given and atom x and a list of pairs (a b), returns all items b that are paired with a, such that a == x
In the context of the question, returns all sites b, that can be reached from site x
Testcase: (reached 1 '((2 3) (1 3) (2 1))) => (3)

If L is empty returns NIL
Then checks if x is equal to the first element in the first pair of L, then adds the second item in the pair to the list
otherwise cuts off that pair in a recursive call
"
    (cond 
       ((null L) NIL)
       ((equal x (caar L)) (cons (cadar L) (reached x (cdr L))))
       (T (reached x (cdr L)))
    )
)


(defun depair (L)
"
Given a list of pairs (a b), returns a list containing only the first value in each pair
Testcase: (depair '((1 2))) => (1)
"
    (cond
        ((null L) NIL)
        (T (cons (caar L) (depair (cdr L))))
    )
)

(defun rank (S L)
"
Prints the list of elemets in S sorted by how many times they appear as B in a list of pairs (A B) L
In the context of the question, prints S ordered in in terms of importance

Testcase: (rank '(a b c d) '((a b) (a c) (b c) (d a) (d b))) => (B C A D)

Works by compiling a list of pairs (A B) where A is a member of S and B is its importance value
The list is then sorted in increasing order by importance value of each pair
The importance value is then removed from the pairs and the list of members of S is returned
"
    (depair (mySort (compileFrequencies S L)))
)

(defun len (L)
"
Returns the length of a list L
"
    (if (null L) 0
        (+ 1 (len (cdr L)))
    )
)

(defun referTo (x L seen)
"
Creates a list of elements from a list of pairs L (A B), where B == X, compiling a list of A where such is true
Testcase: (referTo 3 '((1 2) (1 3) (2 3))) => (1 2)
"
    (cond
        ((null L) NIL)
        ((xmember (car L) seen) (referTo x (cdr L) seen))
        ((equal x (cadar L)) (cons (caar L) (referTo x (cdr L) (cons (car L) seen))))
        (T (referTo x (cdr L) seen))
    )
)

(defun compileFrequencies (S L)
"
Goes through the elements in a list S, and compiles the number of times it appears as B in a list of pairs (A B)
Testcase: (compileFrequencies '(a b c d) '((a b) (a c) (b c) (d a))) => ((A . 1) (B . 1) (C . 2) (D . 0))
"
    (cond
       ((null S) NIL)    
       (T (cons (cons (car S) (len (referTo (car S) L NIL))) (compileFrequencies (cdr S) L)))
    )
)

(defun mySort (L)
"
sorts a list of pair (A B) where B is an integer in increasing order by B
"
    (sort L 'compareFunc)
)

(defun compareFunc (X Y)
"
Given two pairs X and Y of the form (A B), returns true of X.B > Y.B
"
    (> (cdr X) (cdr Y))
)

;; (print (xmember NIL '(NIL) ))
;; (print (flatten '(1 2 (2))))
;; (print (remove-duplicate '(a b c a d b)))
;; (print (mix'(1 3 5) '(2 4 6)))
;; (print (allsubsets '(a b)))
;; (print (reached 1 '((2 3) (1 3) (2 1))))
;; (print (len '(1 2 3 7)))
;; (print (compileFrequencies '(a b c d) '((a b) (a c) (b c) (d a) (a b) (c b))))
;;  (print (referTo 3 '((1 2) (1 3) (2 3))))
;; (print (equal 3 (cadar '((2 3)))))
;; (print (mySort '((a 2) (b 9) (s 4))))
;; (print (rank '(a b c d) '((a b) (a c) (b c) (d a) (d b))))
(terpri)
    
    