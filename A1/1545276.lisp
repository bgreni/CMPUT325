"
Assignment 1
Brian Grenier
bgrenier
1545276
"


" QUESTION 1

This function takes an atom X and a list Y and returns
T if Y contains X, and NIL otherwise

Works by checking if X is equal to the first item in Y, if so returns true
then checks if Y is empty, if so returns  NIL
finally it calls xmember recursively, removing the first item from Y
in order to check the next item in the list.
Once the list is empty, it will become NIL, so if a match has not been found yet,
NIL will be returned
"
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

" QUESTION 2
This function unests any list inside of a list x

Test case:
(flatten '(a (b c))) => (a b c)

If checks if the list x is empty, and then return NIL,
then if the first item is an atom, that atom is cons'd with the flattened version of the rest of the list through recursion
otherwise, the flattened version of the first in the list is appended with the flattened version of the rest of the list
"
(defun flatten (x)
    (cond 
        ((null x) 
            NIL)
        ((atom (car x)) 
            (cons (car x) (flatten (cdr x))))
        (T 
            (append (flatten (car x)) (flatten (cdr x))))
    )
)

"QUESTION 3
Removes duplicate values from a list

checks if list x is empty and returns NIL
then checks if the first item in the list is a member of the rest of the list, then makes a recursive call exlcuding the rest of the list
otherwise combines the first item in the list, with the de-duplicated version of the rest of the list using recursion
"
(defun remove-duplicate (x)
    (cond 
        ((null x) 
            NIL)
        ((xmember (car x) (cdr x)) 
            (remove-duplicate (cdr x)))
        (T (cons (car x ) 
            (remove-duplicate (cdr x))))
    )
)

"QUESTION 4
Combines two lists by choosing elements from L1 and L2 alternatingly. If one of the lists are emtpy, then the rest of the
longer list is added

Checks if L1 is null, then returns the rest of L2
Checks if L2 is null, then returns the rest of L1
otherwise combines the first element of L1 with the rest of the mixed list using recursion, swapping the position of the arguments
L1 and L2, so that on the next recusive call, the first item in L2 will be choosen
"
(defun mix (L1 L2)
    (cond 
        ((null L1) 
            L2)
        ((null L2) 
            L1)
        (T (cons (car L1) 
            (mix L2 (cdr L1))))
    )
)

"QUESTION 5
Generates the powerset of a given list L
"
(defun allsubsets (L)
"
Checks if L is null, then returns empty list
Otherwise, returns powerset of the lists

"
    (cond
        ((null L) 
            '(()))
        (T (gen-subsets (car L) 
            (allsubsets (cdr L))))
    )
) 

(defun gen-subsets (x y)
"
x is single element and y is the currentl powerset

if y is null return nill
otherwise conse the first element in the powerset with the cons of x and the first element in y, and then
with the powerset of x and the remaining elements in y
"
    (cond
        ((null y) 
            NIL)
        (T (cons 
            (car y) (cons 
                (cons x (car y)) (gen-subsets x (cdr y)))))
    )
)

"QUESTION 6
Contains definitions for both the reached and ranked functions
"

; START OF REACHED FUNCTIONS
(defun reached (x L)
"
Given and atom x and a list of pairs (a b), returns all items such that there is a sequence of links such that c is reachable from a

(reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) )) -> (SHOPIFY AIRCANADA DELTA)
"
    (linked (list x) L)
)

(defun linked (seen L)
"
If L is null then returns cdr seen (the first element will be x, so we remove it)
Otherwise checks that the first item in the front pair of the list has been seen, confirming that there exists a path from our root page to the other page in the front pair
that the second item in the front pair in the list is not in seen to avoid circular paths
Otherwise, skips the current pairing at the front of the list 
"
    (cond 
        ((null L) 
            (cdr seen))
        ((and (xmember (caar L) seen) (not (xmember (cadar L) seen)))
            (linked (append seen (cdar L)) (cdr L)))
        (T (linked seen (cdr L)))
    )
)

; START OF RANK FUNCTIONS
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

(defun depair (L)
"
Given a list of pairs (a b), returns a list containing only the first value in each pair
Testcase: (depair '((1 2))) => (1)
"
    (cond
        ((null L) 
            NIL)
        (T (cons (caar L) 
            (depair (cdr L))))
    )
)

(defun len (L)
"
Returns the length of a list L
"
    (if (null L) 
        0
        (+ 1 (len (cdr L)))
    )
)

(defun referTo (x L seen)
"
Creates a list of elements from a list of pairs L (A B), where B == X, compiling a list of A where such is true
Testcase: (referTo 3 '((1 2) (1 3) (2 3))) => (1 2)
"
    (cond
        ((null L) 
            NIL)
        ((xmember (car L) seen) 
            (referTo x (cdr L) seen))
        ((equal x (cadar L)) 
            (cons (caar L) (referTo x (cdr L) (cons (car L) seen))))
        (T 
            (referTo x (cdr L) seen))
    )
)

(defun compileFrequencies (S L)
"
Goes through the elements in a list S, and compiles the number of times it appears as B in a list of pairs (A B)
Testcase: (compileFrequencies '(a b c d) '((a b) (a c) (b c) (d a))) => ((A . 1) (B . 1) (C . 2) (D . 0))
"
    (cond
       ((null S) 
            NIL)    
       (T 
        (cons 
            (cons 
                (car S) (len (referTo (car S) L NIL))) 
            (compileFrequencies (cdr S) L)))
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

;; (assert (equal (xmember '1 '(1)) T))
;; (assert (equal (xmember '1 '( (1) 2 3)) NIL))
;; (assert (equal (xmember '(1) '((1) 2 3)) T))
;; (assert (equal (xmember nil nil) NIL))
;; (assert (equal (xmember nil '(nil)) T))
;; (assert (equal (xmember nil '((nil))) NIL))
;; (assert (equal (xmember '(nil) '(1 2 3 (nil))) T))
;; (assert (equal (xmember '(nil) '(nil)) NIL))

;; (assert (equal (flatten '(a (b c) d)) '(a b c d)))
;; (assert (equal (flatten '((((a)))))  '(a)))
;; (assert (equal (flatten '(a (b c) (d ((e)) f))) '(a b c d e f)))

;; (assert (equal (remove-duplicate '(a b c a d b)) '(c a d b)))

;; (assert (equal (mix '(a b c) '(d e f)) '(a d b e c f)))
;; (assert (equal (mix '(1 2 3) '(a)) '(1 a 2 3)))
;; (assert (equal (mix '((a) (b c)) '(d e f g h))  '((a) d  (b c) e f g h)))
;; (assert (equal (mix '(1 2 3) nil) '(1 2 3)))
;; (assert (equal (mix '(1 2 3) '(nil)) '( 1 NIL 2 3)))

;; (assert (equal (allsubsets nil) '(nil)))
;; (assert (equal (allsubsets '(a)) '(nil (a))))
;; (assert (equal (allsubsets '(a b)) '(nil (a) (b) (a b))))

;; (assert (equal (reached 'google '( (google shopify) (google aircanada) (amazon aircanada))) '(shopify aircanada)))
;; (assert (equal (reached 'google '( (google shopify) (shopify amazon) (amazon google) ) ) '(shopify amazon)))
;; (assert (equal (reached 'google '( (google shopify) (shopify amazon) (amazon indigo))) '(shopify amazon indigo)))
;; (assert (equal (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) )) '(shopify aircanada delta)))

;; (assert (equal (rank '(google shopify aircanada amazon) '((google shopify) (google aircanada) (amazon aircanada))) '(AIRCANADA SHOPIFY GOOGLE AMAZON)))
;; (assert (equal (rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google))) '(GOOGLE SHOPIFY AMAZON)))
;; (assert (equal (rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo))) '(SHOPIFY AMAZON INDIGO GOOGLE)))
;; (assert (equal (rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) '(AIRCANADA GOOGLE SHOPIFY DELTA AMAZON)))
(terpri)
    
    