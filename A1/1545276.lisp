
;QUESTION 1
;checks if Y contains X
(defun xmember (X Y)
    ;; (cond
    ;; ((null X) nil)
    ;; ((null Y) nil)
    (loop for x in Y
        (if (equal x X) then (return t))
    )
)

(prin1 (xmember `"3" `("3" "5")))
(terpri)
    
    