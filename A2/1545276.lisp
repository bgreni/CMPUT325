(defun fl-interp (E P)
  (cond 
	((atom E) E)   %this includes the case where E is nil or a number
        (t
           (let ( (f (car E))  (arg (cdr E)) )
	      (cond 
                ; handle built-in functions
                ((eq f 'first)  (car (fl-interp (car arg) P)))
	        ..... 

	        ; if f is a user-defined function,
                ;    then evaluate the arguments 
                ;         and apply f to the evaluated arguments 
                ;             (applicative order reduction) 
                .....

                ; otherwise f is undefined (not intended to be a function),
                ; the E is returned, as if it is quoted in lisp 