0123456789012345678901234567890123456789012345678901234567890123456789
;Match

; MATCH.LSP -- a recursive pattern-matching function
;	for use in production-systems programming.
(DEFUN MATCH
 (P S)
  (COND
  ((NULL P)(NULL S))                  ;case with both P and S null
                                      ;from here on we can
                                      ; assume P is not null.


  ((ATOM (CAR P))                     ;case when CAR P is an atom
   (AND S                             ;S must not be null.
		(EQUAL (CAR P) (CAR S))
		(MATCH (CDR P) (CDR S)) ) )
                                      ;from here on CAR of P is non atomic.

  ((AND                               ;case when P starts with ? form.
    S                                 ;S must not be null.
		(EQ (CAAR P) '?) )
   (COND  ((MATCH (CDR P)(CDR S))     ;rest much match, too.
		 (SET (CADAR P) (CAR S)) 
		 T)
		(T NIL) ) )
  
  ((EQ (CAAR P) '*)                   ;case when P starts with * form.
	 (COND
    ((AND S (MATCH (CDR P)(CDR S)))   ;subcase 1
		 (SET (CADAR P) (LIST (CAR S))) T)

    ((MATCH (CDR P) S)                ;subcase 2
		 (SET (CADAR P) NIL) T)

    ((AND S (MATCH P (CDR S)))        ;subcase 3
		 (SET (CADAR P) (CONS (CAR S)(EVAL (CADAR P)))) T)

		(T NIL) ) )


  ((eq (caar p) 'same)
    (set (cadar p) (list (first s)))
    (setf s (rest s))
    (do ()
	((not (member (first s) (eval (cadar p)))) (match (rest p) s))
	(set (cadar p) (cons (first s) (eval (cadar p))))
	(setf s (rest s))
    )
;    (set (cadar p) (samer (eval (cadar p)) s))
;    (setf s (destroy (eval (cadar p)) s))
;    (print 'variable-is)
;    (print (cadar p))
;;    (print 'rest-p)
;;    (print (rest p))
;;    (print 's)
;;    (print s)
;    (print 'test-is)
;    (match (rest p) s)
  )

  ((AND                               ;case when P starts with predicate form.
    S                                 ;S must not be null.
		(APPLY (CAAR P) (LIST (CAR S)))
		(MATCH (CDR P) (CDR S)) )
	 (SET (CADAR P)(CAR S)) T)

	(T NIL)
 ) )

(defun samer (ls s)
  (cond ((member (first s) ls) (samer (cons (first s) ls) (rest s)))
	(T ls))
)

(defun destroy (ls s)
  (do ((count (length ls))
      )
      ((eq count 1) s)
      (setf s (rest s))
      (setf count (- count 1))
  )
)





