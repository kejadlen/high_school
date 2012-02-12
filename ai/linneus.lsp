; LINNEUS.LSP
(DEFUN LINNEUS ()		; This is the top-level procedure.
	(PRINT '(I AM LINNEUS))
	(format t "PLEASE GIVE ME INFORMATION OR ASK QUESTIONS~%")
 (LOOP	(SETQ TEXTIN (READ))	; Get a sentence from the user.
	(INTERPRET TEXTIN)	; Try to interpret it and act on it.
 )
)		; Repeat until user aborts program.
;
(DEFUN INTERPRET (TEXT)		; Here are the production rules...
  (COND
	             ;RULE I:  `(a bear is a mammal)
	((MATCH '((MATCHARTICLE ARTICLE1)(? X) IS
		  (MATCHARTICLE ARTICLE2)(? Y)) TEXT)
 	 (ADDSUPERSET X Y)
	 (ADDSUBSET Y X)
	 (PUTPROP X ARTICLE1 'ARTICLE)
	 (PUTPROP Y ARTICLE2 'ARTICLE)
	 (PRINT '(I UNDERSTAND)) )

	             ;RULE II: `(what is a bear)' ...
	((MATCH '(WHAT IS (MATCHARTICLE ARTICLE1)(? X)) TEXT)
	 (SETQ ISAFLAG NIL)	; Default is `no information
	 (SETQ INCLUDEFLAG NIL)	; available'.
	 (COND ((SETQ Y (GET X 'ISA))
		(SETQ ISAFLAG T) )	; Y is a superset of X.
	       ((SETQ Y (GET X 'INCLUDES))
		(SETQ INCLUDEFLAG T) ) ); ` subset '.
	 ; Print out a reply based on one of the two relations...
	 (PRINT (APPEND
		 (LIST (GET X 'ARTICLE)); `A' or `AN',
		 (LIST X)		; whatever X is,
		 (COND (ISAFLAG '(IS))	; one of the two relations,
		       (INCLUDEFLAG
			       '(IS SOMETHING MORE GENERAL THAN) ) )
		 (MAKECONJ Y) )) )	; some things that X is or
					; is more general than.

	                    ;RULE III: `(is a bear a mammal)'
	((MATCH '(IS (MATCHARTICLE ARTICLE1) (? X)
		 (MATCHARTICLE ARTICLE2) (? Y)) TEXT)
	 (COND ((ISATEST X Y 10)	; Search for Y from X.
		(PRINT			; Reply affirmatively.
		  (APPEND '(YES INDEED)
			  (LIST (GET X 'ARTICLE))
			  (LIST X)
			  '(IS)
			  (LIST (GET Y 'ARTICLE))
			  (LIST Y) ) ) )
	       (T (PRINT '(SORRY NOT THAT I KNOW OF))) ) ) ; Negative.

	                    ;RULE IV: `(why is a bear an animal)'
	((MATCH '(WHY IS (MATCHARTICLE ARTICLE1) (? X)
			 (MATCHARTICLE ARTICLE2) (? Y)) TEXT)
	 (COND	((ISATEST X Y 10)	; Is presupposition correct?
		 (PRINT			; Yes, prepare reply with explanation:
		   (CONS 'BECAUSE
			 (EXPLAIN_LINKS X Y) ) ) )	; Create explanation.
		(T (PRINT '(BUT IT ISN'T!))) ) )	; No, give reply
			; indicating that the presupposition is false.

	;rule Ii '(dog has paw)
	((match '((? x) has (? y)) text)
	 (addhas x y)
	 (print '(I SEE)))

	;rule IIi '(has dog paw)
	((match '(has (? x) (? y)) text)
	 (cond ((hasatest x y) (print '(yes)))
	       (T (print '(no)))))

	((match '(why has (? x) (? y)) text)
	 (cond ((hasatest x y)
		(print (append '(because) (explain x y))))
	       (T (print '(but is isn't!)))))

	                        ;RULE V: handles all other inputs
	(T (PRINT '(I DO NOT UNDERSTAND))) ) )

(DEFUN ADDTOSET (ELT LST)
  (COND ((MEMBER ELT LST) LST) (T (CONS ELT LST)) ))

(DEFUN ADDSUPERSET (SYMBOL VALUE)
    (PUTPROP SYMBOL(ADDTOSET VALUE (GET SYMBOL 'ISA)) 'ISA))

(DEFUN ADDSUBSET (Y X)
  (PUTPROP Y (ADDTOSET X (GET Y 'INCLUDES)) 'INCLUDES) )

(defun addhas (sym val)
  (putprop sym (addtoset val (get sym 'hasa)) 'hasa))

(DEFUN ISATEST (X Y N)
  (COND ((EQ X Y) T)
	((MEMBER Y (GET X 'ISA)) T)
	((ZEROP N) NIL)
	(T (ANY (MAPCAR (FUNCTION (LAMBDA (XX) (ISATEST XX Y
						(- N 1))))
			(GET X 'ISA))))))

(defun hasatest (x y)
  (any (mapcar (function (lambda (xx) (isatest xx y 10))) (get x 'hasa))))

(DEFUN ANY (LST)
  (COND	((NULL LST) NIL)
	((CAR LST) T)
	(T (ANY (CDR LST)))))

(DEFUN EXPLAIN_LINKS (X Y)
  (COND ((EQ X Y) '(THEY ARE IDENTICAL))        ; 1st special case
        ((MEMBER Y (GET X 'ISA)) '(YOU TOLD ME)); 2nd special case
        (T (EXPLAIN_CHAIN X (GET X 'ISA) Y)) ) ); General case

(defun explain (x y)
  (setq xxx (mapcar (function (lambda (xx) (explain_chain xx (get xx 'isa)
y))) (get x 'hasa)))
  (print (cons '(Hello) xxx))
  (cond ((member y (get x 'hasa)) '( you told me so))
        ((eq (first xxx) 'a) (append (list x) '( has a ) (first xxx) xxx))
	(T (append (list x) '( has a ) (first (rest xxx)) xxx))))

; It uses the recursive function EXPLAIN_CHAIN:

; Explain the first chain from X to Y that passes through
;  a member of L:
(DEFUN EXPLAIN_CHAIN (X L Y)
  (COND ((NULL L) NIL)           ; L should never be null.
        ((MEMBER Y L)            ; Is this the last link?
         (CONS 'AND (TELL X Y)) ); Yes, precede by AND.
        ((ISATEST (CAR L) Y 10)  ; Does chain go through CAR L?
         (APPEND (TELL X (CAR L)) ; Yes, explain this link, etc.
                 (EXPLAIN_CHAIN (CAR L)
                                (GET (CAR L) 'ISA)
                                Y) ) )
        (T (EXPLAIN_CHAIN X (CDR L) Y)) ) ) ; else try next in L.

; TELL explains the (single) link from X to Y.
(DEFUN TELL (X Y)
  (LIST (GET X 'ARTICLE) X 'IS (GET Y 'ARTICLE) Y) )

(DEFUN MATCHARTICLE (X)
;(print 'in_match_article)
  (MEMBER X '(A AN THE THAT THIS THOSE THESE)) )

(DEFUN MAKECONJ (LST)
  (COND	((NULL LST) NIL)
	((NULL (CDR LST)) (CONS (GET (CAR LST) 'ARTICLE) LST))
	(T (CONS (GET (CAR LST) 'ARTICLE)
		 (CONS  (CAR LST)
			(CONS 'AND (MAKECONJ (CDR LST))) ) )) ) )

(DEFUN MATCH (P S)
  (COND	((NULL P)(NULL S))
	((AND (NULL S)
	      (OR (ATOM (CAR P))
		  (AND	(ATOM (CAAR P))
			(NULL (EQ (CAAR P) '*)) ) ) ) NIL)
	((NULL S)
	 (AND (NULL (ATOM (CAR P)))                               
	      (EQ (CAAR P) '*)
	      (NULL (CDR P))
	      (SET (CADAR P) NIL) T) ) 
	((ATOM (CAR P))
	 (COND	((EQ (CAR P)(CAR S)) (MATCH (CDR P)(CDR S)))
		(T NIL)))
	((EQ (CAAR P) '?)
	 (AND (MATCH (CDR P)(CDR S))
	       (SET (CADAR P) (CAR S))T))             
	((EQ (CAAR P) '*)
	 (COND	((MATCH (CDR P) S)
		     (SET (CADAR P) NIL) T )   
		((MATCH (CDR P)(CDR S))
		     (SET (CADAR P) (LIST (CAR S))) T)                
		((MATCH P (CDR S))  
		     (SET (CADAR P) (CONS (CAR S)(EVAL (CADAR P)))) T)
		(T NIL) ) )

	((AND (APPLY (CAAR P) (LIST (CAR S)))
	      (MATCH (CDR P) (CDR S)) )        ;end AND
	           (SET (CADAR P)(CAR S)) T)   ;end production rule for apply 

        (T NIL)
   )
)


(defun putprop (s v p)
   (setf (get s p) v))
