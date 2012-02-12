; ELIZA - WORKSHEET #7
(DEFUN ELIZA()
  (setq loope 0)
  (setq loopy 0)
  (setq remember ())
  (setq family ())  

	(SETQ WWORDCOUNT 0)
	(SETQ PUNTCOUNT 0)
	(FORMAT T "WELCOME TO MY SOFA~%")
	(FORMAT T "PLEASE ENCLOSE YOUR INPUT IN PARENTHESES ~%")
  (LOOP	(SETQ S (YOU-ME-MAP (READ)))
	(setq loope (+ 1 loope))
	(if (eq loope 2) (setq loope 0))
	(setq loopy (+ 1 loopy))
	(if (eq loopy 7) (setq loopy 0))
;	(cond ((= loope 5) (setq remember s) (setq loope 0)
;	       (printl '(Eh? Say again?)))
;	      ((and (= loopy 4) (match '((* y)(familycheck w)) remember))
;	       (printl remember))
;	      ((= loopy 4) (printl (append '(Earlier you said that)
;					   remember '(- can you explain?))))
;	      ((= loopy 8) (setq loopy 0) (printl (append 
;				   '(You mentioned before that) remember)))
       	(COND	((MATCH '(BYE) S)
		 (RETURN 'GOODBYE))

		((match '(you remember (* x)) s)
		 (setq remember (cons (append '(so you remembered ) x '( - go on)) remember)) (printl '(i see)))

		((match '(you wish (* x)) s) (setq remember
					     (cons (append '(why do you wish ) x) remember)) (printl '(hmmmmm)))

		((MATCH '(YOU ARE (* X)) S)
		 (PRINTL (APPEND '(PLEASE TELL ME)
				  (LIST (WWORD))
				 '(YOU ARE)
				  X)))
		((MATCH '(YOU HAVE (* X)) S)
		 (PRINTL (APPEND '(HOW LONG HAVE YOU HAD) X)) )
		((MATCH '(YOU FEEL (* X)) S)
		 (PRINTL '(I SOMETIMES FEEL THE SAME WAY)) )
		((MATCH '(BECAUSE (* X)) S)
		 (PRINTL '(IS THAT REALLY THE REASON) ))
		((MATCH NIL S)(FORMAT T "SAY SOMETHING~%"))
		((MATCH '(YES (* X)) S)
		 (PRINTL (APPEND '(HOW CAN YOU BE SO SURE) X)) )
		((MATCH '(ME ARE (* X)) S)
		 (PRINTL (APPEND '(OH YEAH I AM) X)) )
		((MATCH '((VERB V) (* X)) S)
		 (PRINTL (APPEND '(OY YOI YOI HE WANTS THAT
				   I SHOULD GO AND) (LIST V) X) ) )
		((MATCH '((WPRED W)(* X)) S)
		 (PRINTL (APPEND '(YOU TELL ME)(LIST W)) ) )
		((MATCH '((DPRED W) ME (* X)) S)
		 (PRINTL (APPEND '(PERHAPS I)(LIST W) X) ) )

		((match '((* y)(violent w)(* x)) s)
		 (cond ((eq loope 1) (printl (append '(You shouldn't )
					  (list (violent1 w)) '(things))))
		       (T (printl (append '(It is not a good idea to )
				        (list (violent1 w)) '(people))))))

		((match '((* y)(familycheck w)(* x)) s)
		 (setq remember (cons (append '(let's go back to your
				        remark about your ) (list w)) remember))
		 (printl '(really?)))

		((MATCH '(DO ME THINK (* X)) S) (PRINTL '(I THINK YOU
			SHOULD ANSWER THAT YOURSELF)) )
		((MEMBER 'DREAM S)
		  (if (= loope 1) (FORMAT T " FOR DREAM ANALYSIS SEE FREUD~%")
		    (format t " Dreams can tell you much~%")))
		((MEMBER 'LOVE S)
		  (if (= loope 1) (FORMAT T "ALL IS FAIR IN LOVE AND WAR~%")
		    (format t "Love is a harsh thing~%")))
		((MEMBER 'NO S)(FORMAT T "DONT BE SO NEGATIVE~%"))
		((MEMBER 'MAYBE S)(FORMAT T "BE MORE DECISIVE~%"))
		((MEMBER 'YOU S)(PRINTL S))
	        (T (cond ((eq loopy 4) (printl (first remember)) 
					      (setq remember (rest remember)) (setq loopy 0))
		   (T (SETQ PUNTCOUNT (+ 1 PUNTCOUNT))
		      (IF (= PUNTCOUNT 12) (SETQ PUNTCOUNT 0))
		      (PRINTL (GETNTH PUNTCOUNT PUNTS))))))))

(DEFUN PRINTL (MESSAGE)
  (MAPCAR #'(LAMBDA (TXT) (FORMAT T "~A " TXT) )MESSAGE)
  (TERPRI)
)

(DEFUN WWORD ()
  (SETQ WWORDCOUNT (+ 1 WWORDCOUNT))
	(IF (= WWORDCOUNT 5)(SETQ WWORDCOUNT 0))
             (GETNTH WWORDCOUNT
		 '(WHEN WHY WHERE WHENEVER THAT)))

(defun violent1 (w)
  (cond ((eq w 'killed) 'kill)
	((eq w 'shot) 'shoot)
	((eq w 'maimed) 'maim)
	((eq w 'tortured) 'torture)
        (T w)))

(defun violent (w)
  (member w '(kill killed shoot shot maim maimed)))

(defun familycheck (w)
  (member w '(mother mom father dad sister brother uncle aunt niece nephew)))

(DEFUN WPRED (W)
 (MEMBER W '(WHY WHERE WHEN WHAT)) )

(DEFUN DPRED (W)
 (MEMBER W '(DO CAN SHOULD WOULD)) )

(DEFUN GETNTH (N LST)
 (COND  ((NULL LST) NIL)
	((ZEROP N)(CAR LST))
	(T (GETNTH (- N 1)(CDR LST))) ))

(SETQ PUNTS   '((PLEASE GO ON)
		(TELL ME MORE)
		(I SEE)
		(WHAT DOES THAT INDICATE)
		(BUT WHY BE CONCERNED ABOUT IT)
		(JUST TELL ME HOW YOU FEEL)
		(Really)
		(Do you really think so)
		(The whole story please)
		(I do not understand - please repeat it)
		(I understand - do go on)
		(Please elaborate)
		))

(DEFUN YOU-ME (W)
 (COND	((EQ W 'I) 'YOU)
	((EQ W 'ME) 'YOU)
	((EQ W 'YOU) 'ME)
	((EQ W 'MY) 'YOUR)
	((EQ W 'YOUR) 'MY)
	((EQ W 'YOURS) 'MINE)
	((EQ W 'MINE) 'YOURS)
	((EQ W 'AM) 'ARE)
	(T W) ) )

(DEFUN YOU-ME-MAP (LST) (MAPCAR 'YOU-ME LST))

(DEFUN VERB (W)
 (MEMBER W '(GO HAVE BE TRY EAT TAKE HELP MAKE GET JUMP
		 WRITE TYPE FILL PUT TURN COMPUTE
		 THINK DRINK BLINK CRASH CRUNCH ADD break die) ) )

; MATCH.LSP -- a recursive pattern-matching function
;	for use in production-systems programming.
(DEFUN MATCH (P S)
  (COND
	((NULL P)(NULL S))	;case I: both P and S null


	((ATOM (CAR P))		;case II: CAR P is an atom
	 (AND	S			;S must not be null.
		(EQUAL (CAR P) (CAR S))
		(MATCH (CDR P) (CDR S)) ) )

	((AND			;case III: P starts with ? form.
		S			
		(EQ (CAAR P) '?) )
	 (COND	((MATCH (CDR P)(CDR S))	(SET (CADAR P) (CAR S)) T)
		(T NIL) ) )

	((EQ (CAAR P) '*)	;case IV: P starts with * form.
	 (COND
		((AND S (MATCH (CDR P)(CDR S)))		;subcase 1
		 (SET (CADAR P) (LIST (CAR S))) T)

		((MATCH (CDR P) S)			;subcase 2
		 (SET (CADAR P) NIL) T)

		((AND S (MATCH P (CDR S)))		;subcase 3
		 (SET (CADAR P) (CONS (CAR S)(EVAL (CADAR P)))) T)

		(T NIL) ) )

	((AND			;case V: P starts with predicate form.
		S			
		(APPLY (CAAR P) (LIST (CAR S)))
		(MATCH (CDR P) (CDR S)) )
	 		(SET (CADAR P)(CAR S)) T)

	(T NIL)
 ) )