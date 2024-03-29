(setf strlen 5)
(setf numstr 4)

(defun genetic ()
(setf all_strings NIL)
(setf int_strings NIL)
(setf func_strings NIL)
(setf sum_nums NIL)
(setf pselect NIL)
(setf sumlst NIL)
(setf rou_nums NIL)

(print '(start))

(do ((all_strings (create_strings numstr))
;(do ((all_strings '((1 1 1 1 1) (0 1 0 1 0) (0 0 0 0 1) (1 0 1 0 1))) 
    (loop 0 (+ 1 loop))
    )
    ((finished all_strings) (convert all_strings))
;    ((= loop 2) (convert all_strings))
(print all_strings)
    (setf int_strings (convert all_strings))
(print int_strings)
    (setf func_strings (func int_strings))
(print func_strings)
    (setf sum_nums (sum func_strings))
(print sum_nums)
;    (setf pselect (percentage func_strings))
    (setf sum_lst (for_roulette func_strings))
(print sum_lst)
    (setf rou_nums (roulette sum_lst all_strings))
(print rou_nums)
    (setf all_strings (crossover rou_nums))
;(cond ((= loop 2) (setf loop 0) (mutate all_strings) (print '(mutate)) (print all_strings)))
;    (setf all_strings rou_nums)
;    (setf debug (printer all_strings))
)
;(setf debug (convert rou_nums))
;(print debug)
;(setf debug (func debug))
;(print debug)

)

(defun printer (lst)
  (print '(all_strings)) (print rou_nums)
  (print all_strings)
  (print (func (convert rou_nums)))
  1
)

(defun create_strings (numstr)
  (do ( (loop 0 (+ 1 loop))
	(string () (cons (make_string) string))
      )
      ((= loop numstr) string)
  )
)

(defun make_string ()
  (do ( (loop 0 (+ 1 loop))
        (string () (cons (random 2) string))
      )
      ((= loop strlen) string)
  )
)

(defun convert (lst)
  (do ( (loop 0 (+ 1 loop))
        (string () (cons (convert-help (first tmplst)) string))
        (tmplst (reverse lst) (rest tmplst))
      )
      ((null tmplst) string)
  )
)

(defun convert-help (lst)
  (setf value 0)
  (do ((tmp strlen (- tmp 1))
       (tmplst lst (rest tmplst))
      )
      ((= tmp 0) value)
      (setf value (+ value (* (expt 2 (- tmp 1)) (first tmplst))))
  )
)

(defun func (lst)
  (do ((tmplst (rest lst) (rest tmplst))
       (val (first lst) (first tmplst))
       (string NIL (cons (expt val 2) string))
;       (string () (cons (+ (- (* 2 (expt (/ val 10) 2)) (expt (/ val 10) 3)) 5) string))
      )
      ((null val) (reverse string))
  )
)

(defun sum (lst)
  (do ((tmplst lst (rest tmplst))
       (val 0 (+ (first tmplst) val))
      )
      ((null tmplst) val)
  )
)

(defun percentage (lst)
  (do ((tmplst lst (rest tmplst))
       (string NIL (cons (/ (first tmplst) sum_nums) string))
      )
      ((null tmplst) (reverse string))
  )
)

(defun for_roulette (lst)
  (do ((tmplst (rest lst) (rest tmplst))
       (string (list (first lst)) (cons (+ (first string) (first tmplst)) string))
      )
      ((null tmplst) (reverse string))
  )
)

(defun roulette (lst all_strings) 
  (do ((tmplst lst (rest tmplst))
       (val 0 (decide_val (random sum_nums) sum_lst))
       (string NIL (putinlist string val all_strings))
      )
      ((null tmplst) (reverse string))
  )
)

(defun decide_val (num lst)
  (do ((val 0 (+ 1 val))
       (tmplst lst (rest tmplst))
      )
      ((> (first tmplst) num) val)
  )
)

(defun putinlist (string val all_strings)
  (do ((tmplst all_strings (rest tmplst))
       (num val (- num 1))
      )
      ((= num 0) (cons (first tmplst) string))
  )
)

(defun crossover (lst)
  (do ((tmplst lst)
       (final NIL (mate tmplst final))
      )
    ((= (length tmplst) (length final)) final) 
  )
)

(defun mate (tmplst final)
  (do ((loop 0 (+ 1 loop))
       (val (random (length tmplst)) (random (length tmplst)))
      )
      ((not (= loop val)) (append (mate2 (max loop val) (min loop val) tmplst) final))
  )
)

(defun mate2 (male fem tmplst)
  (setf femlst (first (nthcdr fem tmplst)))
  (setf malelst (first (nthcdr male tmplst)))
  (setf c-point (+ 1 (random (- (length femlst) 1))))
(print c-point)
  (setf string (append (nthbutlast (- (length femlst) c-point) femlst) (nthcdr c-point malelst)))
  (setf string (list string))
  (setf string (append (list (append (nthbutlast (- (length femlst) 
c-point) malelst) (nthcdr c-point femlst))) string))
  string
)

(defun nthbutlast (num lst)
  (do ((loop 0 (+ 1 loop))
       (tmplst lst (butlast tmplst))
      )
      ((= loop num) tmplst)
  )
)

(defun finished (lst)
  (do ((tmplst lst (rest tmplst))
       (comp (first lst))
       (flag 1 (and flag (equal comp (first tmplst))))
      )
      ((null tmplst) flag)
  )
)

(defun mutate (lst)
  (setf num (random numstr))
  (setf randum (random strlen))
  (setf tmplst (first (nthcdr num lst)))
  (setf lst (append (nthbutlast (- numstr num) lst) (nthcdr num lst)))
  (setf tmplst (append (nthbutlast (- strlen randum) tmplst) (list (- 1 (first (nthcdr randum tmplst)))) (nthcdr (+ randum 1) tmplst)))
(cons tmplst lst )
)