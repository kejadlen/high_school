
;Worksheet #4

;This search procedure is called the "Best-First-Search"

(setq open_count 0)
(setq val 0)
(defun bfs (start goal_node)
(setq goal goal_node)
      (setq closed nil)
      (putprop start nil 'ptr)
      (putprop start (f start) 'fvalue)
      (setq open (list start))
      (loop (cond ((null open)(return 'failure)))
          (setq n (select_best open))
          (setq open (delete n open))
          (setq closed (cons n closed))
          (if (eq n goal) (return(extract_path n)))
          (setq l (get n 'adj)) 
          (mapcar 'open_node (set_diff (set_diff l open) closed))
    )
)



(defun select_best (lst)
   (cond ((eq (first lst) goal)(first lst))
         (T (better (first lst)(rest lst)))
   )
)

(defun better (elem lst)
   (cond ((null lst) elem)
         ((< (get elem 'fvalue)(get (first lst) 'fvalue)) elem)
         ((eq (first lst) goal)(first lst))
         (T (better elem (rest lst)))
   )
)

(defun open_node (M)
   (prog ()
     (setq open_count ( + 1 open_count))
     (putprop m (setq val (f m)) 'fvalue)
     (setq open (insert m open))
     (putprop m n 'ptr)
   )
)

(defun insert (node lst)
    (cond ((null lst)(list node))
          ((< val (get (first lst) 'fvalue))(cons node lst))
          (T (cons (first lst)(insert node (rest lst))))
    )
)

(defun successors (node) (get node 'adj))

(defun putprop (s  v p)
  (setf (get s p) v)
)

(defun set_diff (ls1 ls2)
   (cond ((null ls1) nil)
         ((member (first ls1) ls2)(set_diff (rest ls1) ls2))
         (T (cons (first ls1)(set_diff (cdr ls1) ls2)))
   )
)

;the next two functions could easily be combined but the author wanted
;to make the fvalue property self explanatory
(defun longitude_diff(n1 n2)
    (abs (- (get n1 'lg)(get n2 'lg)))
)

(defun f(n)
    (longitude_diff n goal)
)

(defun extract_path (n)
  (cond ((null n) nil)
        (t (append (extract_path (get n 'ptr))
                                   (list n)))
  )
)

;lg stands for longitude.
;each city is paired with its longitude.
;notice how cleverly the mapcar effectively accomplishes 18 putprops
(mapcar #'(lambda(x) (putprop (first x)(first (rest x)) 'lg))
          '((av 48)(bord -6)(bre -45)(caen -4)(cal 18)
            (di 51)(gren 57)(lim 12)(ly 48)(mars 53)
            (mont 36)(nan -16)(ncy 62)(nice 73)(paris 23)
            (ren -17)(stras 77)(to 14))
)


;these putprops construct the graph. It is similar to Worksheet #3 except that
;the graph is much larger

(putprop 'bre '(ren) 'adj)
(putprop 'ren '(caen paris bre nan) 'adj)
(putprop 'caen '(cal paris ren) 'adj)
(putprop 'cal '(ncy paris caen) 'adj)
(putprop 'ncy '(stras di paris cal) 'adj)
(putprop 'stras '(di ncy) 'adj)
(putprop 'di '(stras ly paris ncy) 'adj)
(putprop 'ly '(gren av lim di) 'adj)
(putprop 'gren '(av ly) 'adj)
(putprop 'av '(gren mars mont ly) 'adj)
(putprop 'mars '(nice av) 'adj)
(putprop 'nice '(mars) 'adj)
(putprop 'mont '(av to) 'adj)
(putprop 'to '(mont bord lim) 'adj)
(putprop 'bord '(lim to nan) 'adj)
(putprop 'lim '(ly to bord nan paris) 'adj)
(putprop 'nan '(lim bord ren) 'adj)
(putprop 'paris '(cal ncy di lim ren caen) 'adj)


