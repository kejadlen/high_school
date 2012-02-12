;;Alpha Chen
;;9/29/99
;;Assignment 3: mapcar

(defun putprop (sym val prop)
  (setf (get sym prop) val)
)

(putprop 's '(a b e) 'adj)
(putprop 'a '(b c e) 'adj)
(putprop 'b '(a c d) 'adj)
(putprop 'c '(a d) 'adj)
(putprop 'd '(b e) 'adj)
(putprop 'e '(s c d) 'adj)

;;Original code
(defun search1 (start finish)
  (search-aux (list (list start)) finish)
)

(defun search-aux (pathlist goal)
  (cond ((endp pathlist) nil)
        ((equal goal (first (first pathlist))) (reverse (first pathlist)))
        (T (search-aux (append (expand (first pathlist)) (rest pathlist)) goal))
  )
)

(defun expand (path)
  (mapcar (lambda (node) (cons node path)) (get (first path) 'Adj))
)

;;First revision:

(defun search2 (start finish)
  (search2-aux (list (list start)) finish)
)

(defun search2-aux (pathlist goal)
  (cond ((endp pathlist) nil)
        ((equal goal (first (first pathlist))) (reverse (first pathlist)))
        (T (search2-aux (append (rest pathlist) (expand (first pathlist))) goal))
  )
)

;;Second revision:

(defun search3 (start finish)
  (search3-aux (list (list start)) finish)
)

(defun search3-aux (pathlist goal)
  (cond ((endp pathlist) nil)
        ((equal goal (first (first pathlist))) (reverse (first pathlist)))
        (T (search3-aux (append (expand2 (first pathlist)) (rest pathlist)) goal))
  )
)

(defun expand2 (path)
  (mapcar (lambda (node) (if (not (member node path)) (cons node path)))
          (get (first path) 'adj))
)