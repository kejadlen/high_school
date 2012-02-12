;;Assignment 2: Recursion
;;Alpha Chen
;;9/22/99
;;Period 2

(defun listnums2 (num mylist)
  (if (> num 0) (listnums2 (- num 1) (cons num mylist))
    mylist)
)

(defun listnums (num)
  (listnums2 num ())
)

(defun fact (num)
  (fact2 num 1)
)

(defun fact2 (num sum)
  (cond ((> num 0) (fact2 (- num 1) (* num sum)))
        (T sum)
  )
)

(defun greaternum (num lst)
  (cond ((null lst) num)
        ((if (> (first lst) num) (first lst)))
        (T (greaternum num (rest lst))))
)

(defun ins (num lst)
  (if (first lst)
    (cond ((< num (first lst)) (cons num lst))
          (T (cons (first lst) (ins num (rest lst)))))
    (list num))
)

(defun insort (lst)
  (cond ((null lst) ())
    (T (ins (first lst) (insort (rest lst)))))
)

(defun skeleton (lst)
  (cond ((null lst) nil)
        ((and (atom (first lst)) (first lst)) (skeleton (rest lst)))
        (T (cons (skeleton (first lst)) (skeleton (rest lst)))))  
)
