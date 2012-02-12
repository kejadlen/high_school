;;Alpha Chen
;;AI-period 2
;;9/15/99

(defun cons-two 
  (a b ls)
  (cons a (cons b ls)))

(defun addit
  (a b)
  (if (null a) b
    (if (member a b) 'found
    (append b (list a)))))

(defun my-length
  (ls)
  (if ls (+ (my-length (rest ls)) 1)
  0
  )
)

(defun lengthy
  (ls)
  (setf count 0)
  (lengthy-two ls)
  count
)

(defun lengthy-two
  (ls)
  (cond (ls (setf count (+ count 1)) (lengthy-two (rest ls))))
)

(defun my-assoc
  (atm lst)
  (cond ((member atm (first lst)) (first lst))
        (lst (my-assoc atm (rest lst)))
        ((not lst) NIL)
  )
)
