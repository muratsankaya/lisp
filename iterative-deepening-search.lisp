(defun shortest-path (start end net)
  (reverse
   (ids (list start) 
        (lambda (state) (eql state end))
        (lambda (path) (cdr (assoc (car path) net))))))

; '(nil) not true failure
(defun ids (path pred gen &optional (depth-limit nil))
  (do ((depth 0 (1+ depth))
       (a-path '(nil) 
               (dls path pred gen depth)))
      ((or (null a-path) ; true failure
           (car a-path) ; a path is found
           (eql depth depth-limit)) ; depth limit is reached
       (if (null (car a-path)) nil a-path))))

(defun get-path (path gen pred n)
  (do ((next-states (funcall gen path) 
                    (cdr next-states))
       (a-path nil 
               (and (not (member (car next-states) path)) 
                    (dls (cons (car next-states) path) 
                         pred 
                         gen 
                         n))))
      ((or (null next-states)
           (car a-path))
       a-path)))

(defun dls (path pred gen n)
  (if (= (length path) (1+ n))
      (let ((end 
             (find-if pred (funcall gen path))))
        (or (and end (cons end path))
            '(nil)))
    (get-path path gen pred n)))
