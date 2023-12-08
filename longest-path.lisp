(defun get-path (node path end net)
  (cond ((eql node end) (cons node path))
        ((member node path) nil)
        (t (dfs node (cons node path) end net))))

(defun get-longer (best-path path)
  (if (> (length path) 
         (length best-path))
      path
    best-path))

(defun dfs (node path end net)
    (do ((children (cdr (assoc node net)) (cdr children))
         (best-path nil (get-longer best-path
                                    (get-path
                                     (car children)
                                     path
                                     end
                                     net))))
        ((null children) best-path)))

; for the given problem; if the start and end is eql
; then if start has a path to start the result will be (start start)
; otherwise the result will be (start). Its a little wierd..
(defun longest-path (start end net)
  (reverse
   (or (dfs start (list start) end net)
       (and (eql start end) (list start)))))
