(defun shortest-path (start end net)
  (reverse
   (bfs (list (list start))
        (lambda (state) 
          (eql state end))
        (lambda (path) 
          (cdr (assoc (car path) net))))))

(defun bfs (paths pred gen)
  (if (null paths)
      nil
    (let ((path (car paths)))
      (let ((node (car path))
            (end (find-if pred (funcall gen path))))
        (if end
            (cons end path)
          (bfs (append 
                (cdr paths)
                (new-paths path gen))
               pred
               gen))))))

(defun new-paths (path gen)
  (mapcan (lambda (n)
            (if (member n path)
                nil
              (list (cons n path))))
          (funcall gen path)))
