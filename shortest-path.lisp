; using non-local exit to optimize the number of levels of search
(defun shortest-path (start end net)
  (catch 'abort (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (null queue)
      nil
    (let ((path (car queue)))
        (let ((node (car path)))
          (bfs end
               (append (cdr queue)
                       (new-paths path node net end))
               net)))))

(defun new-paths (path node net end)
  (mapcan (lambda (n)
            (cond ((eql n end)
                   (throw 'abort (reverse (cons n path))))
                  ((member n path) nil)
                  (t (list (cons n path)))))
          (cdr (assoc node net))))

; doing the same optimization without using non-local exit
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
    (let ((path (car queue)))
        (let ((node (car path)))
          (if (member end (assoc node net))
              (reverse (cons end path))
            (bfs end
                 (append (cdr queue)
                         (new-paths path node net))
                 net))))))

(defun new-paths (path node net)
  (mapcan #'(lambda (n)
              (if (not (member n path))
                  (list (cons n path))
                nil))
          (cdr (assoc node net)))) 
