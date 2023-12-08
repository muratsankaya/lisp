; the idea is to use the stream object as an exit condition
; its a zero cost answer, and its unique
; another alternative is to use gensym function
(defun map-stream (fn s)
    (do ((exp (read s nil s) (read s nil s)))
        ((eql exp s) nil)
      (funcall fn exp)))

(defun map-file (fn pathname)
  (with-open-file (is (make-pathname :name pathname)
                      :direction :input)
    (map-stream fn is)))
