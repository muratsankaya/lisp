(defun camelize (str &optional (capitalize nil))
  (let ((s (remove #\- (string-capitalize str) :test #'char=)))
    (if capitalize
        s
        (nstring-downcase s :start 0 :end 1))))

(defun print-char (curr prev char-case stream)
  (and prev (lower-case-p prev) (upper-case-p curr)
       (format stream "-")) 
  (format stream char-case curr)
  curr)

(defun hyphenate (str &optional case)
  (let ((char-case (if (eql case :lower) 
                       "~(~A~)"
                     "~@(~A~)")))
    (with-output-to-string (s)
      (reduce (lambda (prev curr) 
                (print-char curr 
                            prev 
                            char-case
                            s)) 
              (coerce str 'list)
              :initial-value nil))))
