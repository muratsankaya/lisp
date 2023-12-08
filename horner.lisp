(defun horner (x &rest coefs)
  (reduce (lambda (acc coef) (+ (* acc x) coef)) (cdr coefs) 
              :initial-value (car coefs)))
