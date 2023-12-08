(defstruct (trie (:print-function print-trie))
  (sub-tries nil) ; a list of tries
  (letter nil)
  (count 0)
  (word nil) ; nil or the word it contains
)

(defun print-trie (node stream depth) 
  (format stream 
          "<letter: ~S, count: ~S>" 
          (trie-letter node) 
          (trie-count node)))

(defun add-char (c trie)
  (car (push (make-trie :letter c
                        :count 0)
             (trie-sub-tries trie))))

(defun search-char (c trie)
  (find c 
        (trie-sub-tries trie) 
        :key #'trie-letter
        :test #'char= ))

; flag -> t -> adding t
; if adding is t it will add a sub-trie
(defun get-sub-trie (c trie flag)
  (let ((sub-trie (search-char c trie)))
    (cond (flag 
           (incf (trie-count trie))
           (or sub-trie (add-char c trie)))
          (t sub-trie))))
    
; flag -> t -> adding t  
(defun traverse-trie (str trie flag)
  (do ((i 0 (1+ i))
       (tr trie (get-sub-trie (char str i)
                              tr
                              flag)))
      ((or (null tr)
           (= i (length str)))
       tr)))

(defun add-word (str trie)
  (or (traverse-trie str trie nil) ; if the word does not exist
      (let ((trie-leaf (traverse-trie str trie t)))
        (setf (trie-word trie-leaf) str)
        (incf (trie-count trie-leaf))))
  trie)

(defun subtrie (trie &rest chars)
  (traverse-trie (coerce chars 'string)
                 trie
                 nil))
       
(defun mapc-trie (fn trie)
  (mapc #'(lambda (tr)
            (funcall fn
                     (trie-letter tr)
                     tr))
        (trie-sub-tries trie))
  trie)

(defun read-words (file trie)
  (with-open-file (in file :direction :input
                      :if-does-not-exist nil)
    (loop for line = (read-line in nil)
          while line do (add-word line trie)))
  trie)
