(defun mapc-trie (fn trie)
  (mapc #'(lambda (tr)
            (funcall fn
                     (trie-letter tr)
                     tr))
        (trie-sub-tries trie))
  trie)

(defun variable-case (pattern trie i add-words-list)
  (mapc-trie 
   (lambda (letter sub-trie)
     (get-words pattern sub-trie (1+ i) add-words-list)) 
   trie))

(defun fixed-char-case (pattern trie i add-words-list)
  (get-words 
   pattern   
   (subtrie trie (char pattern i))
   (1+ i)
   add-words-list))

(defun get-words (pattern trie i add-words-list)
  (cond ((null trie) 
         nil)
        ((= (length pattern) i) 
         (funcall add-words-list (trie-word trie)))
        ((char= (char pattern i) #\?)
         (variable-case pattern trie i add-words-list))
        (t 
         (fixed-char-case pattern trie i add-words-list))))

(defun pattern-words (pattern trie)
  (let* ((words-list nil)
         (add-words-list 
          (lambda (word)
            (and word
                 (push word words-list)))))
    (get-words pattern trie 0 add-words-list)
    (sort words-list #'string<)))
