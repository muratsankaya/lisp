(defun load-words (path)
  (setf *trie* (read-words path (make-trie))))

(defun bs-sub-trie (trie c)
  (if (char= c #\q)
      (bs-sub-trie (subtrie trie #\q) #\u)
    (subtrie trie c)))

(defun collect-words (board words-trie adj-lst visited indicies)
  (mapcan (lambda (i)
            (word-search
             i
             board
             (bs-sub-trie words-trie (char board i))
             adj-lst
             (cons i visited)))
          indicies))

(defun update-words-list (words-trie words-list)
  (or (and (> (length (trie-word words-trie)) 2)
           (cons (trie-word words-trie) words-list))
      words-list))

(defun word-search (i board words-trie adj-lst visited)
  (and words-trie
       (update-words-list
        words-trie
        (collect-words board
                       words-trie
                       adj-lst
                       visited
                       (remove-if (lambda (j) (member j visited :test #'=))
                                  (cdr (assoc i adj-lst)))))))
       
; i is the current loc
; j is a potential neighbor loc
(defun valid-cord-p (i j sl)
  (and (< (abs (- i j)) (+ sl 2))
       (/= i j)
       (< (abs (- (mod i sl) (mod j sl))) 2)))

; sl: side-length
(defun make-adj-lst (l)
  (let ((list-for-all-i 
         (loop for i from 0 to (1- l) collect i))
        (sl (isqrt l)))
    (mapcar (lambda (i) 
              (cons i (remove-if-not 
                       (lambda (j) (valid-cord-p i j sl))
                       list-for-all-i)))
            list-for-all-i)))

(defun sort-by-len-then-str (a b)
  (if (= (length a) (length b))
      (string< a b)
    (> (length a) (length b))))

(defun solve-boggle (board)
  (let ((words-trie *trie*)
        (adj-lst (make-adj-lst (length board))))
    (sort (remove-duplicates
           (collect-words
            board
            words-trie
            adj-lst
            nil
            (mapcar #'car adj-lst))
           :test #'string=)
          #'sort-by-len-then-str)))
