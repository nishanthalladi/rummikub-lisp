(ql:quickload "alexandria")

(defun firsts (a) (loop for i in a collect (first i)))
(defun seconds (a) (loop for i in a collect (second i)))
(defun list-equal (lst) (every #'equal lst (rest lst)))

; returns a hand through values as a list of size size and the new deck 
; ex: (hand '(1 2 3 4 5) 2) => (1 2) (3 4 5)
(defun hand (cards size)
    (setf cards-copy cards)
    (values (loop for i to (- size 1) collect (nth i cards)
                do (setf cards-copy (cdr cards-copy))) 
            cards-copy))

; returns a deck of 106 cards in the format (COLOR NUMBER) ex: (RED 12). Jokers are valued -1
(defun setup-deck ()
    (setf colors (list 'red 'blue 'orange 'black))
    (setf numbers (loop for i from 1 to 13 collect i))
    (setf cards (alexandria:map-product 'list colors numbers))
    (alexandria:shuffle (append cards cards '(black -1) '(red -1))))

; returns the modified hand given the sequence 
; ex: (take-sequence '(1 2 3 4 5) '(3 4 5)) => (1 2)
(defun take-sequence (hand seq)
    (when (not (and (check-sequence seq) (has-seq hand seq))) return hand)
    (loop for card in hand unless (member card seq :test #'equal) collect card))

(defun has-seq (hand seq)
    (equal seq (loop for card in hand when (member card seq) collect card)))

; checks whether a sequence of cards is valid
; ex: (check-sequence '((RED 3) (RED 1) (RED 2))) => T
; ex: (check-sequence '((RED 3) (RED 3) (RED 1) (RED 2))) => nil
; ex: (check-sequence '((RED 5) (BLUE 5) (BLACK 5))) => T
; ex: (check-sequence '((RED 5) (BLUE 5) (BLUE 5) (BLACK 5))) => nil
(defun check-sequence (seqo)
    (cond ((< (length seqo) 3) nil)
          ((list-equal (firsts seqo)) (seq-check-nums (sort (seconds seqo) #'<)))
          ((list-equal (seconds seqo)) (seq-check-color (firsts seqo)))
          (t nil)))

(defun seq-check-color (seq)
    (when (or (< 4 (length seq)) (> 3 (length seq))) (return nil))
    (equal (remove-duplicates seq) seq))

; checks whether a sequence of numbers is valid
; ex: (seq-check-nums '(0 1 2 3 4 5)) => T
; ex: (seq-check-nums '(0 1 2 3 6 5)) => nil
(defun seq-check-nums (seq)
    (let ((seqc (flatten-joker seq)))
        (equal seqc (loop repeat (length seq) for i from (first seqc) collect i))))

; change joker into number that fits in the sequence
; ex: (flatten-joker '(0 1 2 -1 4 5)) => (0 1 2 3 4 5)
(defun flatten-joker (seqo)
    (setf seq (copy-list seqo))
    (when (= (first seq) -1) (setf (first seq) (- (second seq) 1)))
    (loop for i to (- (length seq) 1) 
        if (= (nth i seq) -1) do 
            (setf (nth i seq) (+ 1 (nth (- i 1) seq))))
    seq)