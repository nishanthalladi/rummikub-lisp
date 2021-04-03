(ql:quickload "alexandria")

(defmacro firsts (a) `(loop for i in ,a collect (first i)))
(defmacro seconds (a) `(loop for i in ,a collect (second i)))

; returns a hand through values as a list of size size and the new deck 
; ex: (hand '(1 2 3 4 5) 2) => (1 2) (3 4 5)
(defun hand (cards size)
    (setf cards-copy cards)
    (values (loop for i to (- size 1) collect (nth i cards)
                do (setf cards-copy (cdr cards-copy))) 
            cards-copy))

; returns a deck of 106 cards in the format (COLOR NUMBER) ex: (RED 12). Jokers are valued -1
(defun setup_deck ()
    (setf colors (list 'red 'blue 'orange 'black))
    (setf numbers (loop for i from 1 to 13 collect i))
    (setf cards (alexandria:map-product 'list colors numbers))
    (alexandria:shuffle (append cards cards '(black -1) '(red -1))))

; checks whether a sequence of cards is valid
; ex: (check_sequence '((RED 3) (RED 1) (RED 2))) => T
; ex: (check_sequence '((RED 3) (RED 3) (RED 1) (RED 2))) => nil
; ex: (check_sequence '((RED 5) (BLUE 5) (BLACK 5))) => T
; ex: (check_sequence '((RED 5) (BLUE 5) (BLUE 5) (BLACK 5))) => nil
(defun check_sequence (seqo)
    (cond ((< (length seqo) 3) nil)
          ((every #'equal (firsts seqo) (rest (firsts seqo))) (seq_check_nums (sort (seconds seqo) #'<)))
          ((every #'equal (seconds seqo) (rest (seconds seqo))) (seq_check_color (firsts seqo)))
          (t nil)))

(defun seq_check_color (seq)
    (when (or (< 4 (length seq)) (> 3 (length seq))) (return nil))
    (equal (remove-duplicates seq) seq))

; checks whether a sequence of numbers is valid
; ex: (seq_check_nums '(0 1 2 3 4 5)) => T
; ex: (seq_check_nums '(0 1 2 3 6 5)) => nil
(defun seq_check_nums (seq)
    (let ((seqc (flatten_joker seq)))
        (equal seqc (loop repeat (length seq) for i from (first seqc) collect i))))

; change joker into number that fits in the sequence
; ex: (flatten_joker '(0 1 2 -1 4 5)) => (0 1 2 3 4 5)
(defun flatten_joker (seq)
    (when (= (first seq) -1) (setf (first seq) (- (second seq) 1)))
    (loop for i to (- (length seq) 1) 
        if (= (nth i seq) -1) do 
            (setf (nth i seq) (+ 1 (nth (- i 1) seq))))
    seq)