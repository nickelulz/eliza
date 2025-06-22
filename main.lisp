(in-package #:eliza-lisp)

(defparameter *reflections*
  '(("i" . "you")
    ("am" . "are")
    ("my" . "your")
    ("me" . "you")
    ("you" . "me")
    ("your" . "my")))

(defun reflect (words)
  (mapcar (lambda (w)
            (or (cdr (assoc (string-downcase w) *reflections* :test #'string=))
                w))
          words))

(defparameter *rules*
  '(("i need *" . ("Why do you need *?" "Would it help you to get *?"))
    ("i am *" . ("How long have you been *?" "Do you enjoy being *?"))
    ("i feel *" . ("Do you often feel *?" "What makes you feel *?"))
    ("*" . ("Please tell me more." "Let's talk about that."))))

(defun match-pattern (input pattern)
  (let* ((input-words (remove "" (split-sequence:split-sequence #\Space input)))
         (pattern-words (remove "" (split-sequence:split-sequence #\Space pattern)))
         (star-pos (position "*" pattern-words :test #'string=)))
    (cond
      (star-pos
       (let* ((before (subseq pattern-words 0 star-pos))
              (after (subseq pattern-words (1+ star-pos)))
              (match-body (subseq input-words (length before)
                                  (- (length input-words) (length after)))))
         (values t match-body)))
      ((equal input-words pattern-words)
       (values t nil))
      (t (values nil nil)))))

(defun random-elt (list)
  (nth (random (length list)) list))

(defun string-join (strings separator)
  (apply #'concatenate 'string
         (first strings)
         (mapcar (lambda (s) (concatenate 'string separator s))
                 (rest strings))))

(defun generate-response (input)
  "Generates the next response based on user sentence 's'
   See: https://en.wikipedia.org/wiki/ELIZA#Pseudocode"
  (let ((input-str (string-downcase input)))
    (loop for (pattern . responses) in *rules*
          do (multiple-value-bind (matched captured) (match-pattern input-str pattern)
               (when matched
                 (let* ((reflected (reflect captured))
                        (filled (substitute #\* #\* (random-elt responses)))
                        (final (cl-ppcre:regex-replace "\\*" filled (string-join reflected " "))))
                   (return final)))))
    ;; fallback
    (random-elt (cdr (assoc "*" *rules*)))))

(defun is-farewell (s)
  "Returns whether or not 's' is a farewell (some kind
   of goodbye)"
  (string= s "bye"))

(defun randitem (l)
  "returns a random element of a list"
  (nth l (random (length l))))

(defun eliza-farewell ()
  (randitem
   (list "Goodbye."
	 "Ciao."
	 "Talk to you later.")))

(defun eliza-say (s)
  "Produces an ELIZA response"
  (format t "ELIZA: ~A~%" s))

(defun main ()
  "Entry point to the program"
  (eliza-say "Hello. How are you feeling today?")
  ;; Read user input
  (loop (format t "~%YOU: ")
	(let ((input (read-line)))
	  (when (is-farewell input)
	    (eliza-say (eliza-farewell))
	    (return))
	  (eliza-say (generate-response input)))))
