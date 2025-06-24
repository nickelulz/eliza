(in-package #:eliza)

(defun str-contains-any (str tokens)
  (flet ((str-contains-token (token)
	     (not (null (ppcre:scan token str)))))
    (some #'str-contains-token tokens)))

(defun is-farewell (in)
  (str-contains-any
   in
   (list "bye"
	 "farewell"
	 "end")))

(defun randitem (l)
  "returns a random element of a list"
  (nth (random (length l)) l))

(defun eliza-farewell ()
  (randitem
   (list "Ciao." "Goodbye." "See ya later.")))

(defun next-line ()
  (format t "~%"))

(defun eliza-say (s)
  (format t "ELIZA: ~A~%" s))

(defparameter *reflections*
  '(("i" . "you")
    ("am" . "are")
    ("my" . "your")
    ("me" . "you")
    ("you" . "me")
    ("your" . "my")))

(defparameter *raw-rules*
  (list
   (cons "0 YOU 1 ME"
	 (list
	  "What makes you think I 2 you?"
	  "Do you genuinely  believe that I 2 you, or do you simply wish that I 2 you."))
   (cons "*"
	 (list
	  "Please tell me more."
	  "Interesting... continue.."))))

(defun initialize-rules (raw-rules)
  "Transform (string . (responses)) to (string . ((response . 0)))"
  (mapcar (lambda (rule)
            (destructuring-bind (pattern . responses) rule
              (cons pattern
                    (mapcar (lambda (r) (cons r 0)) responses))))
          raw-rules))

(defparameter *rules* (initialize-rules *raw-rules*))

(defun view-rules (raw?)
  (if raw? *raw-rules* *rules*))

(defun join-strings (l delim)
  (reduce (lambda (a b) (concatenate 'string a delim b)) l))

(defun decomp->regex (decomp)
  "Converts a decomposition rule to a regular expression"
  (let ((tokens (cl-ppcre:split "\\s+" decomp))
	(regex-parts '()))
    (loop for token in tokens
	  do (if (cl-ppcre:scan "^\\d$" token)
		 (let* ((n (parse-integer token))
			(regex-group (format nil "((?:\\w+\\s+){0,~D}\\w+)" n)))
		   (push regex-group regex-parts))
		 (push token regex-parts)))
    (join-strings (nreverse regex-parts) "\\s+")))

(defun apply-decomposition (decomp sentence)
  (multiple-value-bind (match captures)
      (cl-ppcre:scan-to-strings (decomp->regex decomp) sentence)
    (cons (not (null captures)) captures)))

(defun apply-reconstruction (reconst tokens)
  (let ((reconst-tokens (cl-ppcre:split "\\s+" reconst))
	(sentence-parts '()))
    (loop for token in reconst-tokens
	  do (if (cl-ppcre:scan "^\\d$" token)
		 (let ((index (1- (parse-integer token))))
		   (push (nth index tokens) sentence-parts))
		 (push token sentence-parts)))
    (join-strings (nreverse sentence-parts) " ")))

;; dummy function for now
(defun reflect (input)
  input)

(defun generate-response (input)
  (loop for (decomp . reconsts) in *rules*
	do (multiple-value-bind (matches? tokens)
	       (apply-decomposition (car decomp) (reflect input))
	     (when matches?
	       (let ((reconst (alexandria:extremum reconsts #'< :key #'cdr)))
		 (rplacd reconst (1+ (cdr reconst)))
		 (return (apply-reconstruction reconst tokens))))))
  (randitem (cdr (assoc "*" *rules*))))

(defun main ()
  (eliza-say "Hello. My name is Eliza, and I'll be your therapist today. How are you feeling?")
  (loop (format t "~%YOU: ")
	(let ((input (string-downcase (read-line))))
	  ;; formatting just so the conversation looks nice
	  (next-line)
	  ;; take randomly between 1-3 seconds to respond to feel
	  ;; more natural
	  (sleep (random 3))
	  (when (is-farewell input)
	    (eliza-say (eliza-farewell))
	    (return))
	  (eliza-say (generate-response input)))))
