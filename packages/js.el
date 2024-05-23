(defvar r760-js-token "[:,]"
  "JSON token regex.

Version: 2024-05-23")

(defun r760-js-next-token ()
  "Goto next token.

Version: 2024-05-23"
  (interactive)
  (let ((before nil) (after nil))
    (setq before (point))
    (search-forward-regexp r760-js-token nil t)
    (setq after (point))
    (if (<= (abs (- after before)) 1)
	(progn
	  (right-char)
	  (search-forward-regexp r760-js-token nil t)))))

(defun r760-js-previous-token ()
  "Goto previous token.

Version: 2024-05-23"
  (interactive)
  (search-backward-regexp r760-js-token nil t))
