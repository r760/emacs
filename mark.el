(defun r760-mark-prefix ()
  "Return mark prefix for current major mode.

Version: 2023-07-15"
  (interactive)
  (cond ((string-equal major-mode "emacs-lisp-mode") ";; ")
	((string-equal major-mode "c-mode") "// ")
	(t "# ")))

(defun r760-set-mark ()
  "Set a mark above the current line.

Version: 2023-07-15"
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert (concat (r760-mark-prefix) "r760-mark-tag"))
  (beginning-of-line)
  (message "Mark saved"))

(defun r760-delete-mark ()
  "Delete mark at the current line or delete all marks in the current buffer if called with prefix argument.

Version: 2023-07-15"
  (interactive)
  (let ((r760-mark (concat (r760-mark-prefix) "r760-mark-tag")))
    (if current-prefix-arg
	(progn
	  (delete-matching-lines r760-mark)
	  (message "All marks deleted in the current buffer"))
      (progn
	(if (string-equal (buffer-substring-no-properties (line-beginning-position) (line-end-position)) r760-mark)
	    (progn
	      (delete-line)
	      (message "Mark deleted")))))))

(defun r760-next-mark ()
  "Go to the next mark in the current buffer.

Version: 2023-07-15"
  (interactive)
  (let ((spos (point)) (r760-mark (concat (r760-mark-prefix) "r760-mark-tag")))
    (end-of-line)
    (if (search-forward r760-mark nil t)
	(progn
	  (beginning-of-line)
	  (message "Mark cycled"))
      (goto-char spos)
      (progn
	(beginning-of-buffer)
	(if (search-forward r760-mark nil t)
	    (progn
	      (beginning-of-line)
	      (message "Mark cycled"))
	  (goto-char spos))))))

(defun r760-previous-mark ()
  "Go to the previous mark in the current buffer.

Version: 2023-07-15"
  (interactive)
  (let ((spos (point)) (r760-mark (concat (r760-mark-prefix) "r760-mark-tag")))
    (beginning-of-line)
    (if (search-backward r760-mark nil t)
	(progn
	  (beginning-of-line)
	  (message "Mark cycled"))
      (goto-char spos)
      (progn
	(end-of-buffer)
	(if (search-backward r760-mark nil t)
	    (progn
	      (beginning-of-line)
	      (message "Mark cycled"))
	  (goto-char spos))))))
