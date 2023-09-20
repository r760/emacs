;; motion across windows

(defun r760-motion-split-window-vertically ()
  "Split the window vertically and select it.

Version: 2023-07-22"
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun r760-motion-split-window-horizontally ()
  "Split the window horizontally and select it.

Version: 2023-07-22"
  (interactive)
  (split-window-horizontally)
  (other-window 1))

;; motion across buffers

(defun r760-motion--user-buffer-p ()
  "Return true if the current buffer is a user buffer, nil otherwise.

Version: 2023-09-19"
  (if (and
       (or (not (string-match "^\*.*\*$" (buffer-name))) (string-match "^\*scratch\*.*$" (buffer-name)))
       (not (or (string-match ".*magit.*" (format "%S" major-mode)) (string-equal major-mode "dired-mode"))))
      t))

(defun r760-motion-next-user-buffer ()
  "Switch to the next user buffer.

Version: 2023-07-20"
  (interactive)
  (let ((orig-buffer (buffer-name)))
    (next-buffer)
    (while (and (not (r760-motion--user-buffer-p)) (not (string-equal (buffer-name) orig-buffer))) (next-buffer))))

(defun r760-motion-previous-user-buffer ()
  "Switch to the previous user buffer.

Version: 2023-07-20"
  (interactive)
  (let ((orig-buffer (buffer-name)))
    (previous-buffer)
    (while (and (not (r760-motion--user-buffer-p)) (not (string-equal (buffer-name) orig-buffer))) (previous-buffer))))

(require 'vterm)

(defvar r760-motion--vterm-return-buffer nil
  "The name of the buffer to return to from vterm.

Version: 2023-07-22")

(defun r760-motion-toggle-vterm ()
  "Switch back and forth between the current buffer and vterm.

Version: 2023-07-22"
  (interactive)
  (let ((cb (buffer-name)) (vtb "*vterm*"))
    (if (string-equal cb vtb)
	(switch-to-buffer r760-motion--vterm-return-buffer)
      (setq r760-motion--vterm-return-buffer cb)
      (if (not (get-buffer vtb))
	  (vterm)
	(switch-to-buffer vtb)))))

;; motion in current buffer

(defun r760-motion--mark-prefix ()
  "Return the mark prefix for the current major mode.

Version: 2023-07-22"
  (cond ((string-equal major-mode "emacs-lisp-mode") ";; ")
	((string-equal major-mode "c-mode") "// ")
	(t "# ")))

(defun r760-motion-set-mark ()
  "Set a mark above the current line.

Version: 2023-07-20"
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert (concat (r760-motion--mark-prefix) "r760-motion-tag"))
  (beginning-of-line)
  (message "Mark saved"))

(defun r760-motion-delete-mark ()
  "Delete the mark at the current line or delete all the marks in the current buffer if called with a prefix argument.

Version: 2023-07-22"
  (interactive)
  (let ((r760-motion (concat (r760-motion--mark-prefix) "r760-motion-tag")))
    (if current-prefix-arg
	(progn
	  (save-excursion
	    (beginning-of-buffer)
	    (delete-matching-lines r760-motion)
	    (message "All marks deleted in the current buffer")))
      (progn
	(if (string-equal (buffer-substring-no-properties (line-beginning-position) (line-end-position)) r760-motion)
	    (progn
	      (delete-line)
	      (message "Mark deleted")))))))

(defun r760-motion-next-mark ()
  "Go to the next mark in the current buffer.

Version: 2023-07-22"
  (interactive)
  (let ((spos (point)) (r760-motion (concat (r760-motion--mark-prefix) "r760-motion-tag")))
    (end-of-line)
    (if (search-forward r760-motion nil t)
	(progn
	  (beginning-of-line)
	  (message "Mark cycled"))
      (progn
	(goto-char spos)
	(beginning-of-buffer)
	(if (search-forward r760-motion nil t)
	    (progn
	      (beginning-of-line)
	      (message "Mark cycled"))
	  (goto-char spos))))))

(defun r760-motion-previous-mark ()
  "Go to the previous mark in the current buffer.

Version: 2023-07-22"
  (interactive)
  (let ((spos (point)) (r760-motion (concat (r760-motion--mark-prefix) "r760-motion-tag")))
    (beginning-of-line)
    (if (search-backward r760-motion nil t)
	(progn
	  (beginning-of-line)
	  (message "Mark cycled"))
      (progn
	(goto-char spos)
	(end-of-buffer)
	(if (search-backward r760-motion nil t)
	    (progn
	      (beginning-of-line)
	      (message "Mark cycled"))
	  (goto-char spos))))))
