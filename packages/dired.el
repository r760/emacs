(defun r760-dired-first-file ()
  "Go to the first file in dired buffer.

Version: 2023-07-20"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun r760-dired-last-file ()
  "Go to the last file in dired buffer.

Version: 2023-07-20"
  (interactive)
  (end-of-buffer)
  (dired-previous-line 1))
