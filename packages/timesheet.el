(defvar r760-timesheet-dir nil
  "The directory in which timesheet files are stored.

Version: 2024-04-28")

(defun r760-timesheet--path (&optional ts)
  "Get timesheet file path for today (or for some other day if given a timestamp).

Version: 2024-04-28"
  (concat r760-timesheet-dir "/" (format-time-string "%Y-%m-%d" ts) ".org"))

(defun r760-timesheet ()
  "Open today's timesheet and update clock report.

Version: 2024-04-28"
  (interactive)
  (let ((fpath nil))
    (if (not (file-directory-p r760-timesheet-dir))
	(dired-create-directory timesheet-path)
      nil)
    (setq fpath (r760-timesheet--path))
    (find-file fpath)
    (if (file-exists-p fpath) nil
      (insert "* TIMESHEET")
      (org-clock-report)
      (end-of-buffer)
      (insert "** PROJECT")
      (org-clock-in)
      (org-show-all)
      (save-buffer)
      (end-of-buffer))
    (save-excursion
      (end-of-buffer)
      (while (search-backward-regexp "CLOCK:" nil t)
        (org-ctrl-c-ctrl-c))
      (if (search-backward-regexp "BEGIN" nil t)
          (org-ctrl-c-ctrl-c)))))

(defun r760-timesheet--week ()
  "Get a list of all timesheet file paths for this week.

Version: 2024-04-28"
  (let ((nd nil) (ts nil) (lst nil))
    (setq ts (string-to-number (format-time-string "%s")))
    (setq nd (string-to-number (format-time-string "%u")))
    (while (>= nd 2)
      (setq nd (- nd 1))
      (setq ts (- ts (* 24 60 60))))
    (while (<= nd 5)
      (setq lst (cons (r760-timesheet--path (seconds-to-time ts)) lst))
      (setq nd (+ nd 1))
      (setq ts (+ ts (* 24 60 60))))
    (nreverse lst)))

(defun r760-timesheet--summary (fpath)
  "Extract total time for the given timesheet file.

Version: 2024-04-28"
  (let ((from nil) (to nil) (summary nil))
    (setq summary (with-temp-buffer
		    (insert-file-contents fpath)
		    (replace-string "Total time" fpath)
		    (beginning-of-buffer)
		    (re-search-forward r760-timesheet-dir)
		    (beginning-of-line)
		    (setq from (point))
		    (re-search-forward ":")
		    (re-search-forward "|")
		    (setq to (point))
		    (buffer-substring from to)))
    (setq summary (replace-regexp-in-string "\*.*\/" "*" summary))
    (setq summary (replace-regexp-in-string "\.org" "" summary))
    (setq summary (replace-regexp-in-string "\*" "" summary))))

(defun r760-timesheet-weekly-report ()
  "Open weekly timesheet report.

Version: 2024-04-28"
  (interactive)
  (let ((fpath nil) (lst nil))
    (if (get-buffer "*timesheet*")
	(kill-buffer "*timesheet*"))
    (switch-to-buffer "*timesheet*")
    (setq lst (r760-timesheet--week))
    (insert "| Date | Time |")
    (newline)
    (insert "|-------+-------|")
    (newline)
    (while lst
      (setq fpath (pop lst))
      (if (file-exists-p fpath)
	  (progn 
	    (insert (r760-timesheet--summary fpath))
	    (newline))
	(progn
	  (insert (concat "| " (replace-regexp-in-string "\.org" "" (replace-regexp-in-string ".*/" "" fpath)) " | 0:00 |"))
	  (newline))))
    (org-mode)
    (insert "|------------+----------|")
    (newline)
    (insert "| Total | 00:00:00 |")
    (newline)
    (insert "#+TBLFM: @>$2=vsum(@I..@II);T")
    (org-ctrl-c-ctrl-c)))
