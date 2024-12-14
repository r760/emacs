;; -*- mode: elisp -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages"))

(use-package r760-motion
  :ensure nil
  :config
  (global-set-key (kbd "C-x 2") 'r760-motion-split-window-vertically)
  (global-set-key (kbd "C-x 3") 'r760-motion-split-window-horizontally)
  (global-set-key (kbd "M-e") 'r760-motion-set-mark)
  (global-set-key (kbd "M-r") 'r760-motion-delete-mark)
  (global-set-key (kbd "M-w") 'r760-motion-next-mark)
  (global-set-key (kbd "M-q") 'r760-motion-previous-mark)
  (global-set-key (kbd "M-s") 'r760-motion-next-user-buffer)
  (global-set-key (kbd "M-a") 'r760-motion-previous-user-buffer)
  (global-set-key (kbd "C-x e") 'eval-buffer)
  (global-set-key (kbd "<f5>") 'revert-buffer))

(use-package r760-dired
  :ensure nil)

(use-package r760-timesheet
  :ensure nil
  :config
  (setq r760-timesheet-dir "~/wlog"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements "<SPC>;" "current buffer menu")
  (which-key-add-key-based-replacements "<SPC>c" "exec menu")
  (which-key-add-key-based-replacements "<SPC>g" "git")
  (which-key-add-key-based-replacements "<SPC>i" "interactive")
  (which-key-add-key-based-replacements "<SPC>j" "open js buffer")
  (which-key-add-key-based-replacements "<SPC>s" "open scratch buffer")
  (which-key-add-key-based-replacements "<SPC>x" "xref")
  (which-key-add-key-based-replacements "<SPC>e" "emacs config")
  (which-key-add-key-based-replacements "<SPC>ee" "edit")
  (which-key-add-key-based-replacements "<SPC>er" "reload")
  (which-key-add-key-based-replacements "<SPC>o" "org menu"))

(use-package magit
  :ensure t)

(use-package transient
  :ensure t
  :config
  (transient-define-prefix r760-org-menu ()
    "Org Menu"
    [["Org"
      ("x"
       "org export dispath"
       org-export-dispatch
       :transient nil)]
     ["Timesheet"
      ("o"
       "open today's timesheet"
       r760-timesheet
       :transient nil)
      ("w"
       "open weekly report"
       r760-timesheet-weekly-report
       :transient nil)
      ("m"
       "open all timesheets for this month"
       r760-timesheet-month
       :transient nil)]
     ["Agenda"
      ("l"
       "org todo list"
       org-todo-list
       :transient nil)
      ("t"
       "open todo"
       (lambda ()
	 (interactive)
	 (find-file "~/todo.org"))
       :transient nil)]])

  (transient-define-prefix r760-exec-menu ()
    "Compile/Interpret/Evaluate/Debug Menu"
    [
     ["Compile"
      ("c"
       "compile"
       compile
       :transient nil)
      ("r"
       "recompile"
       recompile
       :transient nil)
      ("n"
       "next error"
       next-error
       :transient nil)
      ("p"
       "previous error"
       previous-error
       :transient nil)]
     ["Interpret"
      ("i"
       "interpret"
       executable-interpret
       :transient nil)]
     ["Evaluate LISP"
      ("b"
       "evaluate buffer"
       eval-buffer
       :transient nil)
      ("l"
       "evaluate last sexp"
       eval-last-sexp
       :transient nil)]
     ["Debug"
      ("g"
       "gdb"
       gdb
       :transient nil)]
     ["Shell"
      ("s"
       "shell command"
       shell-command
       :transient nil)
      ("a"
       "async shell command"
       async-shell-command
       :transient nil)
      ]])

  (defun r760-make-buffer-file-executable ()
    (interactive)
    (shell-command-to-string (message "%s %s" "chmod +x" (buffer-file-name))))

  (transient-define-prefix r760-current-buffer-menu ()
    "Currrent Buffer Menu"
    [
     ["Buffer"
      ("s"
       "save"
       save-buffer
       :transient nil)
      ("r"
       "revert"
       revert-buffer
       :transient nil)
      ("l"
       "list matching lines"
       list-matching-lines
       :transient nil)
      ("c"
       "copy matching lines"
       copy-matching-lines
       :transient nil)
      ("k"
       "kill matching lines"
       kill-matching-lines
       :transient nil)
      ("d"
       "delete non matching lines"
       delete-non-matching-lines
       :transient nil)
      ("x"
       "make current buffer file executable"
       r760-make-buffer-file-executable
       :transient nil)
      ]]))

(use-package undo-tree
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package org
  :ensure t
  :config
  (setq org-clock-clocktable-default-properties '(:maxlevel 3))
  (setq org-agenda-files '("~/todo.org"))
  (add-hook 'org-mode-hook
	    (lambda () (undo-tree-mode) (org-indent-mode))))

(use-package org-agenda)
:ensure t

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x rl") 'helm-bookmarks)
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-define-key 'normal 'global (kbd "<SPC><SPC>") 'other-window)
  (evil-define-key 'normal 'global (kbd "<SPC>0") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<SPC>1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<SPC>2") 'r760-motion-split-window-vertically)
  (evil-define-key 'normal 'global (kbd "<SPC>3") 'r760-motion-split-window-horizontally)
  (evil-define-key 'normal 'global (kbd "<SPC>b") 'helm-buffers-list)
  (evil-define-key 'normal 'global (kbd "<SPC>k") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<SPC>f") 'helm-find-files)
  (evil-define-key 'normal 'global (kbd "<SPC>F") 'helm-find)
  (evil-define-key 'normal 'global (kbd "<SPC>d") 'dired)
  (evil-define-key 'normal 'global (kbd "<SPC>D") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<SPC>B") 'helm-bookmarks)
  (evil-define-key 'normal 'global (kbd "<SPC>p") 'helm-show-kill-ring)
  (evil-define-key 'normal 'global (kbd "<SPC>g") (lambda () (interactive) (magit-status) (delete-other-windows)))
  (evil-define-key 'normal 'global (kbd "<SPC>o") 'r760-org-menu)
  (evil-define-key 'normal 'global (kbd "<SPC>c") 'r760-exec-menu)
  (evil-define-key 'normal 'global (kbd "<SPC>ee") (lambda () (interactive) (find-file "~/.emacs")))
  (evil-define-key 'normal 'global (kbd "<SPC>er") (lambda () (interactive) (restart-emacs)))
  (evil-define-key 'normal 'global (kbd "<SPC>;") 'r760-current-buffer-menu)
  (evil-define-key 'normal 'global (kbd "<SPC>r") 'recentf-open-files)
  (evil-define-key 'normal 'global (kbd "<SPC>xd") 'xref-find-definitions)
  (evil-define-key 'normal 'global (kbd "<SPC>xD") 'xref-find-definitions-other-window)
  (evil-define-key 'normal 'global (kbd "<SPC>u") 'winner-undo)
  (evil-define-key 'normal 'global (kbd "<SPC>ib") 'ibuffer)
  (evil-define-key 'normal 'global (kbd "<SPC>if") 'imenu)
  (evil-define-key 'normal 'global (kbd "<SPC>ig") 'rgrep)
  (evil-define-key 'normal 'global (kbd "<SPC>iG") 'grep)
  (evil-define-key 'normal 'global (kbd "<SPC>ip") 'proced)
  (evil-define-key 'normal 'global (kbd "<SPC>s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
  (evil-define-key 'normal 'global (kbd "<SPC>j") (lambda () (interactive) (switch-to-buffer "*js*") (js-mode)))
  (evil-define-key 'normal 'global (kbd "<SPC>m") 'man)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init '(magit))
  (evil-collection-init '(dired))
  (evil-collection-init '(proced))
  (evil-collection-init '(compile))
  (evil-collection-init '(help))
  (evil-collection-init '(grep))
  (evil-collection-init '(ripgrep))
  (evil-collection-init '(man))
  (evil-collection-init '(xref))
  (evil-collection-init '(org)))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :ensure t
  :config
  (setq c-default-style "k&r")
  (setq-default c-basic-offset 2))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "gnu")
  (setq clang-format-executable "/opt/local/bin/clang-format-mp-19"))

(defun r760-gen-clang-format ()
  (interactive)
  (shell-command-to-string (concat clang-format-executable " " "-style=gnu -dump-config > .clang-format")))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (evil-define-key 'normal dired-mode-map
	      (kbd "<SPC>") 'nil
	      (kbd "M-s") 'nil
	      (kbd "C-h") 'dired-up-directory
	      (kbd "C-l") 'dired-find-file
	      (kbd "gg") 'r760-dired-first-file
	      (kbd "G") 'r760-dired-last-file)
	    (auto-revert-mode)))

(eval-after-load 'org-agenda
  '(progn
     (evil-set-initial-state 'org-agenda-mode 'normal)
     (evil-define-key 'normal org-agenda-mode-map
       (kbd "<RET>") 'org-agenda-switch-to
       (kbd "Tab") 'org-agenda-goto
       "q" 'org-agenda-quit
       "t" 'org-agenda-todo
       "j" 'org-agenda-next-line
       "k" 'org-agenda-previous-line)))

(add-hook 'help-mode-hook (lambda () (evil-define-key 'normal help-mode-map (kbd "<SPC>") 'nil)))
(add-hook 'Man-mode-hook (lambda () (evil-define-key 'normal Man-mode-map (kbd "<SPC>") 'nil)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (lsp)
	    (setq lsp-enable-indentation nil)
	    (xref-etags-mode)
	    (evil-local-set-key 'normal (kbd "(") 'c-beginning-of-defun)
	    (evil-local-set-key 'normal (kbd ")") 'c-end-of-defun)
	    (evil-local-set-key 'normal (kbd "C-c C-c") 'clang-format)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'json-pretty-print-buffer)))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(winner-mode 1)
(repeat-mode)
(setq bookmark-save-flag 1)
(setq split-width-threshold 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)
(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(toggle-frame-maximized)
(setq compilation-scroll-output t)

(setq custom-file "~/.emacs.custom.el")
(load-file custom-file)
(server-start)
