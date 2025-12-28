;; -*- mode: elisp; lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages"))

(defun r760-zoom-in ()
    (interactive)
    (text-scale-adjust 1))

(defun r760-zoom-out ()
    (interactive)
    (text-scale-adjust -1))

(use-package r760-motion
  :ensure nil
  :config
  (global-set-key (kbd "C-x e") 'eval-buffer))

(use-package r760-dired
  :ensure nil
  :config
  (setq dired-listing-switches "-lah"))

(use-package r760-timesheet
  :ensure nil
  :config
  (setq r760-timesheet-dir "~/wlog"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements "<SPC>q" "exec menu")
  (which-key-add-key-based-replacements "<SPC>g" "git")
  (which-key-add-key-based-replacements "<SPC>z" "edit config")
  (which-key-add-key-based-replacements "<SPC>u" "open js buffer")
  (which-key-add-key-based-replacements "<SPC>i" "open scratch buffer")
  (which-key-add-key-based-replacements "<SPC>o" "timesheet")
  (which-key-add-key-based-replacements "<SPC>;" "current buffer menu"))

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

  (defun r760-make-buffer-file-executable ()
    (interactive)
    (shell-command-to-string (message "%s %s" "chmod +x" (buffer-file-name))))

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
       r760-interpret-file
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

  (defun r760-interpret-file ()
    (interactive)
    (async-shell-command (buffer-file-name)))

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
  (add-hook 'text-mode-hook 'undo-tree-mode)
  (add-hook 'makefile-mode-hook 'undo-tree-mode)
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

(use-package avy
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer t)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'term-mode 'emacs)

  ;; lowercase global marks
  (advice-add 'evil-global-marker-p :override
              (lambda (char)
                "Whether CHAR denotes a global marker."
                (or (and (>= char ?a) (<= char ?z))
                    (assq char (default-value 'evil-markers-alist)))))

  (evil-define-key 'normal 'global (kbd "q") 'evil-goto-mark-line)
  (evil-define-key 'normal 'global (kbd "M-q") 'evil-record-macro)

  (evil-define-key 'normal 'global (kbd "s-<return>") 'save-buffer)

  ;; isearch-mode-map
  (evil-define-key 'insert 'global (kbd "s-<return>")
    (lambda ()
      "Cycle =, +, - , _.

Version: 2025-10-09"
      (interactive)
      (let ((x (preceding-char)))
        (cond
         ((char-equal x ? ) (insert "="))
         ((char-equal x ?=) (left-char) (delete-char 1) (insert "+"))
         ((char-equal x ?+) (left-char) (delete-char 1) (insert "-"))
         ((char-equal x ?_) (left-char) (delete-char 1) (insert "-"))
         ((char-equal x ?-) (left-char) (delete-char 1) (insert "_"))
         (t (if (minibuffer-window-active-p (selected-window)) (insert "-") (insert "_")))))))

  ;; visual maps
  (evil-define-key 'visual 'global (kbd "q") 'eval-region)
  (evil-define-key 'visual 'global (kbd "v") 'evil-visual-line)

  ;; function maps
  (evil-define-key 'normal 'global (kbd "<f5>") 'revert-buffer)

  (evil-define-key 'normal 'global (kbd "s-s") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "s-f") 'evil-search-forward)

  (evil-define-key 'normal 'global (kbd "s-i") 'r760-motion-previous-user-buffer)
  (evil-define-key 'normal 'global (kbd "s-o") 'r760-motion-next-user-buffer)

  (evil-define-key 'normal 'global (kbd "s-j") 'evil-forward-paragraph)
  (evil-define-key 'normal 'global (kbd "s-k") 'evil-backward-paragraph)

  (evil-define-key 'normal 'global (kbd "s-h") 'r760-zoom-out)
  (evil-define-key 'normal 'global (kbd "s-l") 'r760-zoom-in)

  (evil-define-key 'normal 'global (kbd "s-<wheel-down>") 'r760-zoom-out)
  (evil-define-key 'normal 'global (kbd "s-<wheel-up>") 'r760-zoom-in)

  ;; space maps
  (evil-define-key 'normal 'global (kbd "<SPC><SPC>") 'other-window)
  (evil-define-key 'normal 'global (kbd "<SPC>0") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<SPC>1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<SPC>2") 'r760-motion-split-window-vertically)
  (evil-define-key 'normal 'global (kbd "<SPC>3") 'r760-motion-split-window-horizontally)

  (evil-define-key 'normal 'global (kbd "<SPC>qe") (lambda () (interactive) (find-file "~/.emacs")))
  (evil-define-key 'normal 'global (kbd "<SPC>qr") 'restart-emacs)

  (evil-define-key 'normal 'global (kbd "<SPC>w") 'imenu)
  (evil-define-key 'normal 'global (kbd "<SPC>e") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<SPC>r") 'helm-find)
  (evil-define-key 'normal 'global (kbd "<SPC>t") 'rgrep)

  (evil-define-key 'normal 'global (kbd "<SPC>a") 'evil-avy-goto-char)
  (evil-define-key 'normal 'global (kbd "<SPC>s") 'xref-find-definitions)
  (evil-define-key 'normal 'global (kbd "<SPC>d") 'dired)
  (evil-define-key 'normal 'global (kbd "<SPC>f") 'helm-find-files)
  (evil-define-key 'normal 'global (kbd "<SPC>g") (lambda () (interactive) (magit-status) (delete-other-windows)))

  (evil-define-key 'normal 'global (kbd "<SPC>c") 'r760-exec-menu)
  (evil-define-key 'normal 'global (kbd "<SPC>x") 'helm-M-x)

  (evil-define-key 'normal 'global (kbd "<SPC>y") (lambda () (interactive) (switch-to-buffer "*js*") (js-mode)))
  (evil-define-key 'normal 'global (kbd "<SPC>u") 'helm-bookmarks)
  (evil-define-key 'normal 'global (kbd "<SPC>i") 'recentf-open-files)
  (evil-define-key 'normal 'global (kbd "<SPC>o") 'r760-org-menu)
  (evil-define-key 'normal 'global (kbd "<SPC>p") 'helm-show-kill-ring)

  (evil-define-key 'normal 'global (kbd "<SPC>h") (lambda () (interactive) (switch-to-buffer "*scratch*")))
  (evil-define-key 'normal 'global (kbd "<SPC>j") 'helm-buffers-list)
  (evil-define-key 'normal 'global (kbd "<SPC>k") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<SPC>;") 'r760-current-buffer-menu)

  (evil-define-key 'normal 'global (kbd "<SPC>n") 'switch-to-buffer)
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

(use-package evil-mc
  :ensure t
  :config (global-evil-mc-mode 1))


(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :ensure t
  :config
  (setq c-default-style "gnu")
  (setq-default c-basic-offset 2))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "gnu")
  (setq clang-format-executable "/opt/homebrew/bin/clang-format"))

(defun r760-gen-clang-format ()
  (interactive)
  (shell-command-to-string (concat clang-format-executable " " "-style=gnu -dump-config | sed 's|Language.*Cpp|Language: C|g;s|ColumnLimit.*|ColumnLimit: 0|g' > .clang-format")))


(use-package gruber-darker-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(use-package material-theme
  :ensure t)

(use-package doom-themes
  :ensure t)


(add-hook 'dired-mode-hook
	  (lambda ()
	    (evil-define-key 'normal dired-mode-map
	      (kbd "<SPC>") 'nil
	      (kbd "h") 'dired-up-directory
	      (kbd "l") 'dired-find-file
	      (kbd "gg") 'r760-dired-first-file
	      (kbd "G") 'r760-dired-last-file
	      (kbd "C-c C-j") 'rwi-zon-or-dezon)
            (evil-mc-mode 0)
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

;; no tabs
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-hook
          (lambda ()
            (lsp)
            (setq lsp-headerline-breadcrumb-enable nil)
            (setq lsp-enable-indentation nil)
            (xref-etags-mode)
            (evil-local-set-key 'normal (kbd "(") 'c-beginning-of-defun)
            (evil-local-set-key 'normal (kbd ")") 'c-end-of-defun)
            (evil-local-set-key 'normal (kbd "C-c C-c") 'clang-format)
            (evil-local-set-key 'normal (kbd "<tab>") 'evil-toggle-fold)
            (hs-minor-mode)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'json-pretty-print-buffer)))

(add-hook 'js-json-mode-hook
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

;;(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)
(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(toggle-frame-maximized)
(setq compilation-scroll-output t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-file "~/.emacs.custom.el")
(load-file custom-file)

(load-theme 'solarized t)

(setq indent-tabs-mode nil)
(setenv "PATH" (concat "/opt/homebrew/bin/" ":" (getenv "PATH")))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(setq shell-file-name "/bin/zsh")
(setq indent-tabs-mode nil)
(setq Man-sed-command "gsed")
(set-face-attribute 'mode-line nil :height 200)

(unless (server-running-p)
  (server-start))
