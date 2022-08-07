;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; requires
(require 'evil)
(require 'company)
(require 'lsp-mode)
(require 'helm)
(require 'which-key)

;; a not-so smart way to refresh emacs window
;; this is used to fix the weird scrollbar artifact
(toggle-frame-fullscreen)
(toggle-frame-fullscreen)

;; turn on evil-mode
(evil-mode 1)

;; turn off evil-mode in term-mode
(evil-set-initial-state 'term-mode 'emacs)

;; fix the clipboard
(setq x-select-enable-clipboard t)

;; turn off menu-bar-mode
(menu-bar-mode 0)

;; turn off scroll-bar-mode
(scroll-bar-mode 0)

;; turn off tool-bar-mode
(tool-bar-mode 0)

;; turn off splash-screen
(setq inhibit-splash-screen t)

;; turn on which-key-mode
(which-key-mode 1)

;; turn on company mode
(add-hook 'after-init-hook 'global-company-mode)

;; turn on electric pair mode
(electric-pair-mode t)

;; set relative numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; set emacs save directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; turn on time mode
(add-hook 'after-init-hook 'display-time-mode)

;; turn on battery mode
(add-hook 'after-init-hook 'display-battery-mode)

;; replacing default key bindings
(global-set-key (kbd "C-x e") 'eval-buffer)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)

(which-key-add-key-based-replacements "C-x 2" "open window below")
(global-set-key
    (kbd "C-x 2")
	(lambda ()
	    "open window below"
	    (interactive)
	    (split-window-vertically)
	    (other-window 1)))

(which-key-add-key-based-replacements "C-x 3" "open window to the right")
(global-set-key
    (kbd "C-x 3")
	(lambda ()
	    "open window to the right"
	    (interactive)
	    (split-window-horizontally)
	    (other-window 1)))

;; (global-set-key (kbd "C-s") 'swiper)

;; adding new key bindings
(global-set-key (kbd "C-c C-f") 'toggle-frame-fullscreen)

(which-key-add-key-based-replacements "C-c C-e" "edit emacs config")
(global-set-key
    (kbd "C-c C-e")
	(lambda ()
	    "edit emacs config"
	    (interactive)
	    (find-file "~/.emacs")))

(which-key-add-key-based-replacements "C-c C-t" "open a mini terminal below")
(global-set-key
    (kbd "C-c C-t")
	(lambda ()
	    "open a mini terminal below"
	    (interactive)
	    (split-window-vertically)
	    (other-window 1)
	    (shrink-window 10)
	    (term "/bin/bash")))
;; C
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c-mode-hook (setq c-default-style "k&r"))
