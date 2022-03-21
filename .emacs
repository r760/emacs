(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; turn on evil-mode
(require 'evil)
(evil-mode 1)

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

;; relative numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; set emacs save directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-s") 'swiper)

;; C
(require 'lsp-mode)
(add-hook 'c-mode-hook
      (lambda ()
	(add-hook 'c-mode-hook #'lsp)))
