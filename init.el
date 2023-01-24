;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Adjust some functions of native emacs

(electric-pair-mode t)                           ; Completion for parentheses
(add-hook 'prog-mode-hook #'show-paren-mode)     ; When the cursor is on a parenthesis, highlight the other one
(column-number-mode t)                           ; Show column number in mode line
(global-auto-revert-mode t)                      ; When file is changed by other program, emacs can refresh the buffer
(delete-selection-mode t)                        ; When select some words, type to replace, not just insert
(setq inhibit-startup-message t)                 ; Close emacs's welcome
(setq make-backup-files nil)                     ; Close the auto backup
(add-hook 'prog-mode-hook #'hs-minor-mode)       ; Be able to fold the code blocks
(global-display-line-numbers-mode 1)             ; Show the line numbers
(tool-bar-mode -1)                               ; Close the tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)); Close the scroll bar
;(savehist-mode 1)                               ; Open buffer history
;(setq display-line-numbers-type 'relative)      ; Show relative line number
(add-to-list 'default-frame-alist '(width . 90)) ; Set default width
(add-to-list 'default-frame-alist '(height . 45)); Set default height


;; Key bind

(global-set-key (kbd "RET") 'newline-and-indent)

(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))

(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)
(global-set-key (kbd "C-j") nil)

;; Font setting, deal with the display error of Chinese word

(set-fontset-font "fontset-default"
'unicode '("微软雅黑" . "unicode-bmp"))


;; ELPA,MELPA setting

(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)


;; Package

(eval-when-compile
  (require 'use-package))

(use-package good-scroll
  :ensure t
  :if window-system
  :init (good-scroll-mode))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil)
  :hook
  (prog-mode . flycheck-mode))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-more-italic t))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  ;; (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 10)))
  (dashboard-setup-startup-hook))

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :ensure t
  :bind
  (("C-j C-SPC" . avy-goto-char-timer)))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 500)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
    (setq lsp-headerline-breadcrumb-enable t))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions) ; M-.
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)   ; M-?
  (setq lsp-ui-doc-position 'top))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package magit
  :ensure t)

(use-package neotree
  :ensure t
  :bind (("<f8>" . 'neotree-toggle)))

(use-package c++-mode
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))


;; Theme

(load-theme 'solarized-light t)


;; Other

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
