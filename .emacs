(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 12"))
(defvar default-font "DejaVu Sans Mono 12")

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'gud-mode-hook 'ansi-color-for-comint-mode-on)

; make eglot more responsive
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)

(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
;(global-whitespace-mode 0)
(setq whitespace-line-column 999999)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-visual-line-mode 1)
(setq shift-select-mode nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq kill-buffer-query-functions nil)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(electric-pair-mode 1)
(setq scroll-margin 2)
(setq max-mini-window-height 4)
;;(setq scroll-step 3)
;;(setq scroll-conservatively 1000)
(global-set-key [remap list-buffers] 'ibuffer)
(setq tab-bar-show nil)
(tab-bar-mode 1)
(tab-bar-history-mode 1)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
(global-set-key (kbd "C-c c") 'kill-buffer-and-window)


;(use-package hungry-delete
;  :config
;  (global-hungry-delete-mode))
(use-package projectile
  :config
  ;;(projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))
(use-package eglot
  :hook
  (prog-mode . (lambda () (unless (eq major-mode 'emacs-lisp-mode)
                 (eglot-ensure))))
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 1))))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package avy
  :bind
  ("M-s" . avy-goto-char))
(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))
(use-package counsel
  :config
  (counsel-mode))
(use-package swiper
  :config
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))
(use-package gcmh
  :config
  (gcmh-mode 1))
(use-package magit)
(use-package org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (add-hook 'org-mode-hook '(lambda () (visual-line-mode 1)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (C . t)
     (shell . t))))
(use-package vterm
  :bind
  ("M-<return>" . 'vterm)
  (:map 
  vterm-mode-map
      	("C-y" . vterm-yank))
  :config
  (setq vterm-timer-delay 0.02)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0))))
(use-package which-key
  :config
  (which-key-mode))
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package sudo-edit)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(use-package elcord
  :config
  (elcord-mode))
(use-package org-download)
;; (use-package highlight-indent-guides
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   :init
;;   (setq highlight-indent-guides-character ?.)
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode))

;(use-package geiser-guile)
;(use-package geiser)
;(use-package sly)
(use-package multi-vterm :ensure t)
(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-ahgo --group-directories-first")))
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src") 
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(defun pwn-info-variable (str)
  "Insert a string into the current buffer."
  (interactive "sVariable: ") 
  (insert "info(f\"" str ": {hex(" str ")}\")"))
(global-set-key (kbd "M-p") 'pwn-info-variable)

(defun split-term (str)
  "split terminal"
  (interactive "sExecute: ")
  (require 'vterm)
  (split-window-right)
  (other-window 1)
  (multi-vterm)
  (vterm-send-string str)
  (vterm-send-return)
  (text-scale-decrease 2))
(add-to-list 'vterm-eval-cmds '("split-term" split-term))

;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-auto-character-face-perc 100)
 '(org-agenda-files '("/home/poni/org/youtubers.org"))
 '(package-selected-packages
   '(hungry-delete multi-vterm projectile project-x ivy-xref sly geiser-guile fireplace snow org-download flycheck elcord sudo-edit rainbow-delimiters rainbow-delimiters-mode rainbow-mode which-key vterm highlight-indent-guides highlight-indentation vline use-package rustic magit gruvbox-theme gcmh eglot counsel company avy))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warning ((t nil))))
