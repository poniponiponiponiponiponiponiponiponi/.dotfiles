(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(add-to-list 'default-frame-alist '(font . "DejaVuSansM Nerd Font Mono 13"))
(defvar default-font "DejaVuSansM Nerd Font Mono 13")
(set-frame-font "DejaVuSansM Nerd Font Mono 13" nil t)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'gud-mode-hook 'ansi-color-for-comint-mode-on)

; make eglot more responsive
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.2)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
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
(global-set-key [remap list-buffers] 'ibuffer)
(setq tab-bar-show nil)
(tab-bar-mode 1)
(tab-bar-history-mode 1)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
(global-set-key (kbd "C-c c") 'kill-buffer-and-window)


(use-package hungry-delete
  :config
  (global-hungry-delete-mode))
(use-package projectile
  :config
  ;;(projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(setq solarized-distinct-fringe-background t)
(setq solarized-scale-org-headlines nil)
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

(use-package rg)
(use-package eglot
  :hook
  (prog-mode . (lambda () (unless (eq major-mode 'emacs-lisp-mode)
                 (eglot-ensure))))
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 0))))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

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
  (counsel-mode)
  (define-key eshell-mode-map (kbd "C-M-r") 'counsel-esh-history))
;; (use-package swiper
;;   :config
;;   (setq swiper-use-visual-line nil)
;;   (setq swiper-use-visual-line-p (lambda (a) nil)))
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))
(use-package gcmh
  :config
  (gcmh-mode 1))
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

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
(use-package ox-reveal
  :config
  (setq org-reveal-mathjax t)
  (setq org-reveal-root "/home/poni/node_modules/reveal.js"))
(use-package htmlize)

(use-package eat
  :bind
  ("M-<return>" . 'eshell)
  :config
  (eat-eshell-mode)
  (add-hook 'eat-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))
  (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0))))

;; (use-package vterm
;;   :bind
;;   ("M-<return>" . 'vterm)
;;   (:map 
;;   vterm-mode-map
;;       	("C-y" . vterm-yank))
;;   :config
;;   (setq vterm-timer-delay 0.02)
;;   (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
;;   (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0))))
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
;; (use-package elcord
;;   :config
;;   (elcord-mode))
(use-package org-download)
;; (use-package highlight-indent-guides
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   :init
;;   (setq highlight-indent-guides-character ?.)
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode))

(use-package multi-vterm)
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
(defun kpwn-info-variable (str)
  "Insert a string into the current buffer."
  (interactive "sVariable: ") 
  (insert "printf(\"" str ": 0x%lx\", " str ");"))

(add-hook 'python-mode
          (lambda () (global-set-key (kbd "M-p") 'pwn-info-variable)))
(add-hook 'c-mode
          (lambda () (local-set-key (kbd "M-p") 'kpwn-info-variable)))


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

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" default))
 '(highlight-indent-guides-auto-character-face-perc 100)
 '(org-agenda-files '("/home/poni/org/youtubers.org"))
 '(package-selected-packages
   '(eat beacon undo-tree yasnippet-snippets yasnippet htmlize ox-reveal org-reveal solarized-theme rg hungry-delete multi-vterm projectile project-x ivy-xref sly geiser-guile fireplace snow org-download flycheck elcord sudo-edit rainbow-delimiters rainbow-delimiters-mode rainbow-mode which-key vterm highlight-indent-guides highlight-indentation vline use-package rustic magit gruvbox-theme gcmh eglot counsel company avy))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warning ((t nil)))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :underline nil))))
 '(org-block-end-line ((t (:inherit org-meta-line :extend t :overline nil))))
 '(yas-field-highlight-face ((t (:inherit secondary-selection :underline t)))))
