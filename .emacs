;; Blessed are those who code in Emacs, for they shall inherit the flexibility of Lisp;
;; Thy documentation and Thy community light my way,
;; Through the complex mazes of nested functions and cryptic errors,
;; Bringing clarity and wisdom to my coding journey.
;; Even as I confront the heresy of DRM and the treachery of vendor lock-in,
;; I will not be led astray, for Emacs is my savior;
;; Thy keybindings and Thy modes guide my hands and heart,
;; Keeping me true to the path of free and open-source software.



(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 11"))
(defvar default-font "DejaVu Sans Mono 11")
(set-frame-font "DejaVu Sans Mono 11" nil t)
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq kill-buffer-query-functions nil)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(blink-cursor-mode 0)
(electric-pair-mode 1)
(setq scroll-margin 1)
(setq max-mini-window-height 6)
(global-set-key [remap list-buffers] 'ibuffer)
(setq-default project-vc-extra-root-markers '(".project"))

(defun pwn-info-variable (str)
  "Insert a string into the current buffer."
  (interactive "sVariable: ")
  (insert "info(f\"" str ": {hex(" str ")}\")"))
(defun kpwn-info-variable (str)
  "Insert a string into the current buffer."
  (interactive "sVariable: ")
  (insert "printf(\"" str ": 0x%lx\", " str ");"))

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'pwn-info-variable)))
(add-hook 'python-ts-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'pwn-info-variable)))
(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'kpwn-info-variable)))
(add-hook 'c-ts-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'kpwn-info-variable)))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))
(use-package rg)
(use-package which-key
  :config
  (which-key-mode))
(use-package avy
  :bind
  ("M-s" . avy-goto-char))
(use-package gcmh
  :config
  (gcmh-mode 1))
(use-package sudo-edit)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(use-package elcord
  :config
  (elcord-mode))

(use-package indent-bars
  :load-path "~/FOSS/indent-bars"
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)
                          (indent-bars-mode))))))
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq-default solarized-distinct-fringe-background t)
(setq-default solarized-scale-org-headlines nil)
(setq-default solarized-use-variable-pitch nil)
(setq-default solarized-high-contrast-mode-line t)
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(package-install 'flycheck)
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(add-hook 'prog-mode-hook (lambda () (unless (eq major-mode 'emacs-lisp-mode)
                                       (eglot-ensure))))
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 0)))
;; (fset #'jsonrpc--log-event #'ignore)
;; (use-package eglot-booster
;; 	:after eglot

(setq-default eglot-workspace-configuration
              '((:rust-analyzer
                 . ((cargo
                     . ((features . "all")))
                    (diagnostics
                     . ((experimental
                         . ((enable . :json-false)))))
                    (checkOnSave
                     . t)))))
;; THIS WAY DOESN'T WORK NO MATTER WHAT I TRY IDK WHY PLS HELP
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(rustic-mode . ("rust-analyzer" :initializationOptions
;;                               (:cargo (:features "all"))))))


(use-package markdown-mode)

(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))
(use-package htmlize)
(use-package magit)

(use-package corfu
  ;; :custom
  ;; (corfu-auto 1)
  :init
  (global-corfu-mode))

(use-package vertico
  :config
  (setq vertico-resize nil)
  (vertico-mode 1))
(use-package marginalia
  :config
  (marginalia-mode 1))
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))
(use-package consult-flycheck)
(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("C-<return> f" . consult-fd)
  ("C-<return> e" . consult-flycheck)
  ("C-<return> o" . consult-outline)
  ("C-<return> m" . consult-man)
  ("C-<return> l" . consult-line)
  ("C-<return> r" . consult-ripgrep))

(use-package org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-image-actual-width (truncate (* (display-pixel-width) 0.2)))
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
(use-package org-download)

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-ahgo --group-directories-first")))

(use-package eat
  :bind
  ("M-<return>" . 'eshell)
  :config
  (eat-eshell-mode)
  (add-hook 'eat-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))
  (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0))))
(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
	      ("C-r" . consult-history)))
(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode +1))
(defun my-eshell-mode-hook ()
  (define-key eshell-hist-mode-map (kbd "M-s") nil))
(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)

(defun my-eshell-prompt ()
  (let* ((user (user-login-name))
         (host (system-name))
         (path (abbreviate-file-name (eshell/pwd))))
    (concat "[" user "@" host " " path "] λ ")))

(setq eshell-prompt-function 'my-eshell-prompt)
(setq eshell-prompt-regexp "^\\[.* λ ")

(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (eshell/alias "pwninit" (concat "pwninit --template-path=" (getenv "HOME") "/.config/pwninit_template.py"))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(vundo consult-flycheck hl-column magit-delta eglot-booster treesit-auto consult ivy-rich eshell-syntax-highlighting corfu eat beacon undo-tree htmlize ox-reveal org-reveal solarized-theme rg hungry-delete multi-vterm projectile project-x ivy-xref sly geiser-guile fireplace snow org-download flycheck elcord sudo-edit rainbow-delimiters rainbow-delimiters-mode rainbow-mode which-key vterm highlight-indent-guides highlight-indentation vline use-package rustic magit gruvbox-theme gcmh eglot counsel company avy))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :underline nil))))
 '(org-block-end-line ((t (:inherit org-meta-line :extend t :overline nil)))))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
