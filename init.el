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

;; (setq debug-on-error t)
(server-start)

(global-auto-revert-mode 1)
(column-number-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq vc-follow-symlinks t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(blink-cursor-mode 0)
(electric-pair-mode 1)

;; ;; 1) Disable Flymake’s built‑in triggers
;; (with-eval-after-load 'flymake
;;   ;; Never run at startup, idle, or on save automatically
;;   (setq flymake-start-on-flymake-mode nil
;;         flymake-no-changes-timeout nil
;;         flymake-start-on-save-buffer nil))

;; ;; 2) Define overlay‑clearing function
;; (defun my/flymake-clear-overlays (_beg _end _len)
;;   "Delete *all* Flymake diagnostic overlays in the current buffer."
;;   (dolist (ov (overlays-in (point-min) (point-max)))
;;     (when (overlay-get ov 'flymake-diagnostic)
;;       (delete-overlay ov))))

;; ;; 3) On entering Flymake mode, yank out *every* Flymake hook/timer,
;; ;;    then add just our overlay‑clear on-change hook
;; (defun my/flymake-disable-idle-and-hooks ()
;;   ;; Remove the legacy and new on-change functions
;;   (remove-hook 'after-change-functions #'flymake-after-change-function t)
;;   (when (fboundp 'flymake--on-change)
;;     (remove-hook 'after-change-functions #'flymake--on-change t))
;;   ;; Cancel any idle timer Flymake scheduled
;;   (when (fboundp 'cancel-function-timers)
;;     (cancel-function-timers #'flymake--post-self-change))
;;   ;; Now add our own clear‑on‑type
;;   (add-hook 'after-change-functions
;;             #'my/flymake-clear-overlays nil t))

;; (add-hook 'flymake-mode-hook #'my/flymake-disable-idle-and-hooks)

;; ;; 4) Only on save: clear old overlays *and* do a fresh Flymake pass
;; (defun my/flymake-save-and-run ()
;;   "Clear stale Flymake overlays and then start a new check."
;;   (when (bound-and-true-p flymake-mode)
;;     ;; immediately wipe old overlays
;;     (my/flymake-clear-overlays nil nil nil)
;;     ;; start Flymake now that the file is saved
;;     ;; (flymake-start)
;;     (run-with-timer
;;      0.3               ;; delay in seconds
;;      nil               ;; repeat interval (nil = run just once)
;;      (lambda ()
;;        ;; your code here
;;        (flymake-start)))))

;; (add-hook 'after-save-hook #'my/flymake-save-and-run)





(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-<return>") 'eshell)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-v") (lambda () (interactive) (scroll-up-line 14)))
(global-set-key (kbd "M-v") (lambda () (interactive) (scroll-down-line 14)))


(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)
(setq auto-revert-verbose nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq kill-buffer-query-functions nil)
(setq custom-safe-themes t)
(setq whitespace-style '(face trailing tabs newline tab-mark))
(global-whitespace-mode)

;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)


(setq eldoc-idle-delay 0.25)
(setq-default display-line-numbers-type 'relative)
(setq scroll-margin 2)
(setq max-mini-window-height 11)
(setq vertico-count 20)
(setq-default project-vc-extra-root-markers '(".project" "Cargo.toml"))

;; indentation
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-local tab-width 4)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'bsd)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(if (> (x-display-pixel-width) 1920)
    (progn
      (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font 18"))
      (defvar default-font "JetBrainsMono Nerd Font 18")
      (set-frame-font "JetBrainsMono Nerd Font 18" nil t)
      (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 180))
  (progn
    (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font 11"))
    (defvar default-font "JetBrainsMono Nerd Font 11")
    (set-frame-font "JetBrainsMono Nerd Font 11" nil t)
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 110)))


(defun pwn-info-variable (str)
  "Insert a string into the current buffer."
  (interactive "sVariable: ")
  (insert "info(f'{" str " = :#x}')"))
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

(setq gdb-many-windows t
      gdb-use-separate-io-buffer t)
(advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell))

(use-package yasnippet)

(use-package rg)
(use-package which-key
  :config
  (which-key-mode))
(use-package avy
  :bind
  ("M-i" . avy-goto-char))

(use-package sudo-edit)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(use-package elcord
  :config
  (elcord-mode))

(use-package indent-bars
  :config
  (require 'indent-bars-ts)
  ;; :custom
  ;; (indent-bars-treesit-support t)
  ;; (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook ((prog-mode . (lambda ()
                        (if (derived-mode-p 'prog-mode)
                            (unless (derived-mode-p 'emacs-lisp-mode)
                              (indent-bars-mode)))))))
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox-dark-medium)
;;   (set-cursor-color "#d33682"))

(setq solarized-distinct-fringe-background t)
(use-package solarized-theme)
;; (load-theme 'solarized-dark t)
(load-theme 'spacemacs-dark)
(set-cursor-color "#d33682")
(set-face-attribute 'whitespace-trailing nil
                    :foreground nil
                    :background "#212026")

(set-face-attribute 'mode-line nil
                    :box '(:line-width (5 . 1) :color "#5d4d7a" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :box '(:line-width (5 . 1) :color "#5d4d7a" :style nil))


(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; (global-tree-sitter-mode)
;;(setq major-mode-remap-defaults t)
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 0)))
;; (fset #'jsonrpc--log-event #'ignore)

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(c-mode . ("ccls")))
;;   (add-to-list 'eglot-server-programs '(c++-mode . ("ccls"))))
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
(require 'eglot)
(setq eglot-ignored-server-capabilities
      '(:documentOnTypeFormattingProvider))
(add-to-list 'eglot-server-programs
                 '((c-ts-mode c++-ts-mode c-mode c++-mode)
                   . ("clangd"
                      "-j=8"
                      "--log=error"
                      "--malloc-trim"
                      "--background-index"
                      "--clang-tidy"
                      "--pch-storage=memory"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0")))

;; keep asm programming sane
(defun my-asm-newline-and-indent ()
  (interactive)
  (newline)
  (indent-according-to-mode))
(add-hook 'asm-mode-hook
          (lambda ()
            (setq-local comment-start "#")
            (setq-local comment-add 0)
            (setq-local comment-end "")
            (setq-local comment-start-skip "#+\\s-*")
            (electric-indent-local-mode -1)
            (local-set-key (kbd "RET") #'my-asm-newline-and-indent)))
(advice-add #'asm-comment :override #'self-insert-command)
(setq asm-comment-char ?\#)

;; Keep the python indent inside string sane.
;; It is not perfect but good enough.
(defun my-python-string-indent (orig-fun &rest args)
  (let ((context (python-indent-context)))
    (if (or (eq (car context) :inside-string)
            (eq (car context) :inside-docstring))
        (indent-relative)
      (apply orig-fun args))))
(advice-add 'python-indent-line :around #'my-python-string-indent)

(use-package rust-mode)
(use-package markdown-mode)
(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))

(use-package htmlize)

(use-package magit)
(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

(use-package corfu
  ;; :custom
  ;; (corfu-auto 1)
  :init
  (global-corfu-mode))

(use-package cape
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible))

(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)
(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
(use-package vertico
  :config
  (setq vertico-resize nil)
  (vertico-mode 1)
  (keymap-global-set "<f10>" #'tmm-menubar)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))
(use-package marginalia
  :config
  (marginalia-mode 1))
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))
(use-package consult
  :config
  (consult-customize
   consult-buffer :preview-key "M-.")
  (setq consult-async-min-input 2)

  (defun my/consult-fd (&optional include-dotfiles)
    (interactive "P")
    (if include-dotfiles
        (let ((consult-fd-args "fd --full-path --type f --hidden --color=never"))
          (consult-fd))
      (consult-fd)))

  (defun my/consult-ripgrep (&optional include-dotfiles)
    (interactive "P")
    (if include-dotfiles
        (let ((consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden"))
          (consult-ripgrep))
      (consult-ripgrep)))

  :bind
  ("C-x b" . consult-buffer)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-s d" . my/consult-fd)
  ("M-s r" . my/consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ("M-g f" . consult-flycheck)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark))

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
(use-package org-download)

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-ahgo --group-directories-first")))

(use-package eat
  :config
  (eat-eshell-mode)
  (add-hook 'eat-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))
  (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0))))

(defun split-eat (str)
  (interactive "sExecute: ")
  (split-window-right)
  (other-window 1)
  (eshell t)
  (eshell-return-to-prompt)
  (insert str)
  (insert " ; { (progn (kill-this-buffer) (delete-window)) }")
  (eshell-send-input))


(defun alacritty ()
    (interactive)
    (call-process "alacritty" nil 0 nil "--working-directory" (file-truename default-directory)))

(use-package esh-mode
  :ensure nil)
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
    (concat "[" user "@" host " " path "] λδ ")))

(setq eshell-banner-message "")
(setq eshell-prompt-function 'my-eshell-prompt)
(setq eshell-prompt-regexp "^\\[.* λδ ")

(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (eshell/alias "pwninit" (concat
                                     "pwninit --template-path="
                                     (getenv "HOME")
                                     "/.config/pwninit_template.py"))
            (eshell/alias "py" "python")))

(define-key eshell-mode-map (kbd "C-r") 'consult-history)
(setq eshell-save-history-on-exit t)
(setq eshell-history-size 999999)
(setq eshell-hist-ignoredups t)

;; (use-package dtrt-indent
;;   :config
;;   (dtrt-indent-global-mode))
