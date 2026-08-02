;; -*- lexical-binding: t; -*-
;; Blessed are those who code in Emacs

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(server-start)

(delete-selection-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)
(column-number-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq vc-follow-symlinks t)
(global-display-line-numbers-mode)
(blink-cursor-mode 0)
(electric-pair-mode 1)
(setq mode-require-final-newline nil)
(setq initial-scratch-message nil)

(setq ibuffer-human-readable-size t)
(setq kill-region-dwim 'emacs-word)

(global-set-key (kbd "C-c RET") 'eshell)
(global-set-key [remap list-buffers] 'ibuffer)


(defalias 'yes-or-no-p 'y-or-n-p)
(unless (file-exists-p "~/.emacs.d/custom.el")
  (with-temp-buffer (write-file "~/.emacs.d/custom.el")))
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq sentence-end-double-space nil)
(setq custom-safe-themes t)
(setq whitespace-style '(face trailing tabs newline tab-mark))
(global-whitespace-mode)
(setq enable-recursive-minibuffers t)

(setq eldoc-idle-delay 0.25)
(setq-default display-line-numbers-type 'relative)
(setq scroll-margin 7)
(setq max-mini-window-height 17)
(setq vertico-count 27)
(setq password-cache-expiry nil)
(setq-default project-vc-extra-root-markers '(".project" "Cargo.toml"))

(setq magit-diff-fontify-hunk 'all)
(setq magit-diff-specify-hunk-foreground nil)
(setq magit-diff-use-indicator-faces t)

;; indentation
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-local tab-width 4)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'bsd)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-close 0)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font 11"))
(defvar default-font "JetBrainsMono Nerd Font 11")
(set-frame-font "JetBrainsMono Nerd Font 11" nil t)
(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 110)


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

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell))
(use-package cmake-mode
  :defer t)

(use-package org)
(use-package rg
  :config
  (setq rg-group-result nil)
  (rg-enable-default-bindings)
  (rg-enable-menu))
(use-package which-key
  :config
  (which-key-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark)
  (custom-theme-set-faces
   'user
   '(magit-diff-added             ((t :inherit diff-added   :extend t)))
   '(magit-diff-removed           ((t :inherit diff-removed :extend t)))
   '(magit-diff-context           ((t :inherit diff-context :extend t)))
   '(magit-diff-added-highlight   ((t :inherit diff-refine-added   :extend t)))
   '(magit-diff-removed-highlight ((t :inherit diff-refine-removed :extend t)))
   '(magit-diff-context-highlight ((t :inherit diff-context :extend t)))
   '(magit-diff-hunk-heading      ((t :inherit diff-hunk-header)))
   '(magit-diff-hunk-heading-highlight ((t :inherit diff-hunk-header :weight bold)))
   '(magit-diff-file-heading      ((t :inherit diff-file-header)))
   '(magit-diff-file-heading-highlight ((t :inherit diff-file-header :weight bold)))))

(set-cursor-color "#d33682")
(set-face-attribute 'whitespace-trailing nil
                    :foreground nil
                    :background "#212026")
(custom-set-faces
 '(eglot-highlight-symbol-face ((t (:inherit bold :background "#29422d")))))

(use-package eglot
  :ensure nil
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; (setq eglot-events-buffer-config '(:size 0 :format full))
  ;; (setq eglot-events-buffer-size 0)
  ;; (setq eglot-connect-timeout 999)
  (add-hook 'prog-mode-hook 'eglot-ensure)
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 0)))
  ;; THIS WAY DOESN'T WORK NO MATTER WHAT I TRY IDK WHY PLS HELP
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                `(rustic-mode . ("rust-analyzer" :initializationOptions
  ;;                               (:cargo (:features "all"))))))
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
                    "--header-insertion-decorators=0"))))

(use-package csproj-mode
  :defer t)
(use-package markdown-mode
  :defer t)
(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))


(use-package magit
  :config
  (define-advice magit-push-current-to-upstream (:before (args) query-yes-or-no)
    "Prompt for confirmation before permitting a push to upstream."
    (when-let ((branch (magit-get-current-branch)))
      (unless (yes-or-no-p (format "Push %s branch upstream to %s? "
                                   branch
                                   (or (magit-get-upstream-branch branch)
                                       (magit-get "branch" branch "remote"))))
        (user-error "Push to upstream aborted by user")))))
(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode))
  :config
  (transient-append-suffix 'magit-clone "-s"
    '("-r" "Recurse submodules" "--recurse-submodules"))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode)

  ;; magit-blame-mode-hook doesn't work there, so I came up with this shit
  (defun my/after-magit-blame-process-sentinel (&rest _)
    (when (bound-and-true-p diff-hl-mode)
      (diff-hl-magit-post-refresh)))
  (with-eval-after-load 'magit-blame
    (advice-add 'magit-blame-process-sentinel :after #'my/after-magit-blame-process-sentinel)))

(defun my/compilation-notify (buffer status)
  (call-process "notify-send" nil nil nil
                (buffer-name buffer) (string-trim status))
  (call-process "paplay" nil nil nil
                "/usr/share/sounds/freedesktop/stereo/bell.oga"))
(add-hook 'compilation-finish-functions #'my/compilation-notify)

(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-visible-completions-up-down-map
            ("C-n" . minibuffer-next-completion)
            ("C-p" . minibuffer-previous-completion))
  :custom
  (completion-auto-help t)
  (completion-auto-select 'second-tab)
  (completion-eager-update t)
  (completion-eager-display t)
  (minibuffer-visible-completions 'up-down)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completion-styles '(partial-completion flex))
  (completions-detailed t)
  (completions-max-height 17)
  (completions-sort 'historical)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))
(keymap-set minibuffer-visible-completions-up-down-map "C-n"  (minibuffer-visible-completions--bind #'minibuffer-next-completion))
(keymap-set minibuffer-visible-completions-up-down-map "C-p"  (minibuffer-visible-completions--bind #'minibuffer-previous-completion))

(use-package consult
  :config
  (consult-customize
   consult-buffer :preview-key "M-.")
  (setq consult-async-min-input 2)

  (defun my/consult-fd (&optional include-dotfiles)
    (interactive "P")
    (if include-dotfiles
        (let ((consult-fd-args "fd --full-path --type f -I --hidden --color=never"))
          (consult-fd))
      (consult-fd)))

  (defun my/consult-ripgrep (&optional include-dotfiles)
    (interactive "P")
    (if include-dotfiles
        (let ((consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden"))
          (consult-ripgrep))
      (consult-ripgrep)))

  :bind
  ;; ("C-x b" . consult-buffer)
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

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-ahgo --group-directories-first")))
(setq dired-dwim-target t)

(defun alacritty ()
    (interactive)
    (call-process "alacritty" nil 0 nil "--working-directory" (file-truename default-directory)))

;; (setq treesit-auto-install-grammar 'always)
;; (setq treesit-enabled-modes t)

(use-package esh-mode
  :ensure nil
  :config
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package ghostel
  :ensure t)
(require 'ghostel-eshell)
(add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)
(require 'ghostel-compile)
(global-set-key (kbd "C-c c") #'ghostel-compile)
(ghostel-comint-global-mode 1)

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode +1))

(defun my-eshell-prompt ()
  (let ((user (user-login-name))
        (host (system-name))
        (path (abbreviate-file-name (eshell/pwd))))
    (concat "[" user "@" host " " path "] λδ ")))

(setq eshell-banner-message ""
      eshell-prompt-function 'my-eshell-prompt
      eshell-prompt-regexp "^\\[.* λδ "
      eshell-history-size 999999
      eshell-hist-ignoredups t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-r") 'consult-history)
            (define-key eshell-hist-mode-map (kbd "M-s") nil)))

(put 'dired-find-alternate-file 'disabled nil)
