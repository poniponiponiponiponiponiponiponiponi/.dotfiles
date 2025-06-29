(global-auto-revert-mode 1)
(column-number-mode 1)
(global-subword-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq vc-follow-symlinks t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(blink-cursor-mode 0)
(electric-pair-mode 1)

(load-theme 'tango-dark)

(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-<return>") 'eshell)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-v") (lambda () (interactive) (scroll-up-line 14)))
(global-set-key (kbd "M-v") (lambda () (interactive) (scroll-down-line 14)))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-revert-verbose nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq kill-buffer-query-functions nil)
(setq custom-safe-themes t)

(setq eldoc-idle-delay 0.25)
(setq-default display-line-numbers-type 'relative)
(setq scroll-margin 2)
(setq max-mini-window-height 11)
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

(require 'eglot)
(setq eglot-ignored-server-capabilities
      '(:documentOnTypeFormattingProvider))

(ido-mode)
(ido-everywhere)
