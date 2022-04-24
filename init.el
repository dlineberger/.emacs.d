(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode 1)
(setq dired-use-ls-dired nil)
;; Set up the visible bell
;;(setq visible-bell t)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

(load-theme 'tango)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Initialize package sources
(use-package project
  :init
  (setq project-switch-commands 'magit-status)
  :bind-keymap
  ("C-c p" . project-prefix-map))


;; Buffer Completion
(use-package ivy
  :diminish
  :init
  (ivy-mode 1))


;; Project Management
(require 'project)

;; Git Interface
(use-package magit
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :bind (("C-c m" . magit-status)))

(use-package which-key
  :config
  (which-key-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode
)

(use-package lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook ((web-mode . lsp-deferred)))

(use-package lsp-ui
  :commands lsp-ui-mode)


(use-package prettier-js
  :hook (web-mode . prettier-js-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(prettier-js which-key web-mode use-package project magit lsp-ui ivy exec-path-from-shell command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
