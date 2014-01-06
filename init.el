(require 'cl)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/vendor/color-themes")
 '(grep-highlight-matches t)
 '(jabber-account-list (quote (("dlineberger@ringtaildesign.com" (:network-server . "talk.google.com") (:port . 5223) (:connection-type . ssl)))))
 '(jabber-auto-reconnect t)
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#65737e" :foreground "#dfe1e8" :box (:line-width -1 :style released-button) :height 0.85)))))

;; Initialize package manager
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; Install packages I use in case they're not present
(defvar my-packages '(web-mode
					  flycheck
					  helm
					  projectile					  
					  less-css-mode)
  "Default packages")

(defun packages-installed-p ()
  (loop for pkg in my-packages
		when (not (package-installed-p pkg)) do (return nil)
		finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my-packages)
	(when (not (package-installed-p pkg))
	  (package-install pkg))))
					  

;; Make emacs look and behave like a modern text editor
;;(set-default-font "-*-Consolas-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-default 'cursor-type '(bar . 1))
(fringe-mode '(8 . 0))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode t)

;; User details
(setq user-full-name "David Lineberger"
	  user-mail-address "dlineberger@gmail.com")

(setq inhibit-splash-screen t
      initial-scratch-message nil
      make-backup-files nil
      frame-title-format '(buffer-file-name "%f" ("%b")))

;; y or n is good enough
(defalias 'yes-or-no-p 'y-or-n-p)


;; Add vendor directories to load-path
(defvar vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path vendor-dir)

(dolist (project (directory-files vendor-dir t "\\w+"))
  (when (file-directory-p project)
	(message "Loading vendor directory %s" project)
    (add-to-list 'load-path project)))


;; Highlight matching parentheses
(show-paren-mode t)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (untabify-buffer)
  (indent-buffer)
  (delete-trailing-whitespace))

;; Some key bindings
(global-set-key (kbd "<f12>") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Turn on ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t
	  ido-use-virtual-buffers t)

(require 'powerline)
(powerline-default-theme)

;; Enable Projectile
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; Web Mode is awesome
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(setq web-mode-engines-alist
	  '(("angular" . "\\.html\\'")))
(setq web-mode-enable-current-element-highlight t)

;; Set color theme
(load-theme 'base16-ocean)
