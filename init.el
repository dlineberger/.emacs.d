(require 'cl)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Initialize package manager
(load "package")
(package-initialize)
(add-to-list 'package-archives
			 '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install packages I use in case they're not present
(defvar my-packages '(web-mode
					  ace-jump-mode
					  grizzl
					  flycheck
					  helm
					  projectile
					  helm-projectile
					  markdown-mode
					  twittering-mode
					  js2-mode
					  jabber
					  magit
					  smex
					  sass-mode
					  yasnippet
					  rainbow-mode
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
(set-default-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-default 'cursor-type '(bar . 1))
(fringe-mode '(8 . 0))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode t)
(setq linum-format " %4d ")
(global-visual-line-mode t)
(electric-indent-mode t)
(electric-pair-mode t)

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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


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
  (delete-trailing-whitespace)
  (tabify-buffer)
  (indent-buffer))

(require 'ace-jump-mode)
(require 'uniquify)

;; Some key bindings
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c f") 'reveal-in-finder)
(global-set-key (kbd "C-h d") 'dash-at-point)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-x") 'smex)

;; Turn on ido-mode
(ido-mode t)

;; Enable Projectile
(projectile-global-mode)

;; Enable snippets
(yas-global-mode)

;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Web Mode is awesome
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
;(setq web-mode-engines-alist			
;	  '(("angular" . "\\.html\\'")))

(add-hook 'prog-mode-hook (lambda()
							(linum-mode)
							(rainbow-mode)
							(subword-mode)))

(add-hook 'css-mode-hook 'linum-mode)

;; save backup files to temporary directory
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(setq twittering-status-format "%i %S @%s\n%FILL[ ]{%T}\n %FACE[glyphless-char]{%@ from %f%L%r%R}\n\n")

;; prevent lockfile creation (those nasty .# files that screw up Grunt)
(setq create-lockfiles nil)

;; Allow ANSI characters in compilation buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Set color theme
(when window-system
  (load-theme 'base16-ocean))
