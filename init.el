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
(defvar my-packages '(ace-jump-mode
                      zenburn-theme
                      flycheck
                      helm
                      helm-projectile
                      js2-mode
                      less-css-mode
                      magit
                      markdown-mode
                      org
                      page-break-lines
                      projectile
                      rainbow-mode
                      scss-mode
                      smex
                      web-mode
                      coffee-mode
                      yasnippet)
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



;; Set default font to Monaco 10.
;; This looks best when disabling anti-aliasing by running the following command:
;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 10
(set-default-font "-*-Monaco-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
(add-to-list 'face-ignored-fonts "\\`-[^-]*-monaco-bold-")

;; Make emacs look and behave like a modern text editor
(fringe-mode '(nil . 0)) ;; Only show fringe on left
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq auto-window-vscroll nil)
(delete-selection-mode t)
(setq linum-format " %4d ")
(global-visual-line-mode t)
(global-hl-line-mode t)
(electric-indent-mode t)
(electric-pair-mode t)
(blink-cursor-mode 0)

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
  (untabify-buffer)
  (indent-buffer))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(require 'ace-jump-mode)
(require 'uniquify)

;; Some key bindings
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c f") 'reveal-in-finder)
(global-set-key (kbd "C-h d") 'dash-at-point)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-x") 'smex)
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

;; Turn on ido-mode
(ido-mode t)

;; Enable Projectile
(projectile-global-mode)

;; Pretty-print ^L characters
(global-page-break-lines-mode t)

;; Enable snippets
(yas-global-mode)

;; Show Git diff in fringe
(global-diff-hl-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Web Mode is awesome
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

(add-hook 'prog-mode-hook (lambda()
                            (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace))
                            (rainbow-mode)
                            (subword-mode)))

(add-hook 'css-mode-hook (lambda()
                           (rainbow-mode)))

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

;; Javascript REPL
;; (require 'js-comint)
;; (setq inferior-js-program-command "node")
;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list
;;          'comint-preoutput-filter-functions
;;          (lambda (output)
;;            (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))


;; Set color theme
(load-theme 'zenburn)

;; Zignal Labs Section
(setq org-link-abbrev-alist
      '(("jira" . "https://politear.atlassian.net/browse/")))
