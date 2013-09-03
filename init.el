(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/vendor/color-themes")
 '(mac-command-modifier nil)
 '(mac-option-modifier (quote meta))
 '(send-mail-function (quote mailclient-send-it))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make emacs look and behave like a modern text editor
(set-default 'cursor-type '(bar . 1))
(fringe-mode '(4 . 0))
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; User details
(setq user-full-name "David Lineberger"
	  user-mail-address "dlineberger@gmail.com")

(setq inhibit-splash-screen t
      initial-scratch-message nil
      make-backup-files nil
      frame-title-format '(buffer-file-name "%f" ("%b")))

;; y or n is good enough
(defalias 'yes-or-no-p 'y-or-n-p)


(set-default-font "-*-Consolas-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; Add vendor directories to load-path
(defvar vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path vendor-dir)

(dolist (project (directory-files vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


;; Highlight matching parentheses
(show-paren-mode t)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Some key bindings
(global-set-key (kbd "<f12>") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'cleanup-buffer)


;; Turn on ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t
	  ido-use-virtual-buffers t)

(require 'powerline)
(powerline-default-theme)

;; Set color theme
(load-theme 'base16-ocean)
