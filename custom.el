(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/vendor/color-themes")
 '(global-auto-revert-mode t)
 '(grep-command "ggrep")
 '(grep-highlight-matches t)
 '(helm-locate-command "mdfind %s %s")
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(jabber-auto-reconnect t)
 '(js2-basic-offset 2)
 '(line-spacing 2)
 '(locate-command "mdfind")
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(org-agenda-files (quote ("~/Dropbox/Documents/org/agenda")))
 '(org-blank-before-new-entry nil)
 '(org-directory "~/Dropbox/Documents/org")
 '(org-insert-heading-respect-content t)
 '(rainbow-ansi-colors nil)
 '(rainbow-html-colors nil)
 '(rainbow-x-colors nil)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "npm verify" projectile-compilation-cmd-map)))))
 '(scss-compile-at-save t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "David Lineberger")
 '(user-mail-address "dlineberger@gmail.com")
 '(visible-bell nil)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 4)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-current-element-highlight-face ((t (:inherit highlight)))))
