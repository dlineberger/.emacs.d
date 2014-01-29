(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/vendor/color-themes")
 '(global-auto-revert-mode t)
 '(grep-highlight-matches t)
 '(ido-enable-flex-matching t)
 '(ido-use-virtual-buffers t)
 '(jabber-account-list (quote (("dlineberger@ringtaildesign.com" (:network-server . "talk.google.com") (:port . 5223) (:connection-type . ssl)))))
 '(jabber-auto-reconnect t)
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(mu4e-confirm-quit nil)
 '(mu4e-headers-skip-duplicates t)
 '(mu4e-html2text-command "w3m -dump -T text/html")
 '(mu4e-maildir "/Users/davidlineberger/.mail/dlineberger-gmail.com")
 '(mu4e-view-prefer-html nil)
 '(mu4e-view-show-images t)
 '(org-agenda-files (quote ("~/Dropbox/Documents/org/agenda")))
 '(org-directory "~/Dropbox/Documents/org")
 '(projectile-completion-system (quote grizzl))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(solarized-distinct-fringe-background t)
 '(solarized-high-contrast-mode-line t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "David Lineberger")
 '(user-mail-address "dlineberger@gmail.com")
 '(visible-bell t)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 4)
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-current-element-highlight-face ((t (:inherit highlight)))))
