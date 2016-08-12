(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-keys (quote (97 111 101 117 105 100 104 116 110 115)))
 '(blink-cursor-mode nil)
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/vendor/color-themes")
 '(diff-hl-draw-borders t)
 '(dired-dwim-target t)
 '(global-auto-revert-mode t)
 '(global-diff-hl-mode t)
 '(global-magit-file-buffer-mode t)
 '(grep-highlight-matches t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(jabber-auto-reconnect t)
 '(line-spacing 2)
 '(magit-diff-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff")))
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(org-agenda-files (quote ("~/Dropbox/Documents/org/agenda")))
 '(org-blank-before-new-entry nil)
 '(org-directory "~/Dropbox/Documents/org")
 '(org-export-with-sub-superscripts (quote {}))
 '(org-export-with-toc 2)
 '(org-html-head
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />")
 '(org-html-infojs-options
   (quote
    ((path . "http://thomasf.github.io/solarized-css/org-info.min.js")
     (view . "info")
     (toc . :with-toc)
     (ftoc . "0")
     (tdepth . "max")
     (sdepth . "max")
     (mouse . "underline")
     (buttons . "0")
     (ltoc . "1")
     (up . :html-link-up)
     (home . :html-link-home))))
 '(org-insert-heading-respect-content t)
 '(org-use-sub-superscripts nil)
 '(powerline-default-separator (quote wave))
 '(powerline-default-separator-dir (quote (right . right)))
 '(powerline-display-hud nil)
 '(powerline-height 24)
 '(powerline-text-scale-factor 2.0)
 '(projectile-enable-caching t)
 '(projectile-global-mode t)
 '(projectile-use-git-grep t)
 '(rainbow-ansi-colors nil)
 '(rainbow-html-colors nil)
 '(rainbow-x-colors nil)
 '(safe-local-variable-values
   (quote
    ((flycheck-javascript-eslint-executable . "./node_modules/.bin/eslint")
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "npm verify" projectile-compilation-cmd-map)))))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(solarized-distinct-fringe-background t)
 '(solarized-high-contrast-mode-line t)
 '(solarized-use-less-bold t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "David Lineberger")
 '(user-mail-address "dlineberger@gmail.com")
 '(visible-bell nil)
 '(web-mode-enable-control-block-indentation nil)
 '(web-mode-enable-current-element-highlight t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "nil" :slant normal :weight normal :height 130 :width normal))))
 '(minibuffer-prompt ((t (:foreground "#0084C8" :weight bold :family "Source Sans Pro"))))
 '(mode-line ((t (:family "Source Sans Pro"))))
 '(mode-line-inactive ((t (:family "Source Sans Pro"))))
 '(web-mode-current-element-highlight-face ((t (:inherit highlight)))))
