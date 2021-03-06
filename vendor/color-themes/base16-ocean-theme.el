;; Base16 Ocean (https://github.com/chriskempson/base16)
;; Scheme: Chris Kempson (http://chriskempson.com)

;;; base16-ocean-theme.el

;;; Code:

(deftheme base16-ocean)

(let ((background "#2b303b")
      (not-so-dark-background "#343d46")
      (highlight "#4f5b66")
      (current-line "#65737e")
      (selection "#5D81B1")
      (foreground "#dfe1e8")
      (comment "#65737e")
      (cursor "#dfe1e8")
      (red "#bf616a")
      (orange "#d08770")
      (yellow "#ebcb8b")
      (green "#a3be8c")
      (aqua "#96b5b4")
      (blue "#8fa1b3")
      (purple "#b48ead"))

  (custom-theme-set-faces
   'base16-ocean

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background , background))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,current-line :foreground ,foreground))))
   `(region ((t (:background ,selection))))
   `(highlight ((t (:background, highlight))))
   `(button ((t (:foreground ,yellow :underline, t))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:background ,highlight))))
   `(web-mode-html-tag-face ((t (:foreground ,purple))))
   `(web-mode-html-attr-name-face ((t (:foreground ,blue))))
   `(web-mode-html-attr-equal-face ((t (:foreground ,foreground))))
   `(web-mode-variable-name-face ((t (:foreground ,orange))))
   `(web-mode-preprocessor-face ((t (:foreground ,orange))))

   ;; diff
   `(diff-removed ((t (:foreground, red :background, not-so-dark-background))))
   `(diff-added ((t (:foreground, green :background, not-so-dark-background))))

   ;; ido-mode
   `(ido-subdir ((t (:foreground, red))))
   `(ido-first-match ((t (:foreground, yellow))))
   `(ido-only-match ((t (:foreground, green))))

   ;; Font-lock stuff
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; ansi-term
   `(term-color-black ((t (:foreground ,background))))
   `(term-color-red ((t (:foreground ,red))))
   `(term-color-green ((t (:foreground ,green))))
   `(term-color-yellow ((t (:foreground ,yellow))))
   `(term-color-blue ((t (:foreground ,blue))))
   `(term-color-purple ((t (:foreground ,purple))))
   `(term-color-cyan ((t (:foreground ,blue))))
   `(term-color-white ((t (:foreground ,foreground))))

   ;; jabber
   `(jabber-roster-user-online ((t (:foreground ,green))))
   `(jabber-roster-user-away ((t (:foreground ,selection))))
   `(jabber-roster-user-xa ((t (:foreground ,orange))))
   `(jabber-roster-user-offline ((t (:foreground ,red))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,red))))
   `(jabber-chat-prompt-local ((t (:foreground ,blue))))
   `(jabber-rare-time-face ((t (:foreground ,green))))
	  
   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,not-so-dark-background :foreground ,foreground))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,current-line))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,foreground)))))

  (custom-theme-set-variables
   'base16-ocean

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])

   ))
  
(provide-theme 'base16-ocean)

;;; base16-ocean-theme.el ends here
