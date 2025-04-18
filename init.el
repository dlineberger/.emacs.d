;;; init.el --- Emacs Startup Configuration

;; Author: David Lineberger

;;; Commentary:
;;; Code:

(setq use-package-compute-statistics t)

;; Standard Emacs settings
(use-package emacs
  :custom
  (inhibit-startup-message t)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (use-short-answers t)
  (custom-safe-themes t)
  (tooltip-mode -1)           ; Disable tooltips
  (delete-selection-mode 1)   ; Act like a normal editor
  (savehist-mode 1)
  (lock-file-mode -1)
  (mouse-wheel-mode nil)
  (grep-command "rg"))

;; For opening huge files
(use-package so-long
  :ensure nil
  :init
  (global-so-long-mode 1))

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)        ; Disable visible scrollbar
      (tool-bar-mode -1)          ; Disable the toolbar
      (menu-bar-mode 1)
      (pixel-scroll-precision-mode 1)
      (set-fringe-mode '(10 . 0)) ; Give some breathing room
      (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'thin)
      (set-face-attribute 'mode-line nil :font "SF Pro")
      (set-face-attribute 'minibuffer-prompt nil :font "SF Pro")
            (load-theme 'ef-owl t)
      )
  (progn
    (menu-bar-mode -1)))

;; Workaround a tramp-MacOS bug that dramatically slows completion
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(use-package devdocs
  :ensure t
  :custom ((devdocs-site-url "http://localhost:9292")
            (devdocs-cdn-url "http://localhost:9292/docs"))
  :bind (("C-h D" . devdocs-lookup)))

(use-package spacious-padding
  :custom ((spacious-padding-subtle-mode-line t))
  :init
  (spacious-padding-mode 1))


(use-package esh-mode
  :ensure nil
  :config
  (defun my/shell-clear ()
    (interactive)
    (eshell/clear 1)
    (eshell-emit-prompt))
  :bind ((:map eshell-mode-map
               ("s-k" . my/shell-clear))))

(use-package eshell
  :ensure nil
  :commands eshell
  :custom
  ((eshell-scroll-to-bottom-on-input 'this)))

(use-package eat
  :ensure t
  :hook (eshell-mode . eat-eshell-mode))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package diminish
  :ensure t)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode))

(use-package which-key
  :init (which-key-mode)
  :ensure t)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode))

(use-package electric-pair
  :ensure nil
  :hook (prog-mode))

(use-package subword
  :ensure nil
  :hook (prog-mode))

(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure))

(use-package eww
  :ensure nil
  :commands eww
  :custom ((eww-auto-rename-buffer "title")))

(use-package dired
  :commands dired
  :custom
  (insert-directory-program "/opt/homebrew/bin/gls")
  (dired-use-ls-dired t)
  (dired-listing-switches "-alh --group-directories-first --time-style=long-iso"))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :hook (prog-mode))


(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (ts-mode . typescript-ts-mode)
              (tsx-mode . tsx-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package consult
  :ensure t
  :bind (
          ("C-x b" . consult-buffer)))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :bind (:map flycheck-mode-map
;;               ("M-n" . flycheck-next-error) ; optional but recommended error navigation
;;               ("M-p" . flycheck-previous-error)))

(use-package re-builder
  :commands re-builder
  :custom
  (reb-re-syntax 'string))

;; VTerm for fast terminal sessions
(use-package vterm
  :ensure t
  :commands vterm
  :bind (:map vterm-mode-map
	      ("s-k" . vterm-clear))
  :config
  (setq vterm-clear-scrollback-when-clearing t)
  (setq vterm-max-scrollback 10000))

;; Compilation Mode Setup
(use-package compile
  :init
  (setq ansi-color-for-compilation-mode t)
  :ensure nil
  :config (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;; Compilation Settings
(defconst typescript-tsc-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\),\\([0-9]+\\)):[[:blank:]]+"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;; handle pretty compiler-errors like the following when doing M-x compile<ret>tsc<ret>
;; test.ts:2:7 - error TS2322: Type '2' is not assignable to type 'string'.
(defconst typescript-tsc-pretty-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\):\\([0-9]+\\):\\([0-9]+\\) - [[:blank:]]*"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;; (dolist
;;     (regexp
;;      `((typescript-tsc
;;         ,typescript-tsc-error-regexp
;;         1 2 3 2)

;;        (typescript-tsc-pretty
;;         ,typescript-tsc-pretty-error-regexp
;;         1 2 3 2)))
;;   (add-to-list 'compilation-error-regexp-alist-alist regexp)
;;   (add-to-list 'compilation-error-regexp-alist (car regexp)))

;;Project management
(use-package project
  :config
  (defun my/project-vterm ()
  "Start vterm in the current project's root directory.
If a buffer already exists for running vterm in the project's root,
switch to it. Otherwise, create a new vterm buffer.
With \\[universal-argument] prefix arg, create a new vterm buffer even
if one already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
	 (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
	 (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
	(pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm))))
  :init
  (setq project-switch-commands 'project-magit-status)
  :bind-keymap
  ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
          ("h" . project-find-file)
          ("t" . my/project-vterm)))

;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode +1)
;;   (setq projectile-project-search-path '("~/indeed/"))
;;   (setq projectile-switch-project-action 'project-magit-status)
;;   :bind (:map projectile-mode-map
;;               ("s-p" . projectile-command-map)
;;               ("C-c p" . projectile-command-map)))

(use-package shell
  :ensure nil
  :config
  (setq shell-kill-buffer-on-exit t)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  :bind (:map shell-mode-map
	      ("s-k" . comint-clear-buffer)
	      ("<up>" . comint-previous-input)
	      ("<down>" . comint-next-input)))


(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

;; Buffer Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

;; Git Interface
(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :config
  (setq magit-process-finish-apply-ansi-colors t)
  (setq magit-save-repository-buffers 'dontask)
  :bind (("C-c m" . magit-status)))

;; Note: Make sure to install prettier via npm
(use-package prettier-js
  :ensure t
  :hook (typescript-ts-base-mode . prettier-js-mode))

(use-package org
  :ensure t
  :init
  (setq org-directory "~/Sync/org")
  (setq org-agenda-files '("~/Sync/org/indeed/journal/current.org"))
  (setq org-default-notes-file (concat org-directory "/indeed/journal/current.org"))
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")))
  (setq org-link-abbrev-alist
        '(("jira" . "https://bugs.indeed.com/browse/%s")))
  (setq org-startup-indented t)
  (setq org-html-keep-old-src t)
  (setq org-export-with-toc t)
  (setq org-hide-emphasis-markers t)
  (setq org-html-preamble nil)
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers t)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))) 
  :bind
  (("C-c t" . find-default-notes-file)
   ("C-c a" . org-agenda)))

(use-package visual-line
  :ensure nil
  :hook (org-mode))

(use-package org-capture
  :init
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline org-default-notes-file "To Refile")
           "* TODO %?" :prepend t)
	  ("s" "Standup Status" entry
           (file+datetree org-default-notes-file "Standup Statuses")
	   (file "templates/status")
	   :prepend t)))
  :bind
  ("C-c c" . org-capture))

;; org-tempo provides src templates
(use-package org-tempo
  :hook org-mode)

;; Markdown support for org export
(use-package ox-md
  :after org-mode)

;; GitHub-flavored Markdown suppport for org export
;; (use-package ox-gfm
;;   :after org-mode)

(defun find-default-notes-file ()
  (interactive)
  (find-file org-default-notes-file))

(use-package multiple-cursors
  :ensure  t
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
          ))


(use-package elfeed
  :ensure t
  :commands elfeed
  :custom
  (elfeed-feeds
    '(
       "http://baha-news.blogspot.com/feeds/posts/default"
       "http://brandonsanderson.com/feed/"
       ("http://daringfireball.net/index.xml" apple)
       ("http://eastbaydish.com/?feed=rss2" bay-area)
       ("http://emacsredux.com/atom.xml" emacs)
       ("http://endlessparentheses.com/atom.xml" emacs)
       ("http://feeds.feedburner.com/EBNosh" bay-area)
       "http://feeds.feedburner.com/SamHarris"
       "http://feeds.feedburner.com/shawnblanc"
       "http://feeds.feedburner.com/sportsblogs/barkingcarnival"
       "http://hoodline.com/atom"
       ("http://pragmaticemacs.com/feed/" emacs)
       "http://thebuddhistblog.blogspot.com/feeds/posts/default"
       ("http://theshot.coffeeratings.com/feed/" coffee)
       "http://usesthis.com/feed/"
       ("http://www.burntorangenation.com/rss2/index.xml" longhorns)
       "http://www.cerealously.net/feed/"
       "http://www.financialsamurai.com/feed/"
       ("http://www.macrumors.com/macrumors.xml" apple)
       ("http://www.macsparky.com/blog/rss.xml" apple)
       ("http://www.marco.org/rss" apple)
       ("http://xkcd.com/atom.xml" comics)
       ("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" news)
)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("29a073e66535bad18e11e9bcaa17d7f2d17e4c79f01023e59e9841633915c232"
       default))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(doom-nord-brighter-modeline t t)
 '(doom-nord-padded-modeline t t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eshell-prompt-function
    #[0 "\300\301 !\302 \303U\203\17\0\304\202\20\0\305P\207"
       [abbreviate-file-name eshell/pwd user-uid 0 " # " " λ "] 3])
 '(eshell-prompt-regexp "^[^#$\12]* [#$λ] ")
 '(graphviz-dot-preview-extension "png")
 '(ispell-program-name "aspell")
 '(list-matching-lines-default-context-lines 1)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(markdown-css-paths
    '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown.min.css"))
 '(mouse-wheel-progressive-speed nil)
 '(org-preview-html-viewer 'xwidget)
 '(org-safe-remote-resources
    '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages
    '(ace-window add-node-modules-path catppuccin-theme cheat-sh consult
       corfu deft devdocs diminish doom-themes eat editorconfig
       ef-themes eglot elfeed elfeed-web embark envrc esup
       exec-path-from-shell f flycheck forge git-timemachine
       graphviz-dot-mode ht htmlize lv magit marginalia markdown-mode
       mastodon modus-themes multiple-cursors ob-typescript orderless
       org-preview-html ox-gfm prettier-js project project-tab-groups
       projectile restclient solaire-mode spacious-padding spinner
       sqlite sqlite3 ultra-scroll use-package vertico vterm web-mode
       which-key zenburn-theme))
 '(package-vc-selected-packages
    '((ultra-scroll :vc-backend Git :url
        "https://github.com/jdtsmith/ultra-scroll")))
 '(web-mode-comment-formats
    '(("java" . "/*") ("javascript" . "//") ("typescript" . "//")
       ("php" . "/*") ("css" . "/*"))))

(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


(defun drl/markdown-live-preview-window-xwidget-webkit (file)
  "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
  (let ((uri (format "file://%s" file)))
    (xwidget-webkit-browse-url uri)
    xwidget-webkit-last-session-buffer))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :custom ((markdown-live-preview-window-function 'drl/markdown-live-preview-window-xwidget-webkit)))

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil)

;;   ;; Maybe define some palette overrides, such as by using our presets
;;   (setq modus-themes-common-palette-overrides
;; 	'((border-mode-line-active unspecified)
;;           (border-mode-line-inactive unspecified)))

;;   ;; Load the theme of your choice.
;;   (load-theme 'ef-owl))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
(put 'dired-find-alternate-file 'disabled nil)


(defun project-magit-status ()
  "Run VC-Dir in the current project's root."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))


(defun my/log-thing-at-point ()
  "Adds a console.log statement after the current line for the thing at point"
  (interactive)
  (save-excursion
    (let* ((loggable-node (combobulate-node-at-point '("member_expression" "identifier")))
	   (loggable-string (buffer-substring (combobulate-node-start loggable-node) (combobulate-node-end loggable-node))))
      (end-of-line)
	    (newline-and-indent)
	    (insert "console.log(\"" loggable-string "\", " loggable-string ");"))))

(use-package combobulate
   :custom
   ;; You can customize Combobulate's key prefix here.
   ;; Note that you may have to restart Emacs for this to take effect!
   (combobulate-key-prefix "C-c o")
   :hook ((prog-mode . combobulate-mode))
   ;; Amend this to the directory where you keep Combobulate's source
   ;; code.
  :load-path ("vendor/combobulate"))


(load (expand-file-name (concat user-emacs-directory "indeed.el")))
