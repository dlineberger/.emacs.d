;;; init.el --- Emacs Startup Configuration

;; Author: David Lineberger

;;; Commentary:
;;; Code:

;; Standard Emacs settings
(use-package emacs
  :init
  (setq inhibit-startup-message t)
  (setq make-backup-files nil)
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (tooltip-mode -1)           ; Disable tooltips
  (delete-selection-mode 1)   ; Act like a normal editor
  (savehist-mode 1)
  (lock-file-mode -1))

;; For opening huge files
(use-package so-long
  :ensure nil
  :init
  (global-so-long-mode 1))

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
	    (append initial-frame-alist
		    '((left . 100 )
                      (top . 100)
                      (width . 140)
                      (height . 80))))
      (scroll-bar-mode -1)        ; Disable visible scrollbar
      (tool-bar-mode -1)          ; Disable the toolbar
      (menu-bar-mode 1)
      (pixel-scroll-precision-mode 1)
      (set-fringe-mode '(10 . 0)) ; Give some breathing room
      (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light)
      (set-face-attribute 'mode-line nil :font "SF Pro")
      (set-face-attribute 'minibuffer-prompt nil :font "SF Pro"))
  (progn
    (load-theme 'ef-trio-light t)
    (menu-bar-mode -1)))

;; Workaround a tramp-MacOS bug that dramatically slows completion
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package eshell
  :ensure nil
  :custom
  (setq eshell-scroll-to-bottom-on-input 'this))

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
  :config
  (recentf-mode 1))

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

(use-package eww
  :ensure nil
  :config
  (setq eww-auto-rename-buffer "title"))

(use-package dired
  :config
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq dired-use-ls-dired t)
  (setq dired-listing-switches "-alh --group-directories-first --time-style=long-iso"))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string)
  )

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

;; Project management
(use-package project
  :init
  (setq project-switch-commands 'project-magit-status)
  :bind-keymap
  ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("h" . project-find-file)))

(use-package shell
  :ensure nil
  :config
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  :bind (:map shell-mode-map
	      ("s-k" . comint-clear-buffer)
	      ("<up>" . comint-previous-input)
	      ("<down>" . comint-next-input)))

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
  :bind (("C-c m" . magit-status)))

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
	 (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
            typescript-ts-mode
            markdown-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable nil)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe nil)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-use-plists t))


(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands
;;   (lsp-ui-doc-show
;;    lsp-ui-doc-glance)
;;   :bind (:map lsp-mode-map
;;               ("C-c C-d" . 'lsp-ui-doc-glance))
;;   :after (lsp-mode evil)
;;   :config (setq lsp-ui-doc-enable t
;;                 lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;             lsp-ui-doc-include-signature t       ; Show signature
;;             lsp-ui-sideline-show-diagnostics nil
;;                 lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)
;; (use-package lsp-mode
;;   :init
;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;   (setq gc-cons-threshold 100000000)
;;   (setq lsp-keymap-prefix "C-c l")
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   :hook ((web-mode . lsp-deferred)))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode)


;; Note: Make sure to install prettier via npm
(use-package prettier-js
  :ensure t
  :hook (typescript-ts-base-mode . prettier-js-mode))

(use-package org
  :ensure t
  :init
  (setq org-directory "~/Sync/org")
  (setq org-agenda-files '("~/Sync/org/current.org"))
  (setq org-default-notes-file (concat org-directory "/current.org"))
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")))
  (setq org-link-abbrev-alist
        '(("jira" . "https://bugs.indeed.com/browse/%s")))
  (setq org-startup-indented t)
  (setq org-html-keep-old-src t)
  (setq org-export-with-toc t)
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

(use-package org-tempo)
(use-package ox-md)
(use-package ox-gfm)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("28d91e89883df5dd87c7da27f6a15e8e41bb92a0c1341eaa9f397ed67b10b21d" "6ccb6eb66c70661934a94f395d755a84f3306732271c55d41a501757e4c39fcb" "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8" "aa688776604bbddbaba9e0c0d77e8eb5f88d94308f223d1962b6e6b902add6a0" "df1ed4aa97d838117dbda6b2d84b70af924b0380486c380afb961ded8a41c386" "317754d03bb6d85b5a598480e1bbee211335bbf496d441af4992bbf1e777579e" "30b323c73d2b24c3eff96802f0085fbfef3c6d2aad03ddfb5f615b658b2d5bfa" "ccb2ff53e9794d059ff941fabcf265b67c8418da664db8c4d6a3d656962b7135" "14ba61945401e42d91bb8eef15ab6a03a96ff323dd150694ab8eb3bb86c0c580" "ee1670225ebb4abfaef8a0fe1d4224e14fb2e94d05cbae544ac7cfb433e3ff85" "0b96409bc39262906837afe75155e94bc84819cead6e66a778fdd3833cee7435" "1a5bf8692b9aaa73a9a29bf8895546cfc06c0e064e4306f7f78a8f8437502322" "8ad210f0892474f68a5f152bbb7b514667e6f7edddcc52d2f7a3be41ea1edab9" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "2627707bc15dd427ef165fc8ff9868e3e184f6a151f139c092561bbc39734364" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(doom-nord-brighter-modeline t t)
 '(doom-nord-padded-modeline t t)
 '(elfeed-feeds
    '("http://www.reddit.com/.rss" "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" "http://www.austinist.com/index.rdf" "http://brandonsanderson.com/feed/" "http://www.cerealously.net/feed/" "http://xkcd.com/atom.xml" "http://blog.roku.com/feed/" "http://theshot.coffeeratings.com/feed/" "http://feeds.feedburner.com/SamHarris" "http://usesthis.com/feed/" "http://www.marco.org/rss" "http://www.macsparky.com/blog/rss.xml" "http://feeds.feedburner.com/shawnblanc" "http://daringfireball.net/index.xml" "http://www.macrumors.com/macrumors.xml" "http://www.burntorangenation.com/rss2/index.xml" "http://feeds.feedburner.com/sportsblogs/barkingcarnival" "http://thebuddhistblog.blogspot.com/feeds/posts/default" "http://www.financialsamurai.com/feed/" "http://endlessparentheses.com/atom.xml" "http://emacsredux.com/atom.xml" "http://pragmaticemacs.com/feed/" "http://baha-news.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/EBNosh" "http://eastbaydish.com/?feed=rss2" "http://hoodline.com/atom"))
 '(eshell-prompt-function
    #[0 "\300\301 !\302 \303U\203\17\0\304\202\20\0\305P\207"
       [abbreviate-file-name eshell/pwd user-uid 0 " # " " λ "]
       3])
 '(eshell-prompt-regexp "^[^#$\12]* [#$λ] ")
 '(markdown-css-paths
    '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown.min.css"))
 '(org-preview-html-viewer 'xwidget)
 '(org-safe-remote-resources
    '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages
    '(envrc elfeed-web devdocs eat cheat-sh ox-gfm consult htmlize ob-typescript diminish add-node-modules-path editorconfig flycheck elfeed corfu restclient project ef-themes deft mastodon sqlite3 sqlite marginalia forge modus-themes org-preview-html project-tab-groups which-key catppuccin-theme doom-themes vterm git-timemachine web-mode vertico use-package spinner prettier-js orderless multiple-cursors markdown-mode magit lv ht f exec-path-from-shell))
 '(web-mode-comment-formats
    '(("java" . "/*")
       ("javascript" . "//")
       ("typescript" . "//")
       ("php" . "/*")
       ("css" . "/*"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defun drl/project-vterm ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (pop-to-buffer-same-window shell-buffer)
      (vterm (generate-new-buffer-name default-project-shell-name)))))


(defun drl/markdown-live-preview-window-xwidget-webkit (file)
  "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
  (let ((uri (format "file://%s" file)))
    (xwidget-webkit-browse-url uri)
    xwidget-webkit-last-session-buffer))


(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-live-preview-window-function
	'drl/markdown-live-preview-window-xwidget-webkit)
  )

(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  ;; Load the theme of your choice.
  (load-theme 'ef-maris-light))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
(put 'dired-find-alternate-file 'disabled nil)


(defun project-magit-status ()
  "Run VC-Dir in the current project's root."
  (interactive)
  (magit-status (project-root (project-current t))))


(eval-after-load 'typescript-ts-mode
  '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
