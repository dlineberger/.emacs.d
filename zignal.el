;; Zignal Labs Coding Standards

;; Spaces, not Tabs
(setq indent-tabs-mode nil)

;; Default to tab width of 2
(setq tab-width 2)

;; CSS
(setq css-indent-offset 2)

;; JavaScript
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-strict-missing-semi-warning nil)
(setq js2-indent-switch-body t)

;; CoffeeScript
(setq coffee-tab-width 2)

;; Web Mode
(setq web-mode-block-padding 0)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-enable-control-block-indentation nil)

;; Whitespace
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs trailing lines-tail))

(setq org-link-abbrev-alist
      '(("jira" . "https://politear.atlassian.net/browse/")))
