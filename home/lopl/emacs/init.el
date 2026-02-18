;;; -*- lexical-binding: t; -*-
;;; lopl's chaothic init.el

;; Optimization & Startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 300000000
                  gc-cons-percentage 0.1)))

;; Variables
(defvar efs/default-font-size 135)
(defvar efs/default-variable-font-size 135)

;; Use Stylix color scheme if on NixOs
(require 'base16-theme)
(load-theme 'base16-stylix t)

;; Basic UI
(setq inhibit-startup-message t
      visible-bell nil
      initial-scratch-message ""
      disabled-command-function nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-auto-revert-mode t)

;; Fonts
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height efs/default-variable-font-size :weight 'regular)

;; Icons
(use-package all-the-icons)

;; Modeline
(use-package doom-modeline
  :custom (doom-modeline-height 15))

;; Visual Fill
(use-package visual-fill-column
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

;; Dashboard
(use-package page-break-lines)

(set-face-attribute 'link nil :underline nil)
(set-face-attribute 'link-visited nil :underline nil)

(use-package dashboard
  :ensure t    
  :hook (dashboard-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (require 'projectile) 
  (setq initial-buffer-choice t)
  (dashboard-setup-startup-hook) 
  (setq dashboard-page-separator "\n\f\n"
        initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))
        dashboard-banner-logo-title "Welcome to Emacs Dashboard"
        dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        dashboard-items '((recents   . 5)
                          (bookmarks . 10)
                          (projects  . 5))
        dashboard-display-icons-p t
        dashboard-vertically-center-content t
        dashboard-navigation-cycle t
        dashboard-icon-type 'all-the-icons
        dashboard-set-heading-icons nil
        dashboard-set-file-icons t))

;; Global Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; General Editing
(electric-pair-mode 1)
(show-paren-mode 1)
(setq-default tab-width 4)
(global-hl-line-mode +1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1))

(use-package command-log-mode)
(use-package hydra)
(use-package general)

;; Nix direnv integration
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Helm
(use-package helm
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-c b" . helm-bookmarks)
   ("C-c f" . helm-recentf)
   ("C-c g" . helm-grep-do-git-grep)))

(use-package helm-tramp)
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :custom (projectile-completion-system 'helm)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom (dired-listing-switches "-agho --group-directories-first"))

(use-package dired-single)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package ibuffer-project
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-project-mode)
                          (ibuffer-do-sort-by-project-file-relative))))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>"))
  (global-ligature-mode 't))

;; LSP
(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (java-mode . lsp-deferred)
   (latex-mode . lsp-deferred)
   (gdscript-mode . lsp-deferred)
   (css-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (csharp-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (python-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        read-process-output-max (* 1024 1024)
        lsp-completion-provider :capf
        lsp-idle-delay 0.500)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package company)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '()))

(use-package yasnippet-snippets)

(use-package flycheck)

(use-package dap-mode
  :after (lsp-mode)
  :hook (dap-mode . dap-ui-mode)
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :config
  (require 'dap-java)
  (require 'dap-lldb)
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  
  ;; Python Debugging
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python")
  (dap-python-setup))

(use-package lsp-ui
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
              lsp-ui-doc-position 'bottom
              lsp-ui-doc-max-width 100))

(use-package helm-lsp
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package helm-swoop
  :bind (("C-s" . helm-swoop)
         ("M-s o" . helm-swoop)
         ("C-c s s" . helm-swoop)))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package treemacs
  :commands (treemacs)
  :bind (("M-<tab>" . treemacs))
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1))))

(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-all-the-icons :after (treemacs))
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1)
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

;; Java
(use-package lsp-java
  :hook (java-mode . lsp))

;; Web
(use-package impatient-mode)
(use-package web-mode
  :hook (web-mode . impatient-mode))

;; Rust
(use-package rustic
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package cargo-mode)
(use-package cargo-transient)

;; Godot
(use-package gdscript-mode)

;; C/C++
(use-package ccls)

;; Python
(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-multi-root nil)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; Black Formatter
(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode))

;; AMPL 
(use-package ampl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.mod$" . ampl-mode))
  (add-to-list 'auto-mode-alist '("\\.dat$" . ampl-mode))
  (add-to-list 'auto-mode-alist '("\\.run$" . ampl-mode))
  (add-to-list 'interpreter-mode-alist '("ampl" . ampl-mode)))

;; QuakeC
(use-package quakec-mode
  :hook (quakec-mode . (lambda ()
                         (setq-local indent-tabs-mode t
                                     tab-width 4)
                         (quakec-setup-flymake-fteqcc-backend)
                         (flymake-mode 1))))

;; Org
(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      (visual-line-mode 1)
                      (visual-fill-column-mode 1)
                      (variable-pitch-mode 1)
                      (org-bullets-mode 1)
                      (display-line-numbers-mode -1)))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c t" . org-todo)
         ("C-c d" . org-deadline)
         ("C-c s" . org-schedule))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)
  (require 'org-faces)
  (require 'org-indent)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Noto sans" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-document-title nil :inherit 'variable-pitch :weight 'bold :height 1.3)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-bullets
  :after org
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package nix-mode
  :mode "\\.nix\\'")

;; Terminal
(use-package eterm-256color
  :hook ((term-mode . eterm-256color-mode)
         (term-mode . (lambda () (display-line-numbers-mode -1)))))

(use-package shell
  :ensure nil
  :hook (shell-mode . (lambda () (display-line-numbers-mode -1))))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda () (display-line-numbers-mode -1))))

;; Vterm & toggle
(use-package vterm)
(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-reset-window-configration-after-exit t)
  :config
  (global-set-key (kbd "C-<return>") 'vterm-toggle)
  (add-to-list 'display-buffer-alist '("^vterm-toggle.*"
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (dedicated . t)
                                       (reusable-frames . visible)
                                       (window-height . 0.3))))
(use-package eshell-vterm)

(use-package quickrun
  :bind ("C-c r" . quickrun))

;; PDF Tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page))

;; Guile
(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; Latex
(use-package auctex
  :defer t
  :hook (LaTeX-mode . (lambda ()
                        (setq TeX-PDF-mode t)
                        (TeX-source-correlate-mode)
                        (local-set-key (kbd "C-c C-a") 'TeX-command-run-all)))
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; EAF
(use-package eaf
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker nil) 
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer))

;; Typst TS Mode
(use-package typst-ts-mode
  :ensure t
  :custom
  (typst-ts-mode-watch-options "--open"))

;; Typst Preview
(use-package typst-preview
  :after (typst-ts-mode eaf)
  :init
  (setq typst-preview-autostart t)
  (setq typst-preview-open-browser-automatically t)
  :config
  (setq typst-preview-executable "tinymist")
  
  (defun my/typst-preview-browser (url)
    (if (featurep 'eaf-browser)
        (eaf-open-browser url)
      (browse-url-firefox url)))
      
  (setq typst-preview-browser 'my/typst-preview-browser)
  
  (setq typst-preview-invert-colors "never")
  (setq typst-preview-partial-rendering t)
  
  ;; Hotkeys
  (define-key typst-ts-mode-map (kbd "C-c C-p") 'typst-preview-mode)
  (define-key typst-ts-mode-map (kbd "C-c C-b") 'typst-preview-open-browser)
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position))

;; Org-Babel with Coq
(require 'proof-general)
(require 'company-coq nil t)

(defun org-babel-execute:coq (body params)
  "Execute a block of Coq code with org-babel."
  (let* ((session (cdr (assoc :session params)))
         (coq-buffer (or session "*coq*"))
         (result-params (cdr (assoc :result-params params)))
         (full-body (org-babel-expand-body:generic body params)))
    (unless (get-buffer coq-buffer)
      (save-window-excursion
        (let ((coq-prog-name "coqtop")
              (coq-prog-args '("-Q" "." "LF")))
          (coq-mode)
          (proof-shell-start)
          (rename-buffer coq-buffer))))
    (with-current-buffer coq-buffer
      (goto-char (point-max))
      (insert full-body)
      (proof-goto-point))
    (unless (member "silent" result-params)
      "")))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))

(setq coq-load-path '(("." "LF")))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package treesit-auto
  :config (global-treesit-auto-mode))

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
