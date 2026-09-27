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
(defvar efs/default-font-size 115)
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

;; Use system clipboard
(setq select-enable-clipboard t)
(setq select-enable-primary nil)
(setq select-active-regions nil)
(setq interprogram-cut-function #'gui-select-text)
(setq interprogram-paste-function #'gui-selection-value)

(use-package iedit
  :bind ("C-;" . iedit-mode))


;; Fonts
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height efs/default-variable-font-size :weight 'regular)

;; Icons
(use-package nerd-icons)

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
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
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons nil
        dashboard-set-file-icons t))

;; Global Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; General Editing

(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(setq-default tab-width 4)
(global-hl-line-mode +1)
(setq show-paren-delay 0)

(use-package smartparens
  :ensure nil
  :demand t
  :bind
  (:map prog-mode-map
        ("C-c s r"       . sp-raise-sexp)    
        ("C-c s s"       . sp-splice-sexp)   
        ("C-c s u"       . sp-unwrap-sexp)   
        ("C-c s k"       . sp-kill-sexp)     
        ("C-c s w"       . sp-rewrap-sexp)   
        ("C-c s ("       . sp-wrap-round)    
        ("C-c s {"       . sp-wrap-curly)    
        ("C-c s ["       . sp-wrap-square))
  :config
  (require 'smartparens-config)
  (dolist (brace '("(" "[" "{"))
    (sp-pair brace nil :post-handlers '(("||\n[i]" "RET"))))
  (smartparens-global-mode 1))

(use-package apheleia
  :hook ((python-base-mode nix-ts-mode rust-ts-mode c-ts-base-mode c-mode c++-mode) . apheleia-mode)
  :config
  (setf (alist-get 'alejandra apheleia-formatters) '("alejandra" "-q" "-")
        (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'nix-ts-mode apheleia-mode-alist) 'alejandra))

(save-place-mode 1)

(use-package avy
  :bind (("M-j" . avy-goto-char-timer))
  :custom (avy-timeout-seconds 0.3))

(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package crux
  :bind (("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)))

(global-set-key (kbd "C-S-d") #'duplicate-dwim)

(use-package move-text
  :config (move-text-default-bindings))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C-+" . expreg-contract)))

(defun lopl/indent-yanked (&rest _)
  (when (and (derived-mode-p 'prog-mode)
             (not (derived-mode-p 'python-base-mode)))
    (let ((mark-even-if-inactive t))
      (indent-region (min (point) (mark t)) (max (point) (mark t))))))
(advice-add 'yank :after #'lopl/indent-yanked)
(advice-add 'yank-pop :after #'lopl/indent-yanked)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
  :init
  (helm-mode 1)  
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-c b" . helm-bookmarks)
   ("C-c f" . helm-recentf)
   ("C-c g" . helm-grep-do-git-grep)
   ("C-s" . helm-occur)
   ("C-c o" . helm-occur-visible-buffers))
  :config
  (define-key isearch-mode-map (kbd "M-i") #'helm-occur-from-isearch)
  (with-eval-after-load 'helm-occur
    (define-key helm-occur-map (kbd "C-s") #'helm-next-line)
    (define-key helm-occur-map (kbd "C-r") #'helm-previous-line)))

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
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
          ("RET" . dired-find-alternate-file)
         ("^"   . (lambda () (interactive) (find-alternate-file ".."))))
  :custom (dired-listing-switches "-agho --group-directories-first"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

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

(use-package yasnippet
  :ensure nil
  :hook (prog-mode . yas-minor-mode)
  :config
  (unless noninteractive 
    (yas-global-mode 1)))

(use-package yasnippet-snippets
  :after yasnippet)


;; PDF Tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (with-eval-after-load 'with-editor
    (add-to-list 'with-editor-file-name-history-exclude "%PDF")))

;; LaTeX
(use-package latex
  :ensure nil
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . xenops-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . lsp-deferred))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-electric-sub-and-superscript t
        TeX-source-correlate-start-server t)
  (setq-default TeX-master nil)
  :config
  (add-to-list 'TeX-command-list
               '("LatexMk" "latexmk -pdf -synctex=1 %s"
                 TeX-run-TeX nil t :help "Run latexmk"))
  (setq TeX-command-default "LatexMk"
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package cdlatex
  :ensure nil
  :after latex
  :bind (:map cdlatex-mode-map
              ("TAB" . cdlatex-tab)))

(use-package xenops
  :ensure nil
  :after latex
  :hook (xenops-mode . xenops-render)
  :config
  (setq xenops-math-latex-process 'dvisvgm
        xenops-math-image-scale-factor 1.5
        xenops-reveal-on-entry t))

(use-package lsp-latex
  :ensure nil
  :after lsp-mode
  :bind (:map LaTeX-mode-map
              ("C-c b" . lsp-latex-build)
              ("C-c v" . lsp-latex-forward-search))
  :init
  (setq lsp-latex-build-on-save t
        lsp-latex-build-forward-search-after t
        lsp-latex-forward-search-executable "emacsclient"
        lsp-latex-forward-search-args
        '("--eval" "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")"))
  :config
  (defun lopl/ignore-unmapped-forward-search (orig &rest args)
    (condition-case err
        (apply orig args)
      (error
       (unless (string-prefix-p "No such page" (error-message-string err))
         (signal (car err) (cdr err))))))
  (advice-add 'lsp-latex-forward-search-with-pdf-tools
              :around #'lopl/ignore-unmapped-forward-search))

;; LSP
(defun lopl/lsp-deferred ()
  (unless (derived-mode-p 'quakec-mode)
    (lsp-deferred)))

(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . yas-minor-mode)
   ((c-mode c++-mode objc-mode c-ts-base-mode
     java-mode java-ts-mode
     rust-ts-mode
     css-mode css-ts-mode
     csharp-mode csharp-ts-mode
     cmake-ts-mode
     gdscript-mode
     nix-ts-mode) . lopl/lsp-deferred))
  :bind (:map prog-mode-map
              ("M-RET" . lsp-execute-code-action))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        read-process-output-max (* 1024 1024)
        lsp-completion-provider :capf
        lsp-idle-delay 0.100
        lsp-inlay-hint-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-semantic-tokens-enable t
        lsp-enable-snippet t)
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-ts-mode . "nix"))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (add-hook 'lsp-mode-hook #'lsp-inlay-hints-mode))


(use-package company
  :hook ((prog-mode geiser-repl-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 1)   
  (company-idle-delay 0.0)            
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-show-quick-access t)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

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
  
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python3"))

(use-package lsp-ui
  :ensure nil
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.5))

(use-package helm-lsp
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package treemacs
  :commands (treemacs)
  :bind (("C-c e" . treemacs))
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-project-follow-mode t))

(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-nerd-icons
  :after (treemacs)
  :config (treemacs-load-theme "nerd-icons"))
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1)
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

;; Java
(use-package lsp-java
  :after lsp-mode)

;; Web
(use-package impatient-mode)
(use-package web-mode
  :hook (web-mode . impatient-mode))

;; Rust
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'")

(use-package cargo-mode)
(use-package cargo-transient)

;; Godot
(use-package gdscript-mode)

;; C/C++
(use-package cc-mode
  :ensure nil
  :config
  (setq c-basic-offset 4))

(use-package c-ts-mode
  :ensure nil
  :custom (c-ts-mode-indent-offset 4))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(with-eval-after-load 'lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=4"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=iwyu"
          "--header-insertion-decorators=0")))

(use-package treesit-auto
  :config 
  (setq treesit-auto-langs (delq 'latex treesit-auto-langs))
  (global-treesit-auto-mode))


(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :config
  (setq python-indent-offset 4))

(use-package lsp-pyright
  :custom
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-typechecking-mode "strict") 
  :hook (python-base-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

(use-package pyvenv
  :config
  (pyvenv-mode t)
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")
                    org-babel-python-command (concat pyvenv-virtual-env "bin/python"))))
  (add-hook 'pyvenv-post-deactivate-hooks
            (lambda ()
              (setq python-shell-interpreter "python3"
                    org-babel-python-command "python3"))))

(use-package python-pytest
  :after python
  :config
  (dolist (map (list python-mode-map python-ts-mode-map))
    (keymap-set map "C-c t t" #'python-pytest)
    (keymap-set map "C-c t f" #'python-pytest-file)
    (keymap-set map "C-c t F" #'python-pytest-function)))

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
                      (org-superstar-mode 1)
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
  (require 'ox-haunt)
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

(use-package org-superstar
  :after org
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; presentations
(use-package org-tree-slide
  :custom (org-image-actual-width nil))

;; gpg/epa
(require 'epa-file)
(epa-file-enable)

(setq epa-pinentry-mode 'loopback)

(require 'ob-awk)
(require 'ob-calc)
(require 'ob-scheme)
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (scheme     . t)
     (shell      . t)   
     (sql        . t)   
     (sqlite     . t)   
     (R          . t)   
     (C          . t)   
     (awk        . t)
     (calc       . t)
     (plantuml   . t)
     (jupyter    . t))))

(use-package org-journal
  :bind ("C-c j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/Documents/Journal/")
  (org-journal-file-format "%Y-%m-%d.org.gpg")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-encrypt-on-close t)
  (epa-file-encrypt-to '("lopl@lopl.dev")) 
  :config
  (unless (file-exists-p org-journal-dir)
    (make-directory org-journal-dir t))) 

(use-package org-roam
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Notes/")
  (org-roam-db-location "~/Documents/org-roam.db")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n d" . org-roam-dailies-goto-today))
  :config
  (org-roam-db-autosync-mode))

(use-package nix-ts-mode
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
  (global-set-key (kbd "C-`") #'vterm-toggle)
  (define-key vterm-mode-map (kbd "C-`") #'vterm-toggle)
  (add-to-list 'display-buffer-alist '("^vterm-toggle.*"
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (dedicated . t)
                                       (reusable-frames . visible)
                                       (window-height . 0.3))))
(use-package eshell-vterm)

(use-package quickrun
  :bind ("C-c r" . quickrun))

;; Guile
(use-package geiser-guile
  :defer t)

;; Typst
(use-package typst-ts-mode
  :hook (typst-ts-mode . lsp-deferred))

(use-package typst-preview
  :after typst-ts-mode
  :bind (:map typst-ts-mode-map
              ("C-c C-p" . typst-preview-start)))

(use-package websocket)

;; Jupyter
(use-package jupyter
  :ensure nil
  :demand t
  :config
  (setq jupyter-eval-use-overlays t))

(setq org-babel-python-command "python3")

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "jupyter" "jupyter-python"
                      "jupyter-julia" "jupyter-R"))))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

(use-package editorconfig
  :config (editorconfig-mode 1))

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
