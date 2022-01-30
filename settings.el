;;; package setup
;; Bootstraps use-package and sets the repositories.
;; set up package sources
(require 'package)
(require 'nsm)

(setq network-security-level 'high
      gnutls-min-prime-bits 2048
      gnutls-verify-error t)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 20)
        ("melpa" . 20)))

(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;; additional keywords
(defun my/add-new-keywords()
  "Some words like FIXME and TODO should be highlighted in every programming
mode. It doesn't matter if they're inside comments or not."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|fixme\\)\\>" 1 'font-lock-warning-face prepend)
     ("\\<\\(TODO\\|todo\\)\\>" 1 'font-lock-warning-face prepend)
     ("\\<\\(BUG\\|bug\\)\\>" 1 'font-lock-warning-face prepend)
     ("\\<\\(NOTE\\|note\\)\\>" 1 'font-lock-constant-face prepend))))

(add-hook 'prog-mode-hook #'my/add-new-keywords)

;;; align
(defun my/align-whitespace (begin end)
  "Align columns by whitespace between BEGIN and END."
  (interactive "r")
  (align-regexp begin end
                "\\(\\s-*\\)\\s-" 1 0 t))

(use-package align
  :after evil
  :bind*
  (:map evil-visual-state-map
        ("<leader> a a" . align)
        ("<leader> a r" . align-regexp)
        ("<leader> a w" . my/align-whitespace)))

;;; bookmarks
;; Some bookmark keybindings.
(use-package bookmark
  :bind*
  ("<leader> b l" . list-bookmarks)
  ("<leader> b s" . bookmark-set)
  ("<leader> b j" . bookmark-jump))

;;; C, C++
(defalias 'cxx-mode #'c++-mode)

(use-package clang-format
  :ensure t
  :defer t)

(use-package cc-mode
  :defer t
  :config
  (dolist (map (list c-mode-map c++-mode-map))
    (define-key map (kbd "C-c i") #'clang-format-buffer))

  (defconst my-cc-style
    '((c-basic-offset . 4)
      (c-comment-only-line-offset . 0)
      (c-offsets-alist
       (innamespace . 0)
       (case-label . +)
       (statement-block-intro . +)
       (knr-argdecl-intro . +)
       (substatement-open . 0)
       (substatement-label . 0)
       (label . 0)
       (statement-cont . +)
       (inline-open . 0)
       (inexpr-class . 0))))

  (c-add-style "my-style" my-cc-style)
  :custom
  (c-default-style "my-style"))

;;; calendar
(use-package calendar
  :defer t
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-week-start-day 1))

;;; CMake
(use-package cmake-mode
  :ensure t
  :defer t
  :hook
  (cmake-mode . (lambda ()
                  (setq-local company-backends (list #'company-cmake))))
  :config
  (evil-define-key 'normal cmake-mode-map (kbd "<leader> h h") #'cmake-help)
  :custom
  (cmake-tab-width 4))

;;; company
;; Generic settings for the completion framework company.
(use-package company
  :ensure t
  :demand
  :config
  (add-hook 'after-init-hook #'global-company-mode)

  (dolist (key '("<tab>" "TAB"))
    (define-key company-active-map (kbd key) #'company-complete-common-or-cycle))

  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    "Don't use orderless for company."
    (let ((completion-styles my/default-completion-styles))
      (apply orig-fun args)))

  (defun my/setup-prog-mode-completion ()
    "Setup company backends for `prog-mode' derived modes."
    (setq-local company-backends
                '((:separate company-capf company-files company-yasnippet)
                  company-keywords)))

  :hook (prog-mode . my/setup-prog-mode-completion)
  :custom
  (company-global-modes '(not eshell-mode))
  (company-selection-wrap-around t)
  (company-idle-delay 0.1)
  (company-tooltip-minimum-width 40)
  (company-minimum-prefix-length 1))

;;; compile
(defun my/colorize-compilation ()
  "Handle ANSI colors in compilation-mode."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(use-package compile
  :defer t
  :config
  (add-hook 'compilation-filter-hook #'my/colorize-compilation)
  :custom
  (compilation-scroll-output 'first-error))

;;; consult
(use-package consult
  :ensure t
  :after evil
  :bind*
  ("C-c f" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-c d" . consult-imenu)
  ("C-c k" . consult-yank-pop)
  ("<leader> f r" . consult-recent-file)
  ("<leader> f l" . consult-locate)
  ("<leader> f e" . consult-file-externally)
  :config
  (define-advice consult-line
      (:around (orig-fun &rest args) enable-preview)
    (let ((consult-preview-key 'any))
      (apply orig-fun args)))
  :custom
  (consult-preview-key nil)
  (consult-async-refresh-delay 0.1)
  (consult-line-point-placement 'match-end)) ;; Needed for M-j to work

;;; devdocs
(defun my/view-docs-for-major-mode ()
  "Read the documentation for the programming language of the
    current major-mode. Use `devdocs-install' to download docsets."
  (interactive)
  (let ((devdocs-current-docs
         (cdr (assoc major-mode '((sh-mode      . ("bash"))
                                  (rust-mode    . ("rust"))
                                  (c-mode       . ("c"))
                                  (c++-mode     . ("c" "cpp"))
                                  (cmake-mode   . ("cmake~3.21"))
                                  (haskell-mode . ("haskell~9"))
                                  (latex-mode   . ("latex"))
                                  (tex-mode     . ("latex"))
                                  (python-mode  . ("python~3.9")))))))
    (devdocs-lookup (not devdocs-current-docs) (thing-at-point 'symbol t))))

(use-package devdocs
  :ensure t
  :bind*
  ("<leader> h d" . my/view-docs-for-major-mode)
  :after evil
  :custom
  (devdocs-cache-timeout 3600))

;;; dired
(use-package dired-x
  :bind*
  ("<leader> f d" . dired-jump-other-window)
  (:map dired-mode-map
        ("<tab>" . dired-omit-mode)
        ("<backtab>" . dired-hide-details-mode)
        ([remap consult-imenu] . dired-goto-file))
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "C-o") #'dired-display-file)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-omit-files "^\\.?#\\|^\\.[^.\n].*$"))

(use-package wdired
  :defer t
  :config
  (evil-define-key 'normal wdired-mode-map (kbd "u") #'dired-undo))

;;; ediff
(use-package ediff-wind
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;;; embark
(use-package embark
  :ensure t
  :bind*
  ("C-," . embark-act)
  ("C-h b" . embark-bindings)
  (:map minibuffer-mode-map
        ("C-c C-o" . embark-export)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;;; eshell
;;;; config
(use-package eshell
  :bind* (("C-c s" . my/toggle-eshell))
  :hook
  (eshell-mode . (lambda ()
                   (local-set-key (kbd "C-r") #'my/eshell-history)
                   (setq-local completion-styles my/default-completion-styles)
                   (setq-local global-hl-line-mode nil)))
  :config
  (evil-set-initial-state 'eshell-mode 'emacs)
  (push '("\\*eshell\\*" display-buffer-at-bottom (window-height . 0.3))
        display-buffer-alist)
  :custom
  (eshell-cmpl-ignore-case t)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 2048)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-last-dir-ring-size 128)
  (eshell-prompt-function #'my/eshell-prompt))

;;;; prompt
(defun my/eshell-prompt ()
  "Custom eshell prompt."
  (concat
   (when (> eshell-last-command-status 0)
     (propertize (format "(%d) " eshell-last-command-status) 'face 'error))
   (propertize (user-login-name) 'face 'font-lock-type-face)
   (propertize "@" 'face 'font-lock-comment-face)
   (propertize (system-name) 'face 'font-lock-function-name-face)
   " :: "
   (propertize (abbreviate-file-name (eshell/pwd)) 'face 'default)
   (if (= (user-uid) 0) " # " " $ ")))

;;;; popup
(defun my/toggle-eshell ()
  "Open a new eshell window or switch to an existing one."
  (interactive)
  (let ((current-directory default-directory))
    (if (eq major-mode 'eshell-mode)
        (delete-window)
      (eshell)
      (unless (string= default-directory
                       current-directory)
        (eshell/cd current-directory)
        (eshell-reset)))))

;;;; jump
(defun eshell/j ()
  "Jump to a previously visited directory."
  (eshell/cd
   (completing-read "jump: "
                    (delete-dups
                     (ring-elements eshell-last-dir-ring)))))

;;;; open
(defun eshell/o (&rest args)
  "Open ARGS in an external application.
    If there are no arguments open the `default-directory' in an
    external application."
  (if args
      (mapc #'consult-file-externally args)
    (consult-file-externally (expand-file-name default-directory))))

;;;; history
(defun my/eshell-history ()
  "Insert a previous eshell command into the prompt."
  (interactive)
  (goto-char (point-max))
  (insert (completing-read "insert previous command: "
                           (delete-dups
                            (ring-elements eshell-history-ring)))))

;;; evil
;; Vim emulation for emacs.
(use-package evil
  :ensure t
  :demand
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  (evil-lookup-func (lambda () (interactive) (call-interactively #'man)))
  (evil-start-of-line t)
  :bind*
  ("<leader> SPC" . execute-extended-command)
  ("<leader> f f " . find-file)
  ("<leader> f i" . my/edit-init-file)
  ("<leader> f s" . my/ssh-connect)
  ("<leader> q n" . save-buffers-kill-emacs)
  ("<leader> n d" . narrow-to-defun)
  ("<leader> n w" . widen)
  (:map evil-visual-state-map
        ("<leader> n r" . narrow-to-region))
  :config
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (evil-set-leader '(visual normal) (kbd "SPC"))

  (defun my/color-text (text color)
    (propertize text 'face `((:foreground ,color))))

  (setq evil-normal-state-tag   (my/color-text " N " "green")
        evil-emacs-state-tag    (my/color-text " E " "orange")
        evil-insert-state-tag   (my/color-text " I " "red")
        evil-motion-state-tag   (my/color-text " M " "deep sky blue")
        evil-visual-state-tag   (my/color-text " V " "grey80")
        evil-replace-state-tag  (my/color-text " R " "yellow")
        evil-operator-state-tag (my/color-text " O " "purple"))

  (evil-mode))

;;; evil collection
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init)
  :custom
  (evil-collection-key-blacklist (list "SPC"))
  (evil-collection-company-use-tng nil))

;;; evil multiple cursors
(defun my/evil-mc-dispatch ()
  "Dispatch command to create multiple cursors."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "C-n") #'evil-mc-make-and-goto-next-match)
     (define-key map (kbd "C-p") #'evil-mc-make-and-goto-prev-match)
     (define-key map (kbd "C-a") #'evil-mc-make-all-cursors)
     (message "%s" (substitute-command-keys "\\{map}"))
     map)
   t))

(defun my/evil-mc-lines ()
  "Create cursors for each line of the active selection."
  (interactive)
  (evil-mc-make-cursor-in-visual-selection-beg)
  (evil-mc-execute-for-all-cursors #'evil-normal-state))

(evil-define-key 'normal 'global (kbd "<leader> m") #'my/evil-mc-dispatch)
(evil-define-key 'visual 'global (kbd "<leader> m") #'my/evil-mc-lines)

(use-package evil-mc
  :ensure t
  :commands (evil-mc-make-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-make-all-cursors
             evil-mc-make-cursor-in-visual-selection-beg)
  :config
  (setq evil-mc-undo-cursors-on-keyboard-quit t
        evil-mc-custom-known-commands
        '((newline                 (:insert . evil-mc-execute-call))
          (c-indent-line-or-region (:insert . evil-mc-execute-call))
          (wdired--self-insert     (:insert . evil-mc-execute-call))))
  (global-evil-mc-mode 1))

;;; faces
(use-package faces
  :custom-face
  (help-argument-name ((t (:inherit (italic font-lock-function-name-face)))))
  (mode-line ((t (:overline "gainsboro" :background nil))))
  (mode-line-inactive ((t (:overline "dim gray" :background nil))))
  (vertical-border ((t (:foreground "black")))))

;;; fill column indicator
(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom
  (display-fill-column-indicator-character nil))

;;; flycheck/flymake
(defun my/list-errors ()
  "Use flycheck or flymake to list errors."
  (interactive)
  (cond ((bound-and-true-p flycheck-mode) (flycheck-list-errors))
        ((bound-and-true-p flymake-mode) (flymake-show-diagnostics-buffer))
        (t (user-error "Neither flycheck nor flymake are enabled"))))

(define-key evil-normal-state-map (kbd "<leader> e l") #'my/list-errors)

(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

;;; gdb
(use-package gud
  :hook (gud-mode . (lambda ()
                      (setq-local company-minimum-prefix-length 3))))

(use-package gdb-mi
  :custom
  (gdb-restore-window-configuration-after-quit t))

(defun my/debug-rust-program ()
  "Debug the rust program executed by cargo."
  (interactive)
  (let* ((cargo-command (split-string-shell-command
                         (read-string "debug: " "cargo test --bin ")))
         (debug-target-path
          (with-temp-buffer
            (with-environment-variables
                (("CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUNNER" "echo"))
              (apply #'call-process (car cargo-command) nil '(t nil) nil
                     (cdr cargo-command)))
            (string-trim (buffer-string)))))
    (gdb (format "rust-gdb -i=mi -- %s" debug-target-path))))

;;; haskell
(use-package haskell
  :ensure haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :bind*
  (:map interactive-haskell-mode-map
        ("M-." . nil)))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp))

;;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer-other-window)
  :custom
  (ibuffer-default-sorting-mode 'major-mode))

;;; isearch
(use-package isearch
  :bind*
  (:map isearch-mode-map
        ("M-j" . isearch-yank-word-or-char))
  :custom
  (isearch-lazy-count t))

;;; LaTeX
(use-package tex-mode
  :defer t
  :config
  (push '("\\*tex-shell\\*" display-buffer-no-window
          (allow-no-window . t))
        display-buffer-alist))

;;; line numbers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

;;; lsp
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (rust-mode . lsp)
  :bind*
  ("<leader> l r" . lsp-rename)
  ("<leader> l a" . lsp-execute-code-action)
  ("<leader> l h" . lsp-describe-thing-at-point)
  :config
  (setq gc-cons-threshold (* 10 800000)
        read-process-output-max (* 1024 1024))
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil))

(use-package lsp-ui
  :ensure t
  :defer t
  :custom
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-code-actions t))

;;; mail
(use-package mu4e
  :bind* ("C-c m" . mu4e)
  :if (fboundp #'mu4e)
  :config
  (defun my/compose-mail-setup ()
    "Initialize mail settings using the `auth-sources' files."
    (interactive)
    (let* ((users (mapcar (lambda (entry) (plist-get entry :user))
                          (auth-source-search :max 20)))
           (user (completing-read "select user: " users))
           (entry (car (auth-source-search :user user :max 1))))
      (setq user-mail-address user
            smtpmail-smtp-server (plist-get entry :host)
            smtpmail-smtp-service (string-to-number (plist-get entry :port)))))

  (add-hook 'mu4e-compose-pre-hook #'my/compose-mail-setup)
  (setq mu4e-main-hide-personal-addresses t)
  :custom
  (mu4e-view-show-addresses t)
  (mu4e-get-mail-command "mbsync --all")
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function #'completing-read)
  (shr-use-colors nil)
  (smtpmail-stream-type 'ssl)
  (message-send-mail-function #'smtpmail-send-it))

;;; markdown
(use-package markdown-mode
  :defer t
  :custom-face
  (markdown-code-face
   ((t (:inherit default :background nil :foreground nil)))))

;;; occur
(use-package replace
  :hook (occur-mode . next-error-follow-minor-mode)
  :custom
  (list-matching-lines-default-context-lines 3))

;;; orderless
(defconst my/default-completion-styles '(basic partial-completion emacs22))
(use-package orderless
  :ensure t
  :config
  (setq completion-category-defaults nil)
  :custom
  (completion-styles '(orderless))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;;; org
(defun my/org-tag ()
  "Change org-mode tags of the current heading with completion."
  (interactive)
  (let* ((current-tags (org-get-tags nil t))
         (new-tags (completing-read-multiple
                    "change tags: "
                    (org-get-buffer-tags)
                    nil
                    nil
                    (concat (string-join current-tags ",") ","))))
    (org-set-tags (delete-dups new-tags))))

(use-package org
  :bind*
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("C-c t" . my/org-tag))
  :custom-face
  (org-block-begin-line ((t (:underline nil :overline t))))
  (org-block-end-line ((t (:underline t :overline nil))))
  :custom
  (org-agenda-files (list org-directory))
  (org-src-fontify-natively t)
  (org-startup-folded t))

(use-package org-protocol
  :demand
  :config
  (define-advice org-protocol-capture (:before (_) my/focus-capture)
    (select-frame-set-input-focus (window-frame)))
  (define-advice org-protocol-capture (:after (_) my/fill-capture)
    (fill-region (point-min) (point-max))))

(use-package org-capture
  :bind* (("C-c c" . org-capture))
  :custom
  (org-capture-templates
   '(("w" "org-protocol web link" entry (file "Inbox.org")
      "* %?%:description\n  %:link\n\n  %:initial\n"
      :prepend t)
     ("r" "capture region" entry (file "Inbox.org")
      "* %?\n  %i\n"
      :prepend t))))

;;; outline
(use-package outline
  :if (>= emacs-major-version 28)
  :defer t
  :config
  (dolist (key '("<tab>" "TAB"))
    (evil-define-key 'normal outline-minor-mode-map (kbd key) #'outline-cycle))
  :custom
  (outline-minor-mode-highlight 'override))

;;; projectile
(defun my/project-rg ()
  "Search with ripgrep within project.
    If the ripgrep command supports the --pcre2 flag, spaces can be
    used in the query."
  (interactive)
  (let* ((rg-sep " -- ")
         (minibuffer-setup-hook (cons (lambda () (search-backward rg-sep nil t))
                                      minibuffer-setup-hook)))
    (consult-ripgrep (projectile-project-root)
                     (concat (thing-at-point 'symbol t) rg-sep))))

(defun my/projectile-project-find-function (dir)
  "Compatibility layer between projectile and project.el."
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "<leader> p") #'projectile-command-map)
  (autoload #'projectile-command-map "projectile.el" nil nil 'keymap)
  :config
  (push "CMakeLists.txt" projectile-project-root-files-top-down-recurring)
  (define-key projectile-command-map (kbd "s") #'my/project-rg)
  (add-to-list 'project-find-functions #'my/projectile-project-find-function)

  (projectile-mode)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t))

;;; quickrun
(use-package quickrun
  :ensure t
  :bind* ("C-c x" . quickrun))

;;; recentf
(use-package recentf
  :config
  (recentf-mode 1)
  (run-with-idle-timer (* 60 2) t #'recentf-save-list)
  :custom
  (recentf-max-saved-items 1000))

;;; Rust
(use-package rust-mode
  :ensure t
  :bind* (:map rust-mode-map
               ("C-c i" . rust-format-buffer))
  :custom
  (lsp-rust-all-features t)
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-proc-macro-enable t))

;;; theme
(use-package doom-themes
  :ensure t
  :config
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (unless (custom-theme-enabled-p 'doom-dracula)
                (load-theme 'doom-dracula t))))
  (unless (daemonp)
    (load-theme 'doom-dracula t)))

;;; undo-tree
(use-package undo-tree
  :ensure t
  :demand
  :config
  (defun my/undo-tree-config ()
    (setq undo-tree-visualizer-diff t))
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-relative-timestamps t)
  :hook (undo-tree-mode . my/undo-tree-config))

;;; version control
;; Settings for the builtin vc.el.
(use-package vc
  :config
  (global-set-key (kbd "<leader> v") #'vc-prefix-map)
  (when (eq system-type 'windows-nt) ;; too slow
    (remove-hook 'find-file-hook #'vc-refresh-state))
  (setq vc-log-short-style '(directory file))
  :custom
  (vc-follow-symlinks t)
  (vc-git-annotate-switches '("-w" "-M")))

;; Magit keybindings.
(use-package magit
  :ensure t
  :bind*
  ("<leader> g s" . magit-status)
  ("<leader> g g" . magit-dispatch)
  ("<leader> g l" . magit-log-current)
  :custom
  (magit-diff-refine-hunk t))

;;; vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;;; which key
;; show keybindings while typing
(use-package which-key
  :ensure t
  :defer 1
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "<leader> b" "bookmarks"
    "<leader> e" "errors"
    "<leader> f" "files"
    "<leader> g" "git"
    "<leader> h" "help"
    "<leader> l" "lsp"
    "<leader> n" "narrow"
    "<leader> p" "project"
    "<leader> q" "quit"
    "<leader> v" "version control")
  :custom
  (which-key-idle-delay 0.5))

;;; whitespace
(use-package whitespace
  :hook ((prog-mode . whitespace-mode)
         (diff-mode . whitespace-mode))
  :custom
  (fill-column 80)
  (whitespace-line-column fill-column)
  (whitespace-style '(face trailing lines-tail))
  :custom-face
  (whitespace-line
   ((t (:underline (:color "cyan" :style wave) :foreground nil)))))

;;; yasnippet
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-load-directory (locate-user-emacs-file "snippets") 'jit))

;; Also load the snippets.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; sane defaults
;;;; visual
;; don't show a startup message
(setq inhibit-startup-message t)
;; no blinking cursor
(blink-cursor-mode -1)
;; highlight the current line
(global-hl-line-mode)
;; column numbers
(column-number-mode 1)
;; show matching parentheses
(show-paren-mode)
;; pretty lambdas
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)
;; smoother scrolling
(setq scroll-conservatively most-positive-fixnum)
;; resize windows proportionally
(setq window-combination-resize t)
;; hide minor-modes in mode-line
(setq mode-line-modes
      '("%[(" (:propertize mode-name face font-lock-constant-face) ")%] "))
;; mode-line position format
(setq mode-line-position '(20 "%p L%l C%c"))
;; hide help for minibuffer completion
(setq completion-show-help nil)
;; only show one column for minibuffer completion
(setq completions-format 'one-column)

;;;; convenience
;; save backups in .emacs.d
(setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
;; save auto-save files (#file#) in .emacs.d
(let ((auto-save-dir "~/.emacs.d/.autosaves/"))
  (make-directory auto-save-dir t)
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
;; update files when they change on disk
(global-auto-revert-mode 1)
;; ask before killing emacs
(setq confirm-kill-emacs #'y-or-n-p)
;; automatically go to the help window
(setq help-window-select t)
;; show help on hover
(customize-set-variable 'help-at-pt-display-when-idle t)
;; sentences have a single space at the end
(setq sentence-end-double-space nil)
;; typed text replaces the selected text
(delete-selection-mode 1)
;; don't accelerate mouse wheel scrolling
(setq mouse-wheel-progressive-speed nil)
;; set scroll speed
(setcar mouse-wheel-scroll-amount 5)
;; show off-screen matching parens when typing
(setq blink-matching-paren 'echo)
;; add matching pairs automatically
(electric-pair-mode 1)
;; make *scratch* unkillable
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
;; reduce the delay for creating frames and for using undo-tree
(when (eq system-type 'gnu/linux)
  (setq x-wait-for-event-timeout nil))
;; ask "(y/n)?" and not "(yes/no)?"
(defalias #'yes-or-no-p #'y-or-n-p)
;; more information on describe-key
(global-set-key (kbd "C-h c") #'describe-key)
;; add a newline at the end of files
(setq require-final-newline t)
;; no tabs
(customize-set-variable 'indent-tabs-mode nil)
;; use M-o for other-window
(global-set-key (kbd "M-o") #'next-window-any-frame)
;; utf-8 everywhere
(prefer-coding-system 'utf-8)
;; disable suspend-frame
(global-unset-key (kbd "C-x C-z"))
;; make scripts executable on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; custom functions
;;;; edit init file
(defun my/edit-init-file ()
  "Open the init file."
  (interactive)
  (find-file (locate-user-emacs-file "settings.el")))

;;;; indent buffer
(defun my/indent-buffer ()
  "Remove trailing whitespace, indent the current buffer and
remove tabs. In Makefiles only remove trailing whitespace."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (unless (derived-mode-p 'makefile-mode)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))))

(global-set-key (kbd "C-c i") #'my/indent-buffer)

;;;; toggle maximize buffer
;; Original from https://gist.github.com/3402786.
(defun my/toggle-maximize-buffer ()
  "Maximize the current buffer and save the window configuration.
    A second call restores the old window configuration."
  (interactive)
  (if (and (one-window-p t)
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'my/toggle-maximize-buffer)

;;;; ssh connect
(defun my/ssh-connect (host user)
  "Connect to the home directory of a foreign HOST as USER using
SSH. With prefix argument use sshx instead of ssh."
  (interactive "Mhost: \nMuser: ")
  (message "connecting to %s@%s ..." user host)
  (let ((method (if current-prefix-arg "sshx" "ssh")))
    (find-file (format "/%s:%s@%s:~" method user host))))

;;;; duckduckgo dwim
(defun my/ddg-dwim ()
  "Search duckduckgo.com for the symbol at point or the region if active."
  (interactive)
  (let* ((symbol (or (thing-at-point 'symbol t) ""))
         (user-input
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
            (read-string (format "search term [default: \"%s\"]: " symbol)
                         nil
                         'ddg-input-history
                         symbol))))
    (thread-last user-input
                 url-hexify-string
                 (concat "https://duckduckgo.com/?q=")
                 browse-url)))

(global-set-key (kbd "C-c g") #'my/ddg-dwim)

;;;; kill current buffer
(defun my/kill-current-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'my/kill-current-buffer)

;;;; yank words to minibuffer
(defvar-local my/yank-pos nil)

(defun my/remember-yank-pos ()
  "Remember the point in the current buffer when entering the minibuffer."
  (with-current-buffer (cadr (buffer-list))
    (setq my/yank-pos (point))))

(add-hook 'minibuffer-setup-hook #'my/remember-yank-pos)

(defun my/minibuffer-yank-word (&optional arg)
  "Yank ARG words from current line into minibuffer."
  (interactive "p")
  (let (text)
    (with-current-buffer (cadr (buffer-list))
      (save-excursion
        (goto-char my/yank-pos)
        (let* ((beg (point))
               (bol (line-beginning-position))
               (eol (line-end-position))
               (end (progn (forward-word arg)
                           (goto-char (max bol (min (point) eol))))))
          (setq text (buffer-substring-no-properties beg end)
                my/yank-pos end)
          (pulse-momentary-highlight-region beg end 'highlight))))
    (when text
      (insert (replace-regexp-in-string "  +" " " text t t)))))

(define-key minibuffer-local-map (kbd "M-j") #'my/minibuffer-yank-word)

;;;; confirm closing emacsclient frames
(defun my/confirm-delete-frame (&optional arg)
  "Ask for confirmation when closing frames.
With prefix ARG, silently save all file-visiting buffers, then
delete the selected frame."
  (interactive "P")
  (when (y-or-n-p "Close frame? ")
    (save-some-buffers arg)
    (delete-frame))
  (message ""))

(when (daemonp)
  (global-set-key [remap save-buffers-kill-terminal] #'my/confirm-delete-frame))

;;;; create TAGS file
(defun my/create-tags (dir extensions)
  "Create TAGS for files with EXTENSIONS in directory DIR."
  (interactive "Dproject root: \nMenter file extensions: ")
  (let* ((default-directory dir)
         (extensions (split-string extensions))
         (extensions (mapcar (lambda (e) (concat "*." e))
                             extensions)))
    (shell-command
     (concat
      (find-cmd
       '(prune (name ".git" ".svn" ".CVS"))
       `(or (iname ,@extensions)))
      " | etags -"))
    (let ((tags-revert-without-query t))
      (visit-tags-table dir))))

;;;; revert buffer safely
(defun my/revert-buffer (arg)
  "Revert the current buffer if the associated file wasn't modified.
With prefix argument ARG reinitialize the modes."
  (interactive "P")
  (if (and (buffer-modified-p)
           (buffer-file-name))
      (message "Can't revert modified file!")
    (revert-buffer 'noauto 'noconfirm (not arg))
    (message "Buffer reverted")))

(global-set-key (kbd "<f5>") #'my/revert-buffer)

;;; local variables
;; Local Variables:
;; outline-minor-mode-hook: (lambda nil (outline-hide-sublevels 1))
;; outline-regexp: ";;;+"
;; eval: (outline-minor-mode)
;; End:
