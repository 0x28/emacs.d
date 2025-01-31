;; -*- lexical-binding: t; -*-
;;; package setup
(require 'package)
(require 'nsm)

(setopt network-security-level 'high)
(setopt gnutls-min-prime-bits 2048)
(setopt gnutls-verify-error t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setopt package-archive-priorities
        '(("gnu"    . 99)   ; prefer GNU packages
          ("nongnu" . 80)   ; use non-gnu packages if not found in GNU elpa
          ("melpa"  . 0)))  ; if all else fails, get it from melpa

(setopt use-package-enable-imenu-support t)

;;; custom file
(setopt custom-file (locate-user-emacs-file "custom.el"))
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
(defconst my/dabbrev-regexp (rx (seq (or line-start (not (any ";" alnum)))
                                     (group (+ (any ";" alnum))))))

(defun my/abbrev-back ()
  "Like `backward-char' but don't insert the character that
triggered the abbrev expansion. See `define-abbrev' for details."
  (interactive)
  (backward-char)
  'no-insert)

(put 'my/abbrev-back 'no-self-insert t)

(use-package cc-mode
  :defer t
  :config
  (define-abbrev-table 'c-mode-abbrev-table
    '(("f32" "float")
      ("f64" "double")
      ("i8" "int8_t")
      ("i16" "int16_t")
      ("i32" "int32_t")
      ("i64" "int64_t")
      ("u8" "uint8_t")
      ("u16" "uint16_t")
      ("u32" "uint32_t")
      ("u64" "uint64_t")
      (";r" "return;" backward-char)
      (";i" "#include")
      (";po" "#pragma once\n")
      (";c" "const"))
    "C abbrevs"
    :regexp my/dabbrev-regexp)

  (define-abbrev-table 'c++-mode-abbrev-table
    '((";f" "std::function<>" my/abbrev-back)
      (";v" "std::vector<>" my/abbrev-back)
      (";s" "std::string")
      (";e" "explicit")
      (";u" "std::unique_ptr<>" my/abbrev-back)
      (";sp" "std::shared_ptr<>" my/abbrev-back)
      (";o" "std::optional<>" my/abbrev-back)
      (";nd" "[[nodiscard]]")
      (";uu" "[[maybe_unused]]")
      (";sa" "static_assert()" my/abbrev-back))
    "C++ abbrevs"
    :regexp my/dabbrev-regexp
    :parents (list c-mode-abbrev-table))

  (put 'yas-expand 'no-self-insert t)

  (dolist (key (list "mu" "ms" "fe" "sc" "dc" "rs"))
    (define-abbrev c++-mode-abbrev-table (concat ";" key) key 'yas-expand))

  (dolist (map (list c-mode-map c++-mode-map))
    (keymap-set map "C-c i" #'lsp-format-buffer))

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
  (c-default-style "my-style")
  (lsp-clients-clangd-args '("--header-insertion-decorators=0"
                             "--header-insertion=never")))

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
                  (setq-local company-backends '(company-files company-cmake))))
  :config
  (evil-define-key 'normal cmake-mode-map (kbd "<leader> h h") #'cmake-help)
  :custom
  (cmake-tab-width 4))

;;; company
;; Generic settings for the completion framework company.
(defun my/setup-prog-mode-completion ()
  "Setup company backends for `prog-mode' derived modes."
  (setq-local company-backends
              '((:separate company-capf company-files company-yasnippet)
                company-keywords)))

(use-package company
  :ensure t
  :demand
  :config
  (add-hook 'after-init-hook #'global-company-mode)

  (dolist (key '("<tab>" "TAB"))
    (keymap-set company-active-map key #'company-complete-common-or-cycle))

  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    "Don't use orderless for company."
    (let ((completion-styles my/default-completion-styles))
      (apply orig-fun args)))

  :hook (prog-mode . my/setup-prog-mode-completion)
  :custom
  (company-global-modes '(not eshell-mode))
  (company-selection-wrap-around t)
  (company-idle-delay 0.1)
  (company-tooltip-minimum-width 40)
  (company-tooltip-width-grow-only t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1))

;;; compile
(use-package compile
  :defer t
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
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
  ("C-c D" . consult-imenu-multi)
  ("C-c k" . consult-yank-pop)
  ("<leader> f r" . consult-recent-file)
  ("<leader> f l" . consult-locate)
  ("<leader> h i" . consult-info)
  :config
  (consult-customize consult-line consult-imenu consult-imenu-multi
                     :preview-key 'any)
  :custom
  (consult-preview-key nil)
  (consult-async-refresh-delay 0.1)
  (consult-line-start-from-top t))

;;; dired
(use-package dired-x
  :bind*
  ("<leader> f d" . dired-jump-other-window)
  (:map dired-mode-map
        ("<backtab>" . dired-hide-details-mode)
        ([remap consult-imenu] . dired-goto-file))
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :config
  (defun my/dired-ripgrep ()
    "Run ripgrep on the directory at point or the marked files within dired.
Doesn't respect version control ignores (like .gitignore)."
    (interactive)
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore-vcs")))
      (consult-ripgrep (dired-get-marked-files))))

  (evil-define-key 'normal dired-mode-map
    (kbd "A") #'my/dired-ripgrep
    (kbd "C-o") #'dired-display-file
    (kbd "TAB") #'dired-omit-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches "-ahl --group-directories-first --time-style=long-iso")
  (dired-omit-files "^\\.?#\\|^\\.[^.\n].*$")
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t))

(use-package wdired
  :defer t
  :config
  (evil-define-key 'normal wdired-mode-map (kbd "u") #'dired-undo)
  :custom
  (wdired-allow-to-change-permissions 'advanced))

;;; ediff
(defun my/ediff-save-wincfg ()
  (window-configuration-to-register ?~))

(defun my/ediff-restore-wincfg ()
  (jump-to-register ?~))

(use-package ediff
  :defer t
  :hook
  (ediff-before-setup . my/ediff-save-wincfg)
  (ediff-quit . my/ediff-restore-wincfg)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;;; embark
(use-package embark
  :ensure t
  :after evil
  :commands (embark-open-externally)
  :bind*
  ("C-," . embark-act)
  ("C-h b" . embark-bindings)
  (:map minibuffer-local-map
        ("C-c C-o" . embark-export)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;;; eshell
;;;; config
(use-package eshell
  :after evil
  :bind*
  ("C-c s" . my/toggle-eshell)
  :hook
  (eshell-mode . (lambda ()
                   (keymap-local-set "C-r" #'consult-history)
                   (setq-local imenu-generic-expression '(("Prompt" " $ \\(.*\\)" 1))
                               completion-styles my/default-completion-styles
                               global-hl-line-mode nil)))
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
      (mapc #'embark-open-externally args)
    (embark-open-externally (expand-file-name default-directory))))

;;; evil
;; Vim emulation for emacs.
(use-package evil
  :ensure t
  :demand
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system nil)
  (evil-start-of-line t)
  :bind*
  ("<leader> SPC" . execute-extended-command)
  ("<leader> f f" . find-file)
  ("<leader> f i" . my/edit-init-file)
  ("<leader> f s" . my/ssh-connect)
  ("<leader> h d" . my/view-docs-for-major-mode)
  ("<leader> q n" . save-buffers-kill-emacs)
  ("<leader> q r" . restart-emacs)
  ("<leader> n d" . narrow-to-defun)
  ("<leader> n w" . widen)
  ("<leader> c" . quick-calc)
  (:map evil-visual-state-map
        ("<leader> n r" . narrow-to-region))
  :config
  (keymap-unset evil-normal-state-map "M-.")
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
  (evil-collection-outline-enable-in-minor-mode-p nil))

;;; evil multiple cursors
(defun my/evil-mc-lines ()
  "Create cursors for each line of the active selection."
  (interactive)
  (evil-mc-make-cursor-in-visual-selection-beg)
  (evil-mc-execute-for-all-cursors #'evil-normal-state))

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
          (wdired--self-insert     (:insert . evil-mc-execute-call))
          (kill-sexp               (:default . evil-mc-execute-call))
          (backward-kill-sexp      (:default . evil-mc-execute-call-with-count))
          (backward-kill-word      (:default . evil-mc-execute-call-with-count))))
  (global-evil-mc-mode 1))

;;; faces
(use-package faces
  :custom-face
  (help-argument-name ((t (:inherit (italic font-lock-function-name-face)))))
  (mode-line-active ((t (:box (:line-width 3 :style flat-button)))))
  (mode-line-inactive ((t (:box (:line-width 3 :style flat-button)))))
  (vertical-border ((t (:foreground "dim gray")))))

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
        ((bound-and-true-p flymake-mode) (flymake-show-buffer-diagnostics))
        (t (user-error "Neither flycheck nor flymake are enabled"))))

(keymap-set evil-normal-state-map "<leader> e l" #'my/list-errors)

(defun my/set-flycheck-c++-standard ()
  (setq-local flycheck-gcc-language-standard "c++20"
              flycheck-clang-language-standard "c++20"
              flycheck-cppcheck-standards '("c++20")))

(use-package flycheck
  :ensure t
  :defer t
  :hook ((prog-mode . flycheck-mode)
         (c++-mode . my/set-flycheck-c++-standard))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

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
                         (read-string "debug: " "cargo test --bin "
                                      'my/rust-dbg-history)))
         (debug-target-path
          (with-temp-buffer
            (with-environment-variables
                (("CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUNNER" "echo"))
              (apply #'call-process (car cargo-command) nil '(t nil) nil
                     (cdr cargo-command)))
            (string-trim (buffer-string)))))
    (gdb (format "rust-gdb -i=mi -- %s" debug-target-path))))

;;; goto-chg
(use-package goto-chg
  :ensure t
  :defer t)

;;; haskell
(use-package haskell
  :ensure haskell-mode
  :hook
  ;; prefer lsp completion over interactive-haskell completion
  (haskell-mode . lsp)
  (haskell-mode . (lambda ()
                    (setq-local lsp-rename-use-prepare nil)))
  (haskell-mode . interactive-haskell-mode)
  :bind*
  (:map haskell-mode-map
        ("C-c i" . lsp-format-buffer))
  (:map interactive-haskell-mode-map
        ("M-." . nil)))

(use-package lsp-haskell
  :ensure t
  :defer t)

;;; hippie expand
(use-package hippie-exp
  :bind*
  ("M-/" . hippie-expand)
  :custom
  (hippie-expand-dabbrev-skip-space t)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-dabbrev-from-kill
     try-expand-list
     try-expand-line)))

;;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer-other-window)
  :custom
  (ibuffer-default-sorting-mode 'major-mode))

;;; isearch
(use-package isearch
  :bind*
  (:map isearch-mode-map
        ("M-j" . isearch-yank-word-or-char)
        ("DEL" . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  (isearch-repeat-on-direction-change t))

;;; LaTeX
(use-package tex-mode
  :defer t
  :config
  (push '("\\*tex-shell\\*" display-buffer-no-window
          (allow-no-window . t))
        display-buffer-alist))

;;; line numbers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3))

;;; lsp
(use-package lsp-mode
  :ensure t
  :defer t
  :hook
  (rust-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
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
  (lsp-ui-doc-delay 0.8)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-code-actions t))

;;; markdown
(use-package markdown-mode
  :defer t
  :custom-face
  (markdown-code-face
   ((t (:inherit default :background "unspecified" :foreground "unspecified")))))

;;; occur
(use-package replace
  :hook
  (occur-mode-find-occurrence . recenter)
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
(use-package org
  :bind*
  (:map org-mode-map
        ("C-c t" . org-set-tags-command))
  :custom-face
  (org-block-begin-line ((t (:underline nil :overline t))))
  (org-block-end-line ((t (:underline t :overline nil))))
  :custom
  (org-agenda-files (list org-directory))
  (org-src-fontify-natively t)
  (org-startup-folded t)
  (org-complete-tags-always-offer-all-agenda-tags t))

(use-package org-agenda
  :bind* ("C-c a" . org-agenda)
  :config
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "RET") #'org-agenda-goto
    "gr" #'org-agenda-redo-all
    "s" #'org-agenda-manipulate-query-add
    "q" #'org-agenda-quit
    "K" #'org-agenda-earlier
    "J" #'org-agenda-later)
  (evil-set-initial-state 'org-agenda-mode 'normal))

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
   '(("w" "org-protocol web link" entry (file "capture.org")
      "* %?%:description\n  %:link\n\n  %:initial\n"
      :prepend t)
     ("r" "capture region" entry (file "capture.org")
      "* %?\n  %i\n"
      :prepend t))))

;;; outline
(use-package outline
  :hook
  (ediff-prepare-buffer . outline-show-all)
  (diff-mode . outline-minor-mode)
  :config
  (evil-define-key 'normal outline-minor-mode-map
    (kbd "TAB") #'outline-cycle
    (kbd "<backtab>") #'outline-cycle-buffer
    (kbd "C-j") #'outline-next-heading
    (kbd "C-k") #'outline-previous-heading)
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
    (consult-ripgrep (project-root (project-current t))
                     (concat (thing-at-point 'symbol t) rg-sep))))

(defun my/find-other-file ()
  "Open the other file in the same project that has the longest common
prefix with the current file. Useful for switching between header
and source file."
  (interactive)
  (let* ((project-dir (projectile-project-root))
         (current (file-relative-name buffer-file-name project-dir))
         (nearest (cons -1 "")))
    (dolist (file (projectile-get-other-files buffer-file-name))
      (let ((prefix (abs (compare-strings current nil nil
                                          file nil nil))))
        (when (> prefix (car nearest))
          (setq nearest (cons prefix file)))))
    (unless (string-empty-p (cdr nearest))
      (find-file (expand-file-name (cdr nearest) project-dir))
      (message "%s → %s" current (cdr nearest)))))

(use-package projectile
  :ensure t
  :defer t
  :autoload (projectile-project-root projectile-get-other-files)
  :init
  (keymap-global-set "<leader> p" #'projectile-command-map)
  (autoload #'projectile-command-map "projectile.el" nil nil 'keymap)
  :config
  (keymap-set projectile-command-map "s" #'my/project-rg)
  (projectile-mode)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-enable-cmake-presets t))

;;; quickrun
(use-package quickrun
  :ensure t
  :bind* ("C-c x" . quickrun)
  :custom
  (quickrun-focus-p nil))

;;; recentf
(use-package recentf
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup (* 60 2)))

;;; repeat
(use-package repeat
  :ensure nil
  :config
  (repeat-mode))

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
(use-package modus-themes
  :ensure t
  :config
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (unless (custom-theme-enabled-p 'modus-vivendi)
                (load-theme 'modus-vivendi t))))
  (unless (daemonp)
    (load-theme 'modus-vivendi t))
  :custom
  (modus-themes-common-palette-overrides '((fg-heading-1 magenta-cooler)
                                           (bg-region bg-cyan-subtle)
                                           (fg-region unspecified)
                                           (bg-added bg-added-faint)
                                           (bg-removed bg-removed-faint))))

;;; transient
(use-package transient
  :config
  (transient-define-prefix my/smerge-dispatch ()
    "Convenience function for using smerge."
    :transient-suffix 'stay
    :transient-non-suffix 'call
    ["smerge"
     [("n" "next" smerge-next)
      ("p" "prev" smerge-prev)
      ("N" "next-file" vc-find-conflicted-file)]
     [("u" "keep upper" smerge-keep-upper)
      ("l" "keep lower" smerge-keep-lower)
      ("a" "keep all" smerge-keep-all)]
     [("q" "quit" transient-quit-one)
      ("s" "save" save-buffer)]]
    (interactive)
    (smerge-mode)
    (transient-setup 'my/smerge-dispatch))

  (transient-define-prefix my/mc-dispatch ()
    :transient-suffix 'stay
    :transient-non-suffix 'exit
    ["multiple cursors"
     [("C-a" "all" evil-mc-make-all-cursors)
      ("C-n" "next" evil-mc-make-and-goto-next-match)
      ("C-p" "prev" evil-mc-make-and-goto-prev-match)]])

  (evil-define-key 'normal 'global (kbd "<leader> m") #'my/mc-dispatch))

;;; version control
;; Settings for the builtin vc.el.
(use-package vc
  :config
  (keymap-global-set "<leader> v" #'vc-prefix-map)
  (when (eq system-type 'windows-nt) ;; too slow
    (remove-hook 'find-file-hook #'vc-refresh-state))
  (setq vc-log-short-style '(directory file))
  :custom
  (vc-follow-symlinks t))

(defun my/vc-git-grep ()
  "Run `vc-git-grep' in the current project's root directory."
  (interactive)
  (vc-refresh-state)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'vc-git-grep)))

(use-package vc-git
  :commands (vc-git-grep)
  :bind*
  (:map vc-prefix-map
        ("S" . #'my/vc-git-grep))
  :custom
  (vc-git-annotate-switches '("-w" "-M")))

;; Magit keybindings.
(use-package magit
  :ensure t
  :bind*
  ("<leader> g s" . magit-status)
  ("<leader> g g" . magit-dispatch)
  ("<leader> g l" . magit-log-current)
  ("<leader> g c" . magit-branch-checkout)
  ("<leader> g f" . magit-fetch-all)
  :custom
  (magit-diff-refine-hunk t))

(defun my/repolist-vc ()
  "Show the VC status of the repo at point in `magit-repolist-mode'."
  (interactive)
  (vc-dir (tabulated-list-get-id)))

(use-package magit-repos
  :bind
  ("<leader> g R" . magit-list-repositories)
  :config
  (evil-define-key 'normal magit-repolist-mode-map (kbd "v") #'my/repolist-vc)
  :custom
  (magit-repository-directories '(("~/repos/" . 1)))
  (magit-repolist-columns
   '(("Name" 25 magit-repolist-column-ident nil)
     ("Flags" 5 magit-repolist-column-flags nil)
     ("Branch" 25 magit-repolist-column-branch nil)
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream ((:sort <)))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream ((:sort <)))
     ("Path" 99 magit-repolist-column-path nil))))

;;; vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;;; vundo
(use-package vundo
  :ensure t
  :bind* ("C-x u" . vundo)
  :config
  (add-hook 'vundo-post-exit-hook
            (lambda ()
              (kill-matching-buffers "^\\*vundo-diff-" nil :no-ask)))
  :custom
  (vundo-roll-back-on-quit nil))

;;; wgrep
(use-package wgrep
  :ensure t
  :defer t)

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
   ((t (:underline (:color "cyan" :style wave) :inherit nil)))))

;;; xml
(defun my/format-xml ()
  "Format XML buffers."
  (interactive)
  (let ((buffer " *format-xml*"))
    (if (zerop
         (call-process-region nil nil "xmllint" nil buffer nil "--format" "-"))
        (replace-buffer-contents buffer 0.5) ;; NOTE: to preserve the point
      (message "xml formatting failed:\n%s" (with-current-buffer buffer
                                              (string-trim (buffer-string)))))
    (kill-buffer buffer)))

(use-package nxml-mode
  :defer t
  :bind*
  (:map nxml-mode-map
        ("C-c i" . my/format-xml)))

;;; xref
(use-package xref
  :defer t
  :custom
  ;; Doesn't work for binary files in emacs 28 because of bug #56624
  ;; (xref-search-program (if (executable-find "rg") 'ripgrep 'grep))
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (defun my/xrefs-apropos-completing-read ()
    "Jump to a symbol by providing a pattern. The symbol can be
anywhere in the current workspace. Also works with `lsp'."
    (interactive)
    (let ((xref-show-xrefs-function #'xref-show-definitions-completing-read))
      (call-interactively #'xref-find-apropos))))

;;; yasnippet
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-load-directory (locate-user-emacs-file "snippets") 'jit))

;;; sane defaults
;;;; visual
;; don't show a startup message
(setopt inhibit-startup-message t)
;; no blinking cursor
(blink-cursor-mode -1)
;; highlight the current line
(global-hl-line-mode)
;; column numbers
(column-number-mode 1)
;; show matching parentheses
(show-paren-mode)
;; show context for matching parentheses
(setopt show-paren-context-when-offscreen t)
;; pretty lambdas
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(setopt prettify-symbols-unprettify-at-point 'right-edge)
;; smoother scrolling
(setopt scroll-conservatively most-positive-fixnum)
;; resize windows proportionally
(setopt window-combination-resize t)
;; hide minor-modes in mode-line
(setq mode-line-modes
      '("%[(" (:propertize mode-name face font-lock-constant-face) ")%] "))
;; mode-line position format
(setq mode-line-position '(20 "%p L%l C%c"))
;; hide help for minibuffer completion
(setopt completion-show-help nil)
;; only show one column for minibuffer completion
(setopt completions-format 'one-column)
;; show details during completion
(setopt completions-detailed t)
;; stop displaying the warnings buffer
(push '("\\*Warnings\\*" display-buffer-no-window
        (allow-no-window . t))
      display-buffer-alist)

;;;; convenience
;; save backups in .emacs.d
(setopt backup-directory-alist '(("." . "~/.emacs.d/.backups")))
;; save auto-save files (#file#) in .emacs.d
(let ((auto-save-dir "~/.emacs.d/.autosaves/"))
  (make-directory auto-save-dir t)
  (setopt auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
;; update files when they change on disk
(global-auto-revert-mode 1)
;; ask before killing emacs
(setopt confirm-kill-emacs #'y-or-n-p)
;; automatically go to the help window
(setopt help-window-select t)
;; reuse the help window when looking up the source or info page
(setopt help-window-keep-selected t)
;; show help on hover
(setopt help-at-pt-display-when-idle t)
;; sentences have a single space at the end
(setopt sentence-end-double-space nil)
;; typed text replaces the selected text
(delete-selection-mode 1)
;; don't accelerate mouse wheel scrolling
(setopt mouse-wheel-progressive-speed nil)
;; set scroll speed
(setcar mouse-wheel-scroll-amount 5)
;; show off-screen matching parens when typing
(setopt blink-matching-paren t)
;; add matching pairs automatically
(electric-pair-mode 1)
;; make *scratch* unkillable
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
;; reduce the delay for creating frames and for using undo-tree
(when (eq system-type 'gnu/linux)
  (setq x-wait-for-event-timeout nil))
;; ask "(y/n)?" and not "(yes/no)?"
(setopt use-short-answers t)
;; more information on describe-key
(keymap-global-set "C-h c" #'describe-key)
;; add a newline at the end of files
(setopt require-final-newline t)
;; no tabs
(setopt indent-tabs-mode nil)
;; use M-o for other-window
(keymap-global-set "M-o" #'next-window-any-frame)
;; add meaning to other-window if there is only one window
(define-advice other-window (:before (&rest _) my/split-if-single)
  "Split window on `other-window' if there is only one window."
  (when (one-window-p)
    (split-window-sensibly)))
;; utf-8 everywhere
(prefer-coding-system 'utf-8-unix)
;; disable suspend-frame
(keymap-global-unset "C-x C-z")
;; make scripts executable on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)
;; highlight last selected error
(setopt next-error-message-highlight t)
;; f5 is revert
(keymap-global-set "<f5>" #'revert-buffer-quick)
;; make URLs clickable
(global-goto-address-mode 1)
;; use all the width for man pages
(setopt woman-fill-frame t)
;; save the command history
(savehist-mode 1)
;; extra keybindings for help commands
(find-function-setup-keys)
;; we keep the abbrevs in this file not a separate file
(setopt save-abbrevs nil)
;; Don't use TAGS for completion because this can be slow when projectile
;; automatically visits TAGS files.
(setq completion-at-point-functions nil)
;; search everywhere when using `apropos'
(setopt apropos-do-all t)

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
    (unless (derived-mode-p 'makefile-mode 'python-mode)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))))

(keymap-global-set "C-c i" #'my/indent-buffer)

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

(keymap-global-set "C-x 1" #'my/toggle-maximize-buffer)

;;;; ssh connect
(defun my/ssh-connect (user-at-host)
  "Open the home directory of a remote user using SSH.
USER-AT-HOST should have the form user@host. On Linux the
method is \"ssh\", otherwise it's \"plink\"."
  (interactive (list (read-string "user@host: " nil 'my/ssh-history)))
  (let ((method (if (eq system-type 'gnu/linux) "ssh" "plink")))
    (find-file (format "/%s:%s:~" method user-at-host))))

;;;; duckduckgo dwim
(defun my/ddg-dwim ()
  "Search duckduckgo.com for the symbol at point or the region if active."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (user-input
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
            (read-string (format-prompt "search term" symbol)
                         nil
                         'ddg-input-history
                         symbol))))
    (thread-last user-input
                 url-hexify-string
                 (concat "https://duckduckgo.com/?q=")
                 browse-url)))

(keymap-global-set "C-c g" #'my/ddg-dwim)

;;;; kill current buffer
(defun my/kill-current-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(keymap-global-set "C-x k" #'my/kill-current-buffer)

;;;; yank words to minibuffer
(defvar my/yank-pos nil)

(defun my/remember-yank-pos ()
  "Remember the point in the current buffer when entering the minibuffer."
  (with-current-buffer (cadr (buffer-list))
    (setq my/yank-pos (cons (point) (current-buffer)))))

(add-hook 'minibuffer-setup-hook #'my/remember-yank-pos)

(defun my/minibuffer-yank-word (&optional arg)
  "Yank ARG words from current line into minibuffer."
  (interactive "p")
  (let (text)
    (with-current-buffer (cdr my/yank-pos)
      (save-excursion
        (goto-char (car my/yank-pos))
        (let* ((beg (point))
               (bol (line-beginning-position))
               (eol (line-end-position))
               (end (progn (forward-word arg)
                           (goto-char (max bol (min (point) eol))))))
          (setq text (buffer-substring-no-properties beg end))
          (setcar my/yank-pos end)
          (pulse-momentary-highlight-region beg end 'highlight))))
    (when text
      (insert (replace-regexp-in-string "  +" " " text t t)))))

(keymap-set minibuffer-local-map "M-j" #'my/minibuffer-yank-word)

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

;;;; documentation lookup
(defun my/view-docs-for-major-mode (&optional arg)
  "Read the documentation for the programming language of the current
major-mode. With prefix ARG the language filter is skipped."
  (interactive "P")
  (let* ((doc-table '((sh-mode      . "bash")
                      (rust-mode    . "rust")
                      (c-mode       . "c")
                      (c++-mode     . "cpp")
                      (cmake-mode   . "cmake")
                      (haskell-mode . "haskell")
                      (latex-mode   . "latex")
                      (tex-mode     . "latex")
                      (python-mode  . "python3")))
         (symbol (thing-at-point 'symbol t))
         (selected-docs (unless arg
                          (cdr (assoc major-mode doc-table))))
         (selected-docs (if selected-docs (concat selected-docs " ") "")))
    (thread-last
      (read-string (format-prompt "%sdoc search" symbol selected-docs)
                   nil
                   'view-docs-history
                   symbol)
      url-hexify-string
      (concat "https://devdocs.io/#q=" selected-docs)
      browse-url)))

;;; local variables
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; outline-regexp: ";;;+"
;; eval: (outline-minor-mode)
;; eval: (outline-hide-sublevels 1)
;; End:
