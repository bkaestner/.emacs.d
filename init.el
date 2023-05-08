;;;; init.el --- Benjamin's Emacs Configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Benjamin Kästner

;; Author:  bkaestner
;; Keywords: configuration, misc

;;; Commentary:

;; This is my GNU Emacs configuration. There are some goals for this
;; configuration and thus my editor experience:
;;
;; 1. The total number of lines (including commentary and early-init.el)
;;    must never exceed *400 lines*, with `fill-column' set to 80.
;; 2. The configuration should have self-contained sections that can be copied
;;    into other configurations if necessary.
;; 3. The configuration should be `outline-minor-mode' compatible.
;; 4. There must not be any line-count reducing tricks, such as using
;;    `setq' with four arguments in a single line. `dolist' is fine.
;; 5. The configuration should provide a nice Emacs experience for me.

;;; Code:

;;; Package management
;; Package management in Emacs can be done in several ways. I personally like
;; `use-package' together with package.el. Some will prefer straight.el, but I
;; haven't found the need for it yet.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
(setq package-quickstart t)

;; For the actual package configuration, I use `use-package'. There is also
;; leaf.el, but I haven't looked into it yet.
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (unless (ignore-errors (require 'use-package))
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs' first run.
    (require 'package)
    (package-refresh-contents)
    (package-install 'use-package)
    ;; Only in the first run all packages configured within this file will get
    ;; ensured. Speeds up other startups quite nicely.
    (setq use-package-always-ensure t)
    (require 'use-package)))

;;; Emacs core functionality configuration
;; This section mostly handles and configures built-in packages.
(use-package emacs
  :config
  ;; No audible bell
  (setq-default visible-bell t)
  ;; All things utf-8
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; Don't write backups to all folders
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (add-to-list 'auto-save-file-name-transforms
               '(".*" "~/.emacs.d/auto-save-list/" t))
  (when (member system-type '(windows-nt))
    ;; Disable lockfiles, as they are a hassle on Windows.
    (setq create-lockfiles nil))

  ;; No tabs - except for some files, and Emacs knows which ones.
  (setq-default indent-tabs-mode nil)

  ;; Use tab key as completion option
  (setq tab-always-indent 'complete)

  ;; Show possible whitespace problems...
  (setq-default show-trailing-whitespace t)
  ;; ... except for read-only buffers (e.g. org-agenda)
  (add-hook 'read-only-mode-hook
            (lambda () (setq show-trailing-whitespace (not buffer-read-only))))

  ;; Save custom variables in custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)

  ;; Themes
  (load-theme 'modus-vivendi)

  ;; Additional functions
  (defun bk/edit-user-configuration ()
    "Open the user configuration."
    (interactive)
    (find-file user-init-file))
  (defun bk/edit-user-customization ()
    "Edit the custom file."
    (interactive)
    (find-file custom-file))

  :bind (("C-c f e d" . #'bk/edit-user-configuration)
         ("C-c f e c" . #'bk/edit-user-customization)
         ("M-<f4>"    . #'save-buffers-kill-emacs)))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package recentf
  :defer 2
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode)
  (run-with-idle-timer (* 3 60) t #'recentf-save-list))

(use-package savehist
  :init
  (savehist-mode t))

(use-package saveplace
  :init
  (save-place-mode t))

(use-package winner
  :init
  (winner-mode 1))

;;; Helpers
(use-package which-key
  :defer 1
  :config
  (which-key-mode 1))

(use-package writegood-mode
  :hook text-mode)

;;; Key bindings
(use-package evil
  :defer 1
  :config
  (evil-mode t))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :commands (magit magit-status))

;;; Organization
(use-package calendar
  :commands calendar
  :config
  ;; I like to use the German identifiers for weekdays and months.
  (setq calendar-day-abbrev-array ["So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"]
        calendar-day-name-array   ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                   "Donnerstag" "Freitag" "Samstag"]
        calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                   "Juni" "Juli" "August" "September" "Oktober"
                                   "November" "Dezember"]
        calendar-week-start-day 1))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c"    . #'org-capture)
         ("C-c a"    . #'org-agenda)
         ("C-c l"    . #'org-store-link))
  :config
  (dolist (what '(visual-line-mode org-indent-mode org-display-inline-images))
    (add-hook 'org-mode-hook what))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
          (sequence "TODAY(T)" "|")))

  (setq org-todo-keyword-faces
        '(("WAITING" org-warning :weight bold)
          ("HOLD" org-warning :weight bold)))


  ;; `org-capture-templates' might be customized, so only append/add
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("j" "Journal entry" plain
                   (file+olp+datetree "~/org/journal.org")
                   "**** %?"
                   :time-prompt t)))

  ;; Resize images to 300px, unless there's an attribute
  (setq org-image-actual-width '(300))

  ;; Include org-habits for habits in the agenda
  (add-to-list 'org-modules 'org-habit t)

  (setq org-support-shift-select  t   ; Enable shift+arrow for text selection
        org-agenda-include-diary  t   ; Also include diary on org-agenda
        org-log-into-drawer       t)  ; Don' clutter the actual entry with notes

  (defun my-org-confirm-babel-evaluate (lang _body)
    "Check whether LANG should evaluate BODY without confirmation."
    (not (string= lang "emacs-lisp")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (run-with-idle-timer 60 nil (apply-partially #'org-agenda-prepare-buffers
                                               (org-agenda-files t t))))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package corfu
  :init
  (global-corfu-mode))

;;; Vertico + Marginalia + Consult
;; Provide a nicer `completing-read'
(use-package vertico
  :init
  (setq completion-styles '(basic partial-completion substring)
        completion-ignore-case t)
  (vertico-mode)
  (setq vertico-cycle t))

(use-package consult
  :bind (("C-x b"   . #'consult-buffer)
         ("C-x 4 b" . #'consult-buffer-other-window)
         ("C-c f r" . #'consult-recent-file)
         ("M-i"     . #'consult-imenu)))

;; Add more information to selections in `completing-read' / `vertico'
(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))


;;;; IRC and other communication
(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode))

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:
