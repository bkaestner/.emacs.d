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
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; For the actual package configuration, I use `use-package'. There is also
;; leaf.el, but I haven't looked into it yet.
(eval-when-compile
  (unless (package-installed-p 'use-package)
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs' first run.
    (package-refresh-contents)
    (package-install 'use-package)
    ;; Only in the first run all packages configured within this file will get
    ;; ensured. Speeds up other startups quite nicely.
    (setq use-package-always-ensure t))
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

;;; Emacs core functionality configuration
(use-package emacs
  :config
  ;; No audible bell
  (setq-default visible-bell t)
  ;; All things utf-8
  (set-default-coding-systems 'utf-8)

  ;; Don't write backups to all folders
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (add-to-list 'auto-save-file-name-transforms
               '(".*" "~/.emacs.d/auto-save-list/" t))
  (setq create-lockfiles nil)

  ;; No tabs - except for some files, and Emacs knows which ones.
  (setq-default indent-tabs-mode nil)

  ;; Always enable line numbers when programming
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; Save custom variables in custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)

  ;; Themes
  (load-theme 'modus-vivendi)

  ;; Reload previous buffers and configurations
  (desktop-save-mode)

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

;;;; Outline related
(use-package outline
  :hook (prog-mode . outline-minor-mode)
  :custom (outline-minor-mode-cycle t))

;;; Helpers
(use-package which-key
  :defer 1
  :config
  (which-key-mode 1))

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
         ("C-c l"    . #'org-store-link)
         :map org-mode-map
         ("C-c C-#"  . #'org-edit-special)
         ("C-c ä"    . #'org-edit-special)
         :map org-src-mode-map
         ("C-c C-#"  . #'org-edit-src-exit)
         ("C-c ä"    . #'org-edit-src-exit))
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

  (setq org-stuck-projects '("+project/-DONE"
                             ("TODO" "NEXT" "WAITING" "TODAY") nil nil))

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-custom-commands
                 '("d" "Daily Agenda"
                   ((agenda "" ((org-agenda-span 'day)))
                    (todo "NEXT|TODAY")
                    (stuck "")))))

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


(use-package rust-mode
  :bind (:map rust-mode-map
              ("<tab>" . company-indent-or-complete-common)))

(use-package company
  :defer t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.08))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; Vertico + Marginalia + Consult
;; Provides a nicer `completing-read'
(use-package vertico
  :init
  (setq completion-styles '(basic partial-completion flex)
        completion-ignore-case t)
  (vertico-mode)
  (setq vertico-cycle t))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; Save the history of minibuffer commands (built-in)
(use-package savehist
  :init
  (savehist-mode))

;; Add more information to selections in `completing-read' / `vertico'
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Useful functions for specific situations
(use-package consult
  :bind (("C-x b"   . #'consult-buffer)
         ("C-x 4 b" . #'consult-buffer-other-window)
         ("C-c f r" . #'consult-recent-file)
         ("M-i"     . #'consult-imenu)
         ("C-s"     . #'consult-line)))

;;; Recent files
(use-package recentf
  :defer 2
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode)
  (run-with-idle-timer (* 3 60) t #'recentf-save-list))

;;;; IRC and other communication
(use-package elcord
  :disabled
  :config
  (elcord-mode t))

(use-package erc
  :commands erc
  :config
  (setq erc-server "irc.libera.chat"))

(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode))

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:
