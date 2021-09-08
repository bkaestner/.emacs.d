;;; init.el --- Benjamin's Emacs Configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  bkaestner
;; Keywords: configuration, misc

;;; Commentary:

;; This is my GNU Emacs configuration. There area only some goals for this
;; configuration and thus my editor experience:
;;
;; 1. The total number of lines (including commentary) must never exceed 400
;;    lines, with `fill-column' set to 80.
;; 2. The configuration should have self-contained sections that can be copied
;;    into other configurations if necessary.
;; 3. The configuration should be `outline-minor-mode' compatible.
;; 4. There must not be any line-count reducing tricks, such as using
;;    `setq' with four arguments in a single line.
;; 5. The configuration should provide a nice Emacs experience for me.

;;; Code:

;;; Garbage collection
;; This reduces the amount of garbage collection during the initialisation phase
;; of Emacs by increasing both the Garbage collection threshold as well as the
;; maximum heap usage.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
;; After Emacs has completely started, reset the values to more sensible ones.
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
          gc-cons-percentage 0.1)))

;;; Package management
;; Package management in Emacs can be done in several ways. I personally like
;; `use-package' together with package.el. Some will prefer straight.el, but I
;; haven't found the need for it yet.
(require 'package)
(dolist (archive '(("melpa" . "https://melpa.org/packages/") ; Community
                   ("nongnu" . "https://elpa.gnu.org/nongnu/")
                   ("org" . "https://orgmode.org/elpa/")))   ; Only Org <= 9.5
  (add-to-list 'package-archives archive t))
(package-initialize)

;; For the actual package configuration, I use `use-package'. There is also
;; leaf.el, but I haven't looked into it yet.
(eval-when-compile
  (unless (package-installed-p 'use-package)
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs first run. However, I therefore need to
    ;; `package-refresh-contents' regularly on my own.
    ;; TODO Use an idle-timer to refresh the package-contents if stale?
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

;;; Emacs core functionality configuration
(use-package emacs
  :config
  ;; No audible bell
  (setq-default visible-bell t)
  ;; All things utf-8
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)

  ;; Don't write backups to all folders
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (add-to-list 'auto-save-file-name-transforms '(".*" "~/.emacs.d/auto-save-list/" t))

  ;; No tabs
  (setq-default indent-tabs-mode nil)
  ;; Disable usual default bars
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Add convenience functions from other edtiors
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  ;; Always enable line numbers when programming
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; Save custom variables in custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))
;;;; Outline related
(use-package outline
  :hook (prog-mode . outline-minor-mode))
;; TODO: Check whether outline-cycle is in Emacs 28
(use-package outline-magic
  :commands outline-cycle
  :bind (:map outline-minor-mode-map
              ("<tab>" . #'outline-cycle)))

;;; Themes
(load-theme 'wombat)

;;; Helpers
(use-package which-key
  :config
  (which-key-mode 1))

;;; Key bindings
(use-package evil
  :defer 1
  :config
  (evil-mode t))

(use-package general
  :after which-key
  :config
  (defun edit-user-configuration ()
    "Open the user configuration"
    (interactive)
    (find-file user-init-file))
  (defun edit-user-customization ()
    "Edit the custom file"
    (interactive)
    (find-file custom-file))
  (defun load-user-configuration ()
    "Reloads the user configuration"
    (interactive)
    (load-file user-init-file))
  (general-create-definer tyrant-def
    :states '(normal insert visual motion emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  
  (tyrant-def
    "" nil

    ;;; Easier to work with
    "SPC" 'counsel-M-x
    "x" (general-simulate-key "C-x")
    "h" (general-simulate-key "C-h")
    "c" (general-simulate-key "C-c")
    "b" 'counsel-switch-buffer

    ;;; Files
    "f" '(:ignore t :which-key "files")

    ;; Configuration related
    "fe" '(:ignore t :wk "emacs")
    "fed" 'edit-user-configuration
    "fec" 'edit-user-customization
    "feR" 'load-user-configuration

    ;; Finding files
    "fr" 'counsel-recentf
    "ff" 'find-file

    ;; Working with files
    "fs" 'save-buffer)
  (general-define-key   
   "M-<f4>" 'save-buffers-kill-emacs))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :general
  (tyrant-def
    "g" 'magit-status))

(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :bind (
         :map ledger-mode-map
         ("C-c C-r" . ledger-report)
         ;; To get outline-minor-mode in ledger buffers:
         ("TAB" . org-cycle)
         :map ledger-report-mode-map
         ("C-c C-r" . ledger-report))
  :config
  ;; For hledger
  (setq ledger-binary-path "c:/Tools/hLedger/ledger.exe")
  (setq ledger-default-date-format ledger-iso-date-format)

  (setq ledger-reconcile-default-commodity "EUR")
  (setq ledger-report-use-header-line t)
  (setq ledger-report-use-native-highlighting t)
  (setq ledger-report-auto-refresh-sticky-cursor t)
  (setq ledger-report-use-strict t)
  (setq ledger-highlight-xact-under-point nil))

;;; Organization
(use-package calendar
  :config
  ;; I like to use the German identifiers for weekdays and months.
  (setq calendar-day-abbrev-array ["So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"]
        calendar-day-name-array   ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"]
        calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"]
        calendar-week-start-day 1))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode))
  :general
  (general-define-key "C-c c" 'counsel-org-capture)
  (general-define-key "C-c a" 'org-agenda)
  (general-define-key "C-c l" #'org-store-link)
  (:keymaps 'org-mode-map
	    "C-c C-#" 'org-edit-special
	    "C-c ä" 'org-edit-special
            "C-c C-q" 'counsel-org-tag)
  (:keymaps 'org-src-mode-map
	    "C-c C-#" 'org-edit-src-exit
	    "C-c ä" 'org-edit-src-exit)
  :config
  ;; Include org-habits for habits in the agenda
  (add-to-list 'org-modules 'org-habit t)
  ;; Enable shift+arrow for text selection
  (setq org-support-shift-select t)
  ;; Also include diary on org-agenda
  (setq org-agenda-include-diary t)
  
  (defun my-org-confirm-babel-evaluate (lang body)
    "Check whether LANG should evaluate BODY without confirmation."
    (not (member lang '("C"
                        "emacs-lisp"
		        "cpp"
                        "haskell"
		        "plantuml"
		        "ditaa"
		        "gnuplot"
                        "python"
		        "dot"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (run-with-idle-timer 1 nil (lambda()
                       (org-babel-do-load-languages
                        'org-babel-load-languages
                        '((C . t)
                          (haskell . t)
                          (gnuplot . t)
                          (dot . t)
                          (python . t))))))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package projectile
  :config
  (projectile-mode t)
  :general
  (tyrant-def
    "p" 'projectile-command-map))

(use-package rust-mode
  :bind (:map rust-mode-map
	      ("<tab>" . company-indent-or-complete-common)))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.08))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; completing-read support
(use-package ivy
  :config (ivy-mode 1))
(use-package counsel
  :after ivy
  :bind (( "C-x b" . 'counsel-switch-buffer))
  :config (counsel-mode 1))

;;; Recent files
(use-package recentf
  :defer 2
  :config 
  (setq recentf-max-saved-items 100)
  (recentf-mode)
  (run-with-idle-timer (* 3 60) t #'recentf-save-list))

;;;; IRC and other communication
(use-package elcord
  :defer 10
  :config
  (elcord-mode t))

(use-package erc
  :config
  (setq erc-nick "ashraz"
        erc-server "irc.libera.chat"
        erc-user-full-name "Ashraz"))

(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode))

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; End:
