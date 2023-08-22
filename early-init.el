;;;; early-init.el --- Benjamin's Emacs Configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Benjamin Kästner

;; Author:  bkaestner
;; Keywords: configuration, misc

;;; Commentary:

;; This is my early GNU Emacs configuration. There are some goals for this
;; file:
;;
;; 1. The total number of lines (including commentary) must never exceed 100
;;    lines, with `fill-column' set to 80.
;; 2. There must not be any line-count reducing tricks, such as using
;;    `setq' with four arguments in a single line. `dolist' is fine.
;; 3. The configuration should provide a nice Emacs experience for me.
;;    That includes proper readability for future me.

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

;;; Disable UI Elements
;; Usually, this code would reside within my `(use-package emacs ...)' form, but
;; this won't prevent the graphical user interface from showing them for a split
;; second. Not too much of a deal on *nix, but it's visible on Windows.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;; Next, I certainly dislike the behavior of Emacs frames, namely: whenever you
;; change any frame or face setting, you will end up with a moved frame.
(setq frame-inhibit-implied-resize t)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:
