;;; meow-cjk.el --- CJK word segmentation support for Meow  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Lucius Chen <chenyh572@gmail.com>
;; URL: https://github.com/yourusername/meow-cjk
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (meow "1.0") (emt "2.0.0"))
;; Keywords: chinese, cjk, japanese, korean, convenience, meow

;; This file is NOT a part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides CJK word segmentation support for Meow modal editing.
;; Currently supports EMT backend for macOS, with plans to support additional
;; backends for Linux and Windows.
;;
;; Usage:
;;
;;   (require 'meow-cjk)
;;   (meow-cjk-mode 1)
;;
;; Or with use-package:
;;
;;   (use-package meow-cjk
;;     :after meow
;;     :config
;;     (meow-cjk-mode 1))

;;; Code:

(require 'meow)
(require 'emt)

;;; Customization

(defgroup meow-cjk nil
  "CJK word segmentation support for Meow."
  :group 'meow
  :prefix "meow-cjk-")

(defcustom meow-cjk-backend 'emt
  "Backend for CJK word segmentation.
Available backends:
  - `emt': EMT (macOS only, requires EMT package)"
  :type '(choice (const :tag "EMT (macOS)" emt))
  :group 'meow-cjk)

;;; Helper functions

(defun meow-cjk--select-noncjk (thing type backward regexp-format)
  "Select non-CJK text based on THING, TYPE, and BACKWARD direction.
REGEXP-FORMAT is used to format the search regexp."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (meow--make-selection (cons 'expand type) beg end)
        (meow--select t backward))
      (when (stringp regexp-format)
        (let ((search (format regexp-format (regexp-quote (buffer-substring-no-properties beg end)))))
          (meow--push-search search)
          (meow--highlight-regexp-in-buffer search))))))

(defun meow-cjk--select-cjk (direction backward)
  "Select CJK text based on DIRECTION and BACKWARD direction."
  (let* ((bounds (emt--get-bounds-at-point
                  (emt--move-by-word-decide-bounds-direction direction)))
         (beg (car bounds))
         (end (cdr bounds))
         (text (buffer-substring-no-properties beg end))
         (segments (append (emt-split text) nil))
         (pos (- (point) beg))
         (segment-bounds (car segments)))
    (dolist (bound segments)
      (when (and (>= pos (car bound)) (< pos (cdr bound)))
        (setq segment-bounds bound)))
    (when segment-bounds
      (let* ((seg-beg (+ beg (car segment-bounds)))
             (seg-end (+ beg (cdr segment-bounds)))
             (segment-text (buffer-substring-no-properties seg-beg seg-end))
             (regexp (regexp-quote segment-text)))
        (let ((selection (meow--make-selection (cons 'expand 'word) seg-beg seg-end)))
          (meow--select selection t backward)
          (meow--push-search regexp)
          (meow--highlight-regexp-in-buffer regexp))))))

(defun meow-cjk--forward-thing-1 (thing)
  "Move forward one THING, with CJK support."
  (let ((pos (point)))
    (if (eq thing 'word)
        (emt-forward-word 1)
      (forward-thing thing 1))
    (when (not (= pos (point)))
      (meow--hack-cursor-pos (point)))))

(defun meow-cjk--backward-thing-1 (thing)
  "Move backward one THING, with CJK support."
  (let ((pos (point)))
    (if (eq thing 'word)
        (emt-backward-word 1)
      (forward-thing thing -1))
    (when (not (= pos (point)))
      (point))))

;;; Public API

;;;###autoload
(defun meow-cjk-mark-thing (thing type &optional backward regexp-format)
  "Make expandable selection of THING, with TYPE and forward/BACKWARD direction.

THING is a symbol usable by `forward-thing', which see.

TYPE is a symbol. Usual values are `word' or `line'.

The selection will be made in the \\='forward\\=' direction unless BACKWARD is
non-nil.

When REGEXP-FORMAT is non-nil and a string, the content of the selection will be
quoted to regexp, then pushed into `regexp-search-ring' which will be read by
`meow-search' and other commands. In this case, REGEXP-FORMAT is used as a
format-string to format the regexp-quoted selection content (which is passed as
a string to `format'). Further matches of this formatted search will be
highlighted in the buffer."
  (interactive "p")
  ;; Ensure that EMT is loaded
  (emt-ensure)
  (let ((direction (if backward 'backward 'forward)))
    (if (or (eq type 'symbol) (not (looking-at-p "\\cc")))
        (meow-cjk--select-noncjk thing type backward regexp-format)
      (meow-cjk--select-cjk direction backward))))

;;;###autoload
(defun meow-cjk-next-thing (thing type n &optional include-syntax)
  "Create non-expandable selection of TYPE to the end of the next Nth THING.

If N is negative, select to the beginning of the previous Nth thing instead.
INCLUDE-SYNTAX specifies additional syntax to include in the selection."
  (unless (equal type (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (unless include-syntax
    (setq include-syntax
          (let ((thing-include-syntax
                 (or (alist-get thing meow-next-thing-include-syntax)
                     '("" ""))))
            (if (> n 0)
                (car thing-include-syntax)
              (cadr thing-include-syntax)))))
  (let* ((expand (equal (cons 'expand type) (meow--selection-type)))
         (_ (when expand
              (if (< n 0) (meow--direction-backward)
                (meow--direction-forward))))
         (new-type (if expand (cons 'expand type) (cons 'select type)))
         (m (point))
         (p (save-mark-and-excursion
              (if (and (eq thing 'word) (eq system-type 'darwin))
                  (progn
                    (emt-ensure) ;; Ensure EMT is loaded
                    (if (> n 0)
                        (emt-forward-word n)
                      (emt-backward-word (- n))))
                (forward-thing thing n))
              (unless (= (point) m)
                (point)))))
    (when p
      (thread-first
        (meow--make-selection
         new-type
         (meow--fix-thing-selection-mark thing p m include-syntax)
         p
         expand)
        (meow--select t))
      (meow--maybe-highlight-num-positions
       (cons (apply-partially #'meow-cjk--backward-thing-1 thing)
             (apply-partially #'meow-cjk--forward-thing-1 thing))))))

;;; Minor mode

(defvar meow-cjk--original-mark-thing nil
  "Original `meow-mark-thing' function.")

(defvar meow-cjk--original-next-thing nil
  "Original `meow-next-thing' function.")

;;;###autoload
(define-minor-mode meow-cjk-mode
  "Minor mode for CJK word segmentation support in Meow."
  :global t
  :lighter " MeowCJK"
  (if meow-cjk-mode
      (progn
        ;; Ensure EMT is loaded
        (emt-ensure)
        ;; Advise meow functions
        (advice-add 'meow-mark-thing :override #'meow-cjk-mark-thing)
        (advice-add 'meow-next-thing :override #'meow-cjk-next-thing))
    ;; Remove advice when disabled
    (advice-remove 'meow-mark-thing #'meow-cjk-mark-thing)
    (advice-remove 'meow-next-thing #'meow-cjk-next-thing)))

(provide 'meow-cjk)

;;; meow-cjk.el ends here
