;;; tla-tools.el --- Enhancements for running TLA+ tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Matt Curtis

;; Author: Matt Curtis <matt.r.curtis@gmail.com>
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Execute M-x tla-tools-error-regexp-add to add regexps for
;; compilation mode.

;; Using tla-bin (https://github.com/pmer/tla-bin), a TLA+ source file
;; can be checked with M-x compile, with a compile command of "tlc
;; <tla-file>".

;; BUGS
;; ====

;; Doesn't implement much except helping next-error/previous-error.

;; No support TLC reports errors just with the module name, the
;; filename is guessed to be modulename.tla. Sany errors don't have
;; the module name either (makes sense, it's syntax checking the
;; file), so it's guessed from the "Parsing file ..." line. Multiple
;; "Parsing file" lines could lead the errors to the wrong file.

;; Error type (warning, error) is not guessed.

;; No extra support for "-tool" option (which makes tlc emit
;; tool-readable messages.)

;; tlc requires a config file, and this package does nothing to help
;; generate it.

;;; Code:

(eval-and-compile
  (condition-case nil
      (require 'compile) ;; compilation-error-regexp-alist-alist
    (error nil)))


;;; Compile support
(require 'compile)

(defvar tla-tools-error-regexp-alist
  '((tlc-error
     "^line \\([0-9]+\\), col \\([0-9]+\\) to line \\([0-9]+\\), col \\([0-9]+\\) of module \\(.*\\)$"
     (5 "%s.tla") (1 . 3) (2 . 4))
    (tlc-nested-error
     "^\\([0-9]+\\)\\. Line \\([0-9]+\\), column \\([0-9]+\\) to line \\([0-9]+\\), column \\([0-9]+\\) in \\(.*\\)$"
     (6 "%s.tla") (2 . 4) (3 . 5))
    (tlc-assertion-failure
     "^\"Failure of assertion at line \\([0-9]+\\), column \\([0-9]+\\)\\.\"$"
     nil 1 2)
    (sany-error
     "^Encountered \"\\(.*\\)\" at line \\([0-9]+\\), column \\([0-9]+\\)"
     nil 2 3)
    (sany-file
     "^Parsing file \\(.*\\)$"
     1 nil nil nil nil))
  "List of regexps for TLA+ tools (tlc, pcal, sany)
See `compilation-error-regexp-alist` for the formatting.")

(defun tla-tools-error-regexp-add ()
  "Add TLA+ tools regexps for `compilation-mode`.
This allows \\[next-error]/\\[previous-error] to find the errors."
  (interactive)
  (when (and (boundp 'compilation-error-regexp-alist-alist)
	     (not (assoc (caar tla-tools-error-regexp-alist) compilation-error-regexp-alist-alist))
    (mapcar
     (lambda (item)
       (push (car item) compilation-error-regexp-alist)
       (push item compilation-error-regexp-alist-alist))
     tla-tools-error-regexp-alist))))



(provide 'tla-tools)
;;; tla-tools.el ends here
