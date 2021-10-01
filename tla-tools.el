;;; tla-tools.el --- Enhancements for running TLA+ tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Matt Curtis

;; Author: Matt Curtis <matt.r.curtis@gmail.com>
;; Keywords: tools, languages
;; Package-Requires: ((polymode "0.2.2"))

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
;; "Parsing file" lines could lead the errors to the wrong file.  As a
;; hack, there's a super ugly regexp which prevents matching filenames
;; beginning with "/private/", which is where the temp files go on my
;; installation.

;; Error type (warning, error) is not guessed.

;; No extra support for "-tool" option (which makes tlc emit
;; tool-readable messages.)

;; tlc requires a config file, and this package does nothing to help
;; generate it.

;; It would be nice if it auto reverted the buffer after pcal (which
;; rewrites the tla file.)

;;; Code:

(eval-and-compile
  (condition-case nil
      (require 'compile) ;; compilation-error-regexp-alist-alist
    (error nil)))


;;; Compile support
(require 'compile)

(defvar tla-tools-ignore-file-regexp
  "\\(?:/\\(?:p\\|[^p].*?\\|pr\\|p[^r].*?\\|pri\\|pr[^i].*?\\|priv\\|pri[^v].*?\\|priva\\|priv[^a].*?\\|privat\\|priva[^t].*?\\|privat[^e]\\|private[^/]+\\)/\\)"
  "Regexp to skip temp files when trying to work out the module
file name.

The goal is to ignore /private/ at the start of the file name.

This is super ugly, and likely contains bugs, but Emacs doesn't
support lookahead assertions (there is a patch to support it:
https://debbugs.gnu.org/cgi/bugreport.cgi?bug=5393.)")

(defvar tla-tools-state-face 'font-lock-function-name-face
  "Face name to use for state names in tlc output.")

(defvar tla-tools-error-regexp-alist
  `((tlc-error
     "line \\([0-9]+\\), col \\([0-9]+\\) to line \\([0-9]+\\), col \\([0-9]+\\) of module \\([[:word:]]*\\)"
     (5 "%s.tla") (1 . 3) (2 . 4) 2)
    (tlc-nested-error
     "^\\([0-9]+\\)\\. Line \\([0-9]+\\), column \\([0-9]+\\) to line \\([0-9]+\\), column \\([0-9]+\\) in \\(.*\\)$"
     (6 "%s.tla") (2 . 4) (3 . 5) 2)
    (tls-unlabeled-state
     "^State \\([0-9]+\\): \\(<Initial predicate>\\|Stuttering\\)"
     nil nil nil 0)
    (tlc-labeled-state
     "^State \\([0-9]+\\): <\\(.*\\) line \\([0-9]+\\), col \\([0-9]+\\) to line \\([0-9]+\\), col \\([0-9]+\\) of module \\(.*\\)>$"
     (7 "%s.tla") (3 . 5) (4 . 6) 0 0 (2 tla-tools-state-face))
    (tlc-assertion-failure
     "^\"Failure of assertion at line \\([0-9]+\\), column \\([0-9]+\\)\\.\"$"
     nil 1 2 2)
    (tlc-invariant-violation
     "^Error: Invariant \\(.*\\) is violated.*"
     nil nil nil 2 0 (1 tla-tools-state-face))
    (sany-error
     "^Encountered \"\\(.*\\)\" at line \\([0-9]+\\), column \\([0-9]+\\)"
     nil 2 3 2)
    (sany-file
     ,(concat "^Parsing file \\(" tla-tools-ignore-file-regexp ".*\\)$")
     1 nil nil 0 1))
  "List of regexps for TLA+ tools (tlc, pcal, sany)
See `compilation-error-regexp-alist` for the formatting.")

(defun tla-tools-error-regexp-add ()
  "Add TLA+ tools regexps for `compilation-mode`.
This allows \\[next-error]/\\[previous-error] to find the errors."
  (interactive)
  (when (boundp 'compilation-error-regexp-alist-alist)
    (mapcar
     (lambda (item)
       (unless (member (car item) compilation-error-regexp-alist)
	 (push (car item) compilation-error-regexp-alist))
       (let ((e (assoc (car item) compilation-error-regexp-alist-alist)))
	 (if e
	     (setcdr e (cdr item))
	   (push item compilation-error-regexp-alist-alist))))
     tla-tools-error-regexp-alist)))

(defun tla-tools--test-crazy-sany-file-regexp ()
  (let ((re (cadr (assoc 'sany-file tla-tools-error-regexp-alist)))
	(input "Parsing file /Users/mrc/shed/tla/learntla/hanoi.tla
Parsing file /private/var/folders/wl/ftangh/T/TLC.tla
Parsing file /private/var/folders/wl/ftangh/T/Sequences.tla
Parsing file /private/var/folders/wl/ftangh/T/Integers.tla
Parsing file /private/var/folders/wl/ftangh/T/Naturals.tla
Parsing file /private/var/folders/wl/ftangh/T/FiniteSets.tla
Parsing file /Users/private/test
Parsing file /Users/mrc/private/var/folders/wl/ftangh/T/TLC.tla
Parsing file /private/test
Parsing file /p/test
Parsing file /px/test
Parsing file /pr/test
Parsing file /prx/test
Parsing file /pri/test
Parsing file /prix/test
Parsing file /priv/test
Parsing file /privx/test
Parsing file /priva/test
Parsing file /privax/test
Parsing file /privat/test
Parsing file /privatx/test
Parsing file /privatex/test
")
	(expected "Parsing file /Users/mrc/shed/tla/learntla/hanoi.tla
Parsing file /Users/private/test
Parsing file /Users/mrc/private/var/folders/wl/ftangh/T/TLC.tla
Parsing file /p/test
Parsing file /px/test
Parsing file /pr/test
Parsing file /prx/test
Parsing file /pri/test
Parsing file /prix/test
Parsing file /priv/test
Parsing file /privx/test
Parsing file /priva/test
Parsing file /privax/test
Parsing file /privat/test
Parsing file /privatx/test
Parsing file /privatex/test
"))
    (with-temp-buffer
      (insert input)
      (keep-lines re (point-min) (point-max))
      (if (string= (buffer-string) expected)
	  (message "Crazy sany-file regex seems to work!")
	(error "unexpected output: %s" (buffer-string))))))


(provide 'tla-tools)
;;; tla-tools.el ends here


; Local Variables:
; tab-width: 8
; End:
