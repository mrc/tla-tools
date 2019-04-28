;;; tla-pcal-mode.el --- major mode for editing TLA+ and PlusCal files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Matt Curtis

;; Author: Matt Curtis <matt.r.curtis@gmail.com>
;; Keywords: languages

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

;; Simple mode for editing TLA+ and PlusCal files

;; BUGS

;; Indent is very broken.

;;; Code:

(defvar tla-mode-syntax-table
  (let ((table (make-syntax-table)))
    table))

(defvar pcal-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()" table) ;; also need to handle (* .. *) comments
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?, "." table)
    table))

(defvar tla-mode-font-lock-keywords
  `((,(regexp-opt
       '("ASSUME" "ASSUMPTION" "AXIOM" "CASE" "CHOOSE" "CONSTANT"
	 "CONSTANTS" "DOMAIN" "ELSE" "ENABLED" "EXCEPT" "EXTENDS"
	 "IF" "IN" "INSTANCE" "LET" "LOCAL" "MODULE" "OTHER"
	 "SF_" "SUBSET" "THEN" "THEORUM" "UNCHANGED" "UNION"
	 "VARIABLE" "VARIABLES" "WF_" "WITH")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("TRUE" "FALSE")
       'symbols)
     . font-lock-constant-face)
    (,(regexp-opt
       '("\\\\\\*" "(\\*" "\\*)")
       'symbols)
     . font-lock-keyword-comment-delimiter-face)
    ))

(defvar pcal-mode-font-lock-keywords
  `((,(regexp-opt
       '("assert" "await" "begin" "call" "define" "do" "either"
	 "else" "elsif" "end" "goto" "if" "macro" "or" "print"
	 "procedure" "process" "return" "skip" "then" "variable"
	 "variables" "when" "while" "with" ":=" "||")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("TRUE" "FALSE")
       'symbols)
     . font-lock-constant-face)
    (,(regexp-opt
       '("\\\\\\*" "(\\*" "\\*)")
       'symbols)
     . font-lock-keyword-comment-delimiter-face)
    ))

(defun tla-pcal-mode-set-comment-syntax-vars ()
  (setq-local comment-start "\\\\\\*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "\\(\\\\\\*\\)\\s *"))

(define-derived-mode tla-mode prog-mode "TLA+"
  "Major mode for editing TLA+ and PlusCal files."
  :syntax-table tla-mode-syntax-table
  (setq font-lock-defaults '(tla-mode-font-lock-keywords))
  (tla-pcal-mode-set-comment-syntax-vars)
  )

(define-derived-mode pcal-mode prog-mode "PlusCal"
  "Major mode for editing TLA+ and PlusCal files."
  :syntax-table pcal-mode-syntax-table
  (setq font-lock-defaults '(pcal-mode-font-lock-keywords))
  (tla-pcal-mode-set-comment-syntax-vars)
  (setq-local indent-line-function #'pcal-mode-indent-line)
  )

(defun pcal-mode--indent-column ()
  ;; woefully inadequate, doesn't handle TLA+ rules
  (car (syntax-ppss)))

(defun pcal-mode-indent-line ()
  (interactive)
  (beginning-of-line)
  (indent-line-to (pcal-mode--indent-column)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polymode
(define-hostmode poly-tla-pcal-hostmode
  :mode 'tla-mode)

(define-innermode poly-tla-pcal--pcal-innermode
  :mode 'pcal-mode
  :head-matcher "^(\\* --algorithm"
  :tail-matcher "^end algorithm; \\*)$"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode tla-pcal-mode
  :hostmode 'poly-tla-pcal-hostmode
  :innermodes '(poly-tla-pcal--pcal-innermode))

(provide 'tla-pcal-mode)
;;; tla-pcal-mode.el ends here
