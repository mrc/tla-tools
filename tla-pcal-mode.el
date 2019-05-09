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
    (modify-syntax-entry ?\( "()1" table)  ; (* comment starter
    (modify-syntax-entry ?\) ")(4" table)  ; *) comment ender
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?\\ ". 1" table)  ; \* comment starter
    (modify-syntax-entry ?* ". 23c" table) ; (* or *) or \*
    (modify-syntax-entry ?\n "> c" table)  ; \* comment ender
    (modify-syntax-entry ?_ "_" table)     ; x_y
    (modify-syntax-entry ?' "_" table)     ; x'
    table))

(defvar pcal-mode-syntax-table
  (let ((table (copy-syntax-table tla-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
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
  (setq-local comment-start "\\*")
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

(defvar tla-mode-indent-offset 2
  "Indent lines by this many columns.")

(defvar tla-mode--conj-re "/\\\\") ; /\
(defvar tla-mode--disj-re "\\\\/") ; \/
(defvar tla-mode--neg1-re "#")     ; #
(defvar tla-mode--neg2-re "/=")    ; /=
(defvar tla-mode--impl-re "=>")    ; =>

(defvar tla-mode--align-syntax-re
  (concat tla-mode--conj-re "\\|"
	  tla-mode--disj-re "\\|"
	  tla-mode--neg1-re "\\|"
	  tla-mode--neg2-re "\\|"
	  tla-mode--impl-re)
  "Regexp for matching TLA+ syntax to align.")

(defvar pcal-mode-indent-offset 2
  "Indent lines by this many columns.")

(defvar pcal-mode--block-begin-re
  (concat ".*\\<begin\\>"            "\\|"
	  ".*\\<if\\>.*\\<then\\>"   "\\|"
	  ".*\\<do\\>"               "\\|"
	  ".*\\<either\\>"           "\\|"
	  ".*\\<define\\>")
  "Regexp for matching the beginning of a block.")

(defvar pcal-mode--block-else-re
  ".*\\<else\\>"
  "Regexp for matching the else condition of an if-then-else.")

(defvar pcal-mode--block-end-re
  (concat ".*\\<end\\>"              "\\|"
	  ".*\\<end if\\>"           "\\|"
	  ".*\\<end either\\>")
  "Regexp for matching the end of a block.")

(defvar pcal-mode--statement-end-re ".*;"
  "Regexp for matching the end of a statement.")

(defvar pcal-mode--statement-begin-re
  (concat ".*\\<variables?\\>"))

(defun tla-mode--indent-column ()
  "Find the appropriate column for the current line."
  ;; Really basic: look at line above, if it has tla syntax, align
  ;; with that. Doesn't handle indenting/outdenting operators that are
  ;; not alike, which is a huge pain.
  (forward-line -1)
  (save-match-data
    (if (looking-at (concat ".*\\(" tla-mode--align-syntax-re "\\)"))
	(progn
	  (goto-char (nth 2 (match-data)))
	  ;; todo -- could return list of options for in/outdent?
	  (current-column))
      0))
  ;; just return nil to skip indenting because it's annoying right now :-(
  nil)

(defvar pcal-mode--align-syntax-re
  tla-mode--align-syntax-re
  "Regexp for matching TLA+ syntax to align.")

(defun pcal-mode--block-start ()
  "Move to the start of the block."
  (beginning-of-line)
  (let ((level 0))
    (while (not (or (bobp) (< level 0)))
      (forward-line -1)
      (cond ((looking-at-p pcal-mode--block-end-re)
	     (setq level (+ level 1)))
	    ((looking-at-p pcal-mode--block-begin-re)
	     (setq level (- level 1))))))
  (point))

(defun pcal-mode--indent-column ()
  "Find the appropriate column for the current line, or
nil if the syntax isn't recognized for indentation."
  (save-excursion
    (beginning-of-line)
    (cond ((bobp) 0)
	  ((looking-at-p pcal-mode--block-end-re)
	   (pcal-mode--block-start)
	   (current-indentation))
	  ((looking-at-p pcal-mode--block-else-re)
	   (pcal-mode--block-start)
	   (current-indentation))
	  ((looking-at-p (concat "[[:blank:]]*" pcal-mode--align-syntax-re))
	   (tla-mode--indent-column))
	  (t
	   ;; work backwards and base indent off the previous block
	   (let (current after-stmt)
	     (while (null current)
	       (forward-line -1)
	       (cond ((looking-at-p pcal-mode--block-end-re)
		      (setq current (current-indentation)))
		     ((looking-at-p pcal-mode--statement-end-re)
		      (setq after-stmt t))
		     ((looking-at-p pcal-mode--statement-begin-re)
		      (if after-stmt
			  (setq current (current-indentation))
			(setq current (+ (current-indentation) pcal-mode-indent-offset))))
		     ((or (looking-at-p pcal-mode--block-begin-re)
			  (looking-at-p pcal-mode--block-else-re))
		      (setq current (+ (current-indentation) pcal-mode-indent-offset)))
		     ((bobp)
		      (setq current 0))))
	     (and current (max 0 current)))))))

(defun pcal-mode-indent-line ()
  "Indent the current line according to PlusCal rules."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((col (pcal-mode--indent-column)))
      (when col
	(indent-line-to col)))))

;; polymode is doing a special indent for the first line, maybe it
;; expects it's lining up markdown or something, but it's no good
;; here. Can maybe fix it with advice around pm--first-line-indent --
;; this uses some undocumented polymode stuff.
(defun tla-pcal-mode--dont-indent-first-line (orig-fun &rest args)
  (let ((res (apply orig-fun args)))
    (if (and polymode-mode ; nb: true for both host and inner mode
	     (eq major-mode 'pcal-mode))
	0
      res)))
(advice-add 'pm--first-line-indent :around #'tla-pcal-mode--dont-indent-first-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test indentation
(require 'ert)
(ert-deftest pcal-mode--indent-column-tests ()
  (let ((lines '((0 "variables")
		 (2 "  x = 17,")
		 (2 "  y = 23;")
		 (0 "")
		 (0 "define")
		 (2 "  Everything == TRUE")
		 (0 "end define;")
		 (0 "macro fiddle(n) begin")
		 (2 "  if n = 1 then")
		 (4 "    skip;")
		 (2 "  end if;")
		 (0 "end macro;")
		 (0 "")
		 (0 "begin")
		 (2 "  skip;")
		 (2 "  while 1 /= 2 do")
		 (4 "    if 1 < 2 then")
		 (6 "      skip;")
		 (4 "    else")
		 (6 "      skip;")
		 (4 "    end if;")
		 (4 "    skip;")
		 (2 "  end while;")
		 (0 "end"))))
    (dotimes (n 100)
      (with-temp-buffer
	(dolist (line lines)
	  (insert (cadr line) "\n"))
	(goto-char (random (point-max)))
	(save-excursion
	  (beginning-of-line)
	  (delete-horizontal-space))
	(let ((res (pcal-mode--indent-column))
	      (exp (car (elt lines (- (line-number-at-pos) 1)))))
	  (should (= exp res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polymode
(define-hostmode poly-tla-pcal-hostmode
  :mode 'tla-mode)

(define-innermode poly-tla-pcal--pcal-innermode
  :mode 'pcal-mode
  :head-matcher "^(\\*[[:blank:]]*--algorithm"
  :tail-matcher "^[[:blank:]]*end[[:blank:]]+algorithm;[[:blank:]]*\\*)$"
  :head-mode 'host
  :tail-mode 'host)

;;;###autoload
(define-polymode tla-pcal-mode
  :hostmode 'poly-tla-pcal-hostmode
  :innermodes '(poly-tla-pcal--pcal-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.tla\\'" 'tla-pcal-mode))

(provide 'tla-pcal-mode)
;;; tla-pcal-mode.el ends here
