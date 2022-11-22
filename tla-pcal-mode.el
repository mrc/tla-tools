;;; tla-pcal-mode.el --- major mode for editing TLA+ and PlusCal files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Matt Curtis

;; Author: Matt Curtis <matt.r.curtis@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (polymode "0.2") (transient "0.3"))
;; Keywords: languages
;; URL: https://github.com/mrc/tla-tools

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

(require 'polymode)
(require 'transient)
(require 'seq)

;;; Customization
(defgroup tla+ nil
  "Major mode for editing TLA+ and PlusCal files."
  :group 'languages
  :tag "TLA+")

(defvar tla-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()1" table)  ; (* comment starter
    (modify-syntax-entry ?\) ")(4" table)  ; *) comment ender
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?\\ "_ 1" table)  ; \* comment starter, also \A, \in, etc
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

(defvar pcal-mode--identifier-re
  "[A-Za-z0-9_]+") ; Actually, can't be just a digit, but oh well

(defvar tla-pcal-mode--shared-keywords
  `((,(regexp-opt
       '("TRUE" "FALSE")
       'symbols)
     . font-lock-constant-face)
    (,(regexp-opt
       '("\\\\\\*" "(\\*" "\\*)")
       'symbols)
     . font-lock-keyword-comment-delimiter-face)
    ("\\_<\\\\[[:word:]]+"
     . font-lock-builtin-face)
    (,(concat "\\b\\(" pcal-mode--identifier-re "\\)\\((.*)\\)?\\S+==")
     . '(1 font-lock-function-name-face))
     ))

(defvar tla-mode-font-lock-keywords
  `((,(regexp-opt
       '("ASSUME" "ASSUMPTION" "AXIOM" "CASE" "CHOOSE" "CONSTANT"
         "CONSTANTS" "DOMAIN" "ELSE" "ENABLED" "EXCEPT" "EXTENDS"
         "IF" "IN" "INSTANCE" "LET" "LOCAL" "MODULE" "OTHER"
         "SF_" "SUBSET" "THEN" "THEORUM" "UNCHANGED" "UNION"
         "VARIABLE" "VARIABLES" "WF_" "WITH")
       'symbols)
     . font-lock-keyword-face)
    ,@tla-pcal-mode--shared-keywords
    ))

(defvar pcal-mode-font-lock-keywords
  `((,(regexp-opt
       '("assert" "await" "begin" "call" "define" "do" "either"
         "else" "elsif" "end" "goto" "if" "macro" "or" "print"
         "procedure" "process" "return" "skip" "then" "variable"
         "variables" "when" "while" "with" ":=" "||")
       'symbols)
     . font-lock-keyword-face)
    (,(concat "\\bmacro\\S+\\(" pcal-mode--identifier-re "\\)\\((.*)\\)?\\S+begin")
     . '(1 font-lock-function-name-face))
    ,@tla-pcal-mode--shared-keywords))

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
  (setq-local indent-line-function #'tla-mode-indent-line)
  )

(define-derived-mode pcal-mode prog-mode "PlusCal"
  "Major mode for editing TLA+ and PlusCal files."
  :syntax-table pcal-mode-syntax-table
  (setq font-lock-defaults '(pcal-mode-font-lock-keywords))
  (tla-pcal-mode-set-comment-syntax-vars)
  (setq-local indent-line-function #'pcal-mode-indent-line)
  )

(defcustom tla-mode-indent-offset 2
  "Amount of columns to indent lines in TLA+ code."
  :type 'integer
  :safe 'integerp)

(defvar tla-mode--conj-re "/\\\\") ; /\
(defvar tla-mode--disj-re "\\\\/") ; \/
(defvar tla-mode--neg1-re "#")     ; #
(defvar tla-mode--neg2-re "/=")    ; /=
(defvar tla-mode--impl-re "=>")    ; =>

; "^[[:blank:]]*\\(/\\\\\\|\\\\/\\)"
(defvar tla-mode--align-syntax-re
  (concat "[[:blank:]]*\\("
          tla-mode--conj-re "\\|"
          tla-mode--disj-re "\\|"
          tla-mode--neg1-re "\\|"
          tla-mode--neg2-re "\\|"
          tla-mode--impl-re
          "\\)"
          )
  "Regexp for matching TLA+ syntax to align.")

(defun tla-mode--indent-column ()
  "Find the appropriate column for the current line."
  ;; For now, just using the same indentation is used in PlusCal mode.  This is
  ;; mostly right, and I think only really wrong if someone sprinkles in bits
  ;; of PlusCal syntax in their TLA+, in which case, on their head be it.
  (pcal-mode--indent-column))

(defun tla-mode--align-indent ()
  "When looking at the special TLA+ symbols where whitespace is significant,\
   check line above: if it doesn't have those symbols, go to left column;\
   if it *does* have those symbols: if current indent is less than that of \
   line above, go to that indent; otherwise return nil to indicate do nothing."
  (save-excursion
    (skip-chars-forward "[[:blank:]]")
    (let ((cur-indent (current-indentation)))
      (forward-line -1)
      (save-match-data
        (if (looking-at tla-mode--align-syntax-re)
            (progn
              (goto-char (nth 2 (match-data)))
              ;; todo -- could return list of options for in/outdent?
              (if (> cur-indent (current-column))
                  nil
                (current-column)))
          0)))))

(defun tla-mode-indent-line ()
  "Indent the current line according to TLA+ rules."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((col (tla-mode--indent-column)))
      (when col
        (indent-line-to col)))))

(defcustom pcal-mode-indent-offset 2
  "Amount of columns to indent lines in PlusCal code."
  :type 'integer
  :safe 'integerp)

(defvar pcal-mode--block-begin-re
  (concat "^[[:blank:]]*"
          (regexp-opt
           '("begin"
             "define"
             "either"
             "if"
             "macro"
             "procedure"
             "process"
             "fair process"
             "fair+ process"
             "while"
             "with")
           'symbols)
          )
  "Regexp for matching the beginning of a block.")

(defvar pcal-mode--block-else-re
  (concat "^[[:blank:]]*"
          (regexp-opt
           '("else"
             "elsif"
             "or")))
  "Regexp for matching the else/or condition of an if-then-else/either.")

; Strings that matches block end will match process end, check process end first
(defvar pcal-mode--process-end-re
  "^[[:blank:]]*end process\\>"
  "Regexp for matching the end of a process.")

(defvar pcal-mode--block-end-re
  "^[[:blank:]]*end\\>"
  "Regexp for matching the end of a block.")

(defvar pcal-mode--statement-end-re
  ".*;"
  "Regexp for matching the end of a statement.")

(defvar pcal-mode--statement-begin-re
  (concat "^[[:blank:]]*variables?\\>")
  "Regexp for matching the start of a statement.")

(defvar pcal-mode--label-re
  (concat "^[[:blank:]]*" pcal-mode--identifier-re ":[[:blank:]]*$")
  "Regexp for matching a label.")

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

(defun pcal-mode--in-paren (syntax-ppss)
  (> (nth 0 syntax-ppss) 0))

(defun pcal-mode--paren-start (syntax-ppss)
  (nth 1 syntax-ppss))

(defun pcal-mode--indent-column ()
  "Find the appropriate column for the current line, or
nil if the syntax isn't recognized for indentation."
  (save-excursion
    (beginning-of-line)
    (cond ((bobp) 0)
          ((pcal-mode--in-paren (syntax-ppss))
           (goto-char (pcal-mode--paren-start (syntax-ppss)))
           (forward-char 1)
           (skip-chars-forward " ")
           (current-column))
          ((looking-at-p pcal-mode--process-end-re)
           (setq current 0)) ; Possibly should match process start,
          ((or (looking-at-p pcal-mode--block-end-re)
               (looking-at-p pcal-mode--block-else-re))
           (pcal-mode--block-start)
           (current-indentation))
          ((looking-at-p pcal-mode--label-re)
           (pcal-mode--block-start)
           (+ (current-indentation) pcal-mode-indent-offset))
          ((looking-at-p tla-mode--align-syntax-re)
           (tla-mode--align-indent))
          (t
           ;; work backwards and base indent off the previous block
           (let (current after-stmt)
             (while (null current)
               (forward-line -1)
               (cond
                ((looking-at-p pcal-mode--block-end-re)
                 (setq current (current-indentation)))
                ((looking-at-p pcal-mode--statement-end-re)
                 (setq after-stmt t))
                ((looking-at-p pcal-mode--statement-begin-re)
                 (if after-stmt
                     (setq current (current-indentation))
                   (setq current (+ (current-indentation) pcal-mode-indent-offset))))
                ((or (looking-at-p pcal-mode--block-begin-re)
                     (looking-at-p pcal-mode--block-else-re)
                     (looking-at-p pcal-mode--label-re))
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
;; polymode
(define-hostmode poly-tla-pcal-hostmode
  :mode 'tla-mode)

(define-innermode poly-tla-pcal--pcal-innermode
  :mode 'pcal-mode
  :head-matcher "^(\\*[[:blank:]]*--algorithm"
  :tail-matcher "^[[:blank:]]*end[[:blank:]]+algorithm;[[:blank:]]*\\*)$"
  :head-mode 'host
  :tail-mode 'host)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert
(defun tla-auto-insert ()
  (let ((name (file-name-sans-extension (file-name-base (buffer-file-name)))))
    (insert (concat
             "------------------------------ MODULE " name " ------------------------------\n\n"
             (make-string (+ 69 (length name)) ?=))))
  (forward-line -1))
(define-auto-insert 'tla-mode #'tla-auto-insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLC configuration template
(defvar-local tla--current-config-file nil
  "The most recently-used or created configuration file for TLC.")

(defun tla-create-tlc-config-file (config-file)
  "Generate an empty TLC configuration in file CONFIG-FILE."
  (interactive "FTLC configuration filename: ")
  ;; TODO: we could detect all constants, and insert "X <-" for each
  (when (or (not (file-exists-p config-file))
            (yes-or-no-p "File exists, overwrite? "))
    (let ((buffer (find-file-noselect config-file)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "\\* -*- mode: tla; -*-

\\* For documentation of this file, see e.g. Lamport,
\\* \"Specifying Systems\" Section 14.7.1 (Page 262), available
\\* online at http://lamport.azurewebsites.net/tla/book-21-07-04.pdf

\\* CONSTANT definitions
CONSTANTS
\\* X <- const_X_1 \\* All constant definitions here

\\* INIT definition
INIT
\\* Init \\* The name of the Init formula.

\\* NEXT definition
NEXT
\\* Next \\* The name of the Next formula.

\\* INVARIANT definitions
INVARIANTS
\\* TypeOk OtherInvariantOk \\* Any invariant formulas

")
        (tla-mode))
      (setq tla--current-config-file config-file)
      (pop-to-buffer buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands: tlc, pcal, tlatex
(defcustom tla-tlc-command "tlc"
  "The command to run the TLC model checker.
Can be a script, or e.g. directly `java -cp tla2tools.jar tlc2.TLC'."
  :type 'string
  :risky t)

(defcustom tla-pcal-command "pcal"
  "The command to run the PlusCal translator.
Can be a script, or e.g. directly `java -cp tla2tools.jar pcal.trans'."
  :type 'string
  :risky t)

(defcustom tla-tlatex-command "tlatex -latexCommand pdflatex -latexOutputExt pdf"
  "The command to run the TLA-to-LaTeX translator.
Can be a script, or e.g. directly `java -cp tla2tools.jar tla2tex.TLA'.

Note that any arguments except `-shade' and the filename should
be added to this command."
  :type 'string
  :risky t)

(transient-define-infix tla--tlc-config-file ()
  :description "TLC configuration"
  :class 'transient-lisp-variable
  :variable 'tla--current-config-file
  :key "-m"
  :shortarg "-m"
  :argument "-config "
  :reader (lambda (prompt _initial-input _history)
            (read-file-name
             prompt
             (file-name-directory (or tla--current-config-file ""))
             (file-name-nondirectory (or tla--current-config-file ""))
             t
             nil
             (lambda (f)
               ;; If the extension isn't ".cfg", TLC will add ".cfg" to the
               ;; filename by itself and then fail to find the config file
               (or (not (stringp f))
                   (directory-name-p f)
                   (string= (file-name-extension f) "cfg"))))))

(defun tla--run-pcal (&optional _args)
  (interactive
   (list (transient-args 'tla-pcal-transient)))
  (transient-set)
  (let ((filename (file-relative-name buffer-file-name)))
    (set (make-local-variable 'compile-command)
         (concat tla-pcal-command " "
                 " "
                 (shell-quote-argument filename)))
    (compile compile-command)
    ;; PlusCal creates a configuration file for us; use it
    (setq tla--current-config-file (concat (file-name-sans-extension filename) ".cfg"))))

(defun tla--run-tlc (&optional args)
  (interactive
   (list (transient-args 'tla-pcal-transient)))
  (transient-set)
  (set (make-local-variable 'compile-command)
         (concat tla-tlc-command " "
                 "-config " tla--current-config-file " "
                 (if (member "-deadlock" args) "-deadlock " "")
                 (shell-quote-argument (file-relative-name buffer-file-name))))
  (compile compile-command))

(defun tla--convert-to-pdf (&optional args)
  (interactive
   (list (transient-args 'tla-pcal-transient)))
  (transient-set)
  (set (make-local-variable 'compile-command)
       (concat tla-tlatex-command " "
               (if (member "-shade" args) "-shade " "")
               (shell-quote-argument buffer-file-name)))
  (compile compile-command))

(transient-define-prefix tla-pcal-transient ()
  "Menu of commands for TLA+ and PlusCal files."
  :value '("-shade")
  ["TLC Configuration"
   ("c" "Create new TLC configuration" tla-create-tlc-config-file)]
  ["TLC"
   ("-d" "Skip deadlock checking" "-deadlock")
   (tla--tlc-config-file)
   ("m" "Run TLC model checker" tla--run-tlc)]
  ["PlusCal"
   ("t" "Translate PlusCal to TLA+" tla--run-pcal)]
  ["PDF"
   ("-s" "Shade comments" "-shade")
   ("p" "Create PDF Version of spec" tla--convert-to-pdf)])

;;;###autoload
(define-polymode tla-pcal-mode
  :hostmode 'poly-tla-pcal-hostmode
  :innermodes '(poly-tla-pcal--pcal-innermode))

(define-key tla-pcal-mode-map (kbd "C-c C-c") 'tla-pcal-transient)

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.tla\\'" 'tla-pcal-mode))

(provide 'tla-pcal-mode)
;;; tla-pcal-mode.el ends here
