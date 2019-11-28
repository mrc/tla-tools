;;; tla-pcal-mode-test.el --- testing for TLA and PlusCal modes -*- lexical-binding: t; -*-

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

;; Regression tests for tla-pcal-mode

;;; Code:

(require 'ert)

;; From https://stackoverflow.com/questions/26991001/elisp-get-path-to-file-relative-to-script
(defvar local-directory
  (if load-file-name
      ;; File is being loaded.
      (file-name-directory load-file-name)
    ;; File is being evaluated using, for example, `eval-buffer'.
    default-directory))

(defun test-file-path (fname)
  "Return an absolute path to a file containing properly indented TLA+ or Pluscal"
  (expand-file-name fname local-directory))

;; From http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun read-lines (file-path)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun indent-test (test-name mode)
  "Run a single test of indentation, in either pcal or tla mode"
  ;; You might think: why read into list of lines instead of just operating on
  ;; buffer directly? By having a list of lines, we can test indentation of
  ;; each line separately, being sure the above lines are in the form they were
  ;; in the test file
  (let ((lines (read-lines (test-file-path (concat test-name "." mode))))
        (indent-func (if (equal mode "pcal")
                         #'pcal-mode--indent-column
                       #'tla-mode--indent-column)))
    (run-indent-test lines indent-func)))

(ert-deftest pcal-mode--indent-column-test ()      (indent-test "indent-columns" "pcal"))
(ert-deftest pcal-mode--indent-else-blocks-test () (indent-test "indent-else-blocks" "pcal"))
(ert-deftest pcal-mode--indent-end-block-test ()   (indent-test "indent-end-block" "pcal"))
(ert-deftest pcal-mode--indent-process-test ()     (indent-test "indent-process" "pcal"))
(ert-deftest pcal-mode--indent-label-test ()       (indent-test "indent-label" "pcal"))
(ert-deftest pcal-mode--indent-parens-test ()      (indent-test "indent-parens" "pcal"))

(ert-deftest tla-mode--indent-parens-test ()      (indent-test "indent-parens" "tla"))

(defun run-indent-test (lines indent-func)
  (dotimes (n (length lines))
    (with-temp-buffer
      (dolist (line lines)
        (insert line "\n"))
      (goto-char (point-min))
      (forward-line n) ; Add something in to place point at various places in line?
      (let ((expected-indent 0))
        (save-excursion
          (beginning-of-line)
          (skip-chars-forward " ")
          (setq expected-indent (current-column))
          (beginning-of-line)
          (delete-horizontal-space))
        (let* ((pcal-mode-indent-offset 2) ; dynamic binding because of defvar
               (actual-indent (funcall indent-func))
               (exp-line (elt lines n)))
          ;; Add enough extra data to make clear where the failure happened
          (should (equal (list expected-indent (list 'line n) exp-line)
                         (list actual-indent (list 'line n) exp-line))))))))

(ert-deftest tla-mode--keep-conjucts-indented ()
  (let ((test-lines
         "
/\\ x = 1
  \\/ y = 2
"))
    (with-temp-buffer
      (insert test-lines)
      (goto-char (point-min))
      (forward-line 2)
      ;;(should (string= (thing-at-point 'line) "foo"))
      (should (equal (pcal-mode--indent-column) nil))
      )))

(ert-deftest tla-mode--align-syntax-match-works ()
  (should (equal 0 (string-match-p tla-mode--align-syntax-re "/\\")))
  (should (equal 0 (string-match-p tla-mode--align-syntax-re " /\\")))
  (should (equal 0 (string-match-p tla-mode--align-syntax-re " \\/")))
  )
