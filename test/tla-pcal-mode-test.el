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

;; test indentation
(require 'ert)
(ert-deftest pcal-mode--indent-column-tests ()
  (let ((lines '((0  "variables")
		 (2  "  x = 17,")
		 (2  "  y = 23;")
		 (0  "")
		 (0  "define")
		 (2  "  Everything == TRUE")
		 (0  "end define;")
		 (0  "macro fiddle(n) begin")
		 (2  "  if n = 1 then")
		 (4  "    skip;")
		 (2  "  end if;")
		 (0  "end macro;")
         (0  "\* comment with keyword in it: process")
         (0  "\* should not be indented")
		 (0  "")
		 (0  "begin")
		 (2  "  Label1:")
		 (4  "    skip;")
		 (4  "    while 1 /= 2 do")
		 (6  "      if 1 < 2 then")
		 (8  "        skip;")
		 (6  "      else")
		 (8  "        either")
		 (10 "          skip;")
		 (8  "        or")
		 (10 "          skip;")
		 (8  "        end either;")
		 (6  "      end if;")
		 (6  "      Label2:")
		 (8  "        skip;")
		 (4  "    end while;")
		 (0  "end"))))
    (dotimes (n (length lines))
      (with-temp-buffer
        (dolist (line lines)
          (insert (cadr line) "\n"))
        (goto-char (point-min))
        (forward-line n) ; Add something in to place point at various places in line?
        (save-excursion
          (beginning-of-line)
          (delete-horizontal-space))
        (let* ((pcal-mode-indent-offset 2)
               (actual-indent (pcal-mode--indent-column))
               (exp-spec (elt lines n))
               (exp-indent (car exp-spec))
               (exp-line (cadr exp-spec)))
          ; Add enough extra data to make clear where the failure happened
          (should (equal (list exp-indent n exp-line)
                         (list actual-indent n exp-line))))))))
