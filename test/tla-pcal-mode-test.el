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

;; test indentation
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
    (run-indent-test lines)))

(ert-deftest pcal-mode--indent-else-blocks-tests ()
  (let ((lines '((0 "if 1 /= 2 then")
                 (2 "  foo = 13;")
                 (0 "else")
                 (2 "  foo = 42;")
                 (0 "end if;")
                 ;
                 (0 "if 2 /= 3 then")
                 (2 "  bar = 13;")
                 (2 "  /* ignore else in comment")
                 (2 "  baz = 27;")
                 (0 "end if;")
                 ;
                 (0 "if 3 /= 4 then")
                 (2 "  if 4 /= 5 then")
                 (4 "    foo = 23;")
                 (2 "  else")
                 (4 "    foo = 97;")
                 (2 "  end if;")
                 (2 "  foo = 77;")
                 (0 "end if;")
                 )))
    (run-indent-test lines)))

(ert-deftest pcal-mode--indent-end-block-tests ()
  (let ((lines '((0 "if 1 /= 2 then")
                 (2 "  foo = 18;")
                 (0 "end if;")
                 ;
                 (0 "if 2 /= 3 then")
                 (2 "  /* ignore end in comment")
                 (2 "  baz = 29;")
                 (0 "end if;")
                 )))
    (run-indent-test lines)))

(ert-deftest pcal-mode--indent-process-tests ()
  (let ((lines '((0 "process thing")
                 (2 "  begin")
                 (4 "    foo = 49;")
                 (0 "end process;")
                 ;;
                 (0 "fair process thing")
                 (2 "  begin")
                 (4 "    foo = 49;")
                 (0 "end process;")
                 ;;
                 (0 "fair+ process thing")
                 (2 "  begin")
                 (4 "    foo = 49;")
                 (0 "end process;")
                 ;;
                 (0 "process thing")
                 (2 "  variables")
		 (4 "    processtest_x = 17,")
		 (4 "    processtest_y = 23;")
                 (2 "  begin")
                 (4 "    processtest_foo = 49;")
                 (0 "end process;")
                 )))
    (run-indent-test lines)))

(defun run-indent-test (lines)
  (dotimes (n (length lines))
    (with-temp-buffer
      (dolist (line lines)
        (insert (cadr line) "\n"))
      (goto-char (point-min))
      (forward-line n) ; Add something in to place point at various places in line?
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space))
      (let* ((pcal-mode-indent-offset 2) ; dynamic binding because of defvar
             (actual-indent (pcal-mode--indent-column))
             (exp-spec (elt lines n))
             (exp-indent (car exp-spec))
             (exp-line (cadr exp-spec)))
        ; Add enough extra data to make clear where the failure happened
        (should (equal (list exp-indent (list 'line n) exp-line)
                       (list actual-indent (list 'line n) exp-line)))))))

; Local Variables:
; tab-width: 8
; End:
