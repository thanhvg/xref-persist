;;; ob-xref.el --- Org Babel support for saved xref results -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Your Name <you@example.com>
;; Maintainer: Your Name <you@example.com>
;; URL: https://github.com/yourname/xref-persist
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (xref-persist "0.1.0"))
;; Keywords: tools, convenience, xref, org

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Lets you keep xref results inline in an Org file instead of (or
;; alongside) a `.xref' file on disk.  Requires `xref-persist'.
;;
;;   M-x xref-persist-copy-as-org-block
;;   ;; yank into your notes:
;;   #+begin_src xref :results none
;;   ((:summary "..." :file "..." :line 10 :column 0) ...)
;;   #+end_src
;;
;; Put point in the block and press `C-c C-c' any time later to
;; reopen it as an interactive xref buffer.  No `org-babel-do-load-
;; languages' entry is needed; Org finds `org-babel-execute:xref'
;; by name as soon as this file is loaded.

;;; Code:

(require 'xref-persist)
(require 'org)

;;;###autoload
(defvar org-babel-default-header-args:xref '((:results . "none"))
  "Default header arguments for #+begin_src xref blocks.
Results are suppressed by default since the effect of evaluating
such a block is an xref buffer popping up, not a textual result.")

;;;###autoload
(defun org-babel-execute:xref (body _params)
  "Execute a #+begin_src xref block and display it as an xref buffer.
BODY is read as a list of plists, as produced by
`xref-persist-copy-as-org-block' / `xref-persist-item-to-plist'."
  (xref-persist-show-items
   (mapcar #'xref-persist-plist-to-item (car (read-from-string body))))
  nil)

;;;###autoload
(defun xref-persist-copy-as-org-block (&optional buffer)
  "Copy BUFFER's xref items (default current buffer) as an Org src block.
Yank the result into an Org file; `C-c C-c' on the block later
reopens it as an interactive xref buffer."
  (interactive)
  (let ((items (xref-persist-items-from-buffer buffer)))
    (unless items
      (user-error "No xref items found in this buffer"))
    (with-temp-buffer
      (insert "#+begin_src xref :results none\n")
      (pp (mapcar #'xref-persist-item-to-plist items) (current-buffer))
      (insert "#+end_src\n")
      (kill-new (buffer-string))))
  (message "Xref org block copied to the kill ring -- yank it into your org file"))

(provide 'ob-xref)
;;; ob-xref.el ends here
