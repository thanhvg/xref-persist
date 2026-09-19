;;; xref-persist.el --- Save and restore xref results across sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Your Name <you@example.com>
;; Maintainer: Your Name <you@example.com>
;; URL: https://github.com/yourname/xref-persist
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, xref

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

;; `xref.el' results normally live and die with the `*xref*' buffer:
;; once you kill it, the list of matches is gone.  xref-persist lets
;; you save the items shown in an xref results buffer and bring them
;; back later as a fully interactive xref buffer again (RET, `n',
;; `p', etc. all keep working), in three ways:
;;
;; 1. As a `.xref' file on disk:
;;
;;      M-x xref-persist-save RET ~/notes/foo.xref RET
;;      ...later, even in a new Emacs session...
;;      C-x C-f ~/notes/foo.xref RET
;;
;;    `.xref' files are opened via `xref-persist-mode', which
;;    renders the saved data exactly like a live xref search, and
;;    the buffer keeps the file's name instead of the generic
;;    "*xref*".
;;
;; 2. Programmatically, via `xref-persist-items-from-buffer' and
;;    `xref-persist-show-items', if you want to build your own
;;    save/restore workflow.
;;
;; For saving results as an Org Babel `xref' src block instead of a
;; file, see the companion file `ob-xref.el', which is built on top
;; of the public functions below and has no effect unless you also
;; use Org.
;;
;; Caveat: only xref items whose location is a file/line/column
;; (`xref-file-location') round-trip -- this covers grep, ripgrep,
;; `project-find-regexp', and most LSP/eglot "find references"
;; backends.  Backends that use other location types (for instance
;; Emacs Lisp's own `M-.' backend) will save with a nil position and
;; won't jump correctly on reload.

;;; Code:

(require 'xref)
(require 'cl-lib)

(defgroup xref-persist nil
  "Save and restore xref results."
  :group 'xref
  :prefix "xref-persist-")

(defcustom xref-persist-default-extension "xref"
  "File extension used when prompting to save xref results."
  :type 'string
  :group 'xref-persist)

;;; --- extracting items from a live xref buffer -------------------------

(defun xref-persist-items-from-buffer (&optional buffer)
  "Return the list of `xref-item' objects shown in BUFFER.
BUFFER defaults to the current buffer, which must be in
`xref--xref-buffer-mode' (or a mode derived from it, such as
`xref-persist-mode')."
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'xref--xref-buffer-mode)
      (user-error "Not in an xref results buffer"))
    (let (items)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((item (get-text-property (point) 'xref-item)))
            (push item items))
          (goto-char (or (next-single-property-change (point) 'xref-item)
                         (point-max)))))
      (nreverse items))))

(defun xref-persist-item-to-plist (item)
  "Convert xref ITEM into a printable plist.
Only xref items backed by a `xref-file-location' (file, line,
column) round-trip meaningfully; others get nil position data.
This is part of the public API other packages (such as
`ob-xref.el') can build on."
  (let ((loc (xref-item-location item)))
    (list :summary (xref-item-summary item)
          :file   (and (xref-file-location-p loc) (xref-file-location-file loc))
          :line   (and (xref-file-location-p loc) (xref-file-location-line loc))
          :column (and (xref-file-location-p loc) (xref-file-location-column loc)))))

(defun xref-persist-plist-to-item (plist)
  "Convert a PLIST produced by `xref-persist-item-to-plist' back into an `xref-item'."
  (xref-make (plist-get plist :summary)
             (xref-make-file-location (plist-get plist :file)
                                       (plist-get plist :line)
                                       (plist-get plist :column))))

;;; --- displaying a list of items ----------------------------------------

(defun xref-persist-show-items (items)
  "Display ITEMS (a list of `xref-item' objects) as an xref buffer."
  (funcall xref-show-xrefs-function (lambda () items) nil))

(defun xref-persist--insert-items (items)
  "Erase the current buffer and render ITEMS as an xref listing."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (xref--insert-xrefs (xref--analyze items))
    (goto-char (point-min))))

;;; --- saving/loading .xref files -----------------------------------------

;;;###autoload
(defun xref-persist-save (file &optional buffer)
  "Save the xref items in BUFFER (default current buffer) to FILE.
BUFFER must be in `xref--xref-buffer-mode' or a derived mode."
  (interactive
   (list (read-file-name "Save xref results to: " nil nil nil
                          (concat "results." xref-persist-default-extension))))
  (let ((items (xref-persist-items-from-buffer buffer)))
    (unless items
      (user-error "No xref items found in this buffer"))
    (with-temp-file file
      (prin1 (mapcar #'xref-persist-item-to-plist items) (current-buffer)))
    (message "Saved %d xref result%s to %s"
             (length items) (if (= (length items) 1) "" "s") file)))

(defun xref-persist--parse-buffer ()
  "Read plist data from the current buffer and turn it into xref items."
  (mapcar #'xref-persist-plist-to-item
          (save-excursion (goto-char (point-min)) (read (current-buffer)))))

(defvar-local xref-persist--source nil
  "Path to the .xref data file backing this `xref-persist-mode' buffer.")

(defun xref-persist--revert (&rest _)
  "Reread `xref-persist--source' from disk and redisplay it."
  (let ((items (with-temp-buffer
                 (insert-file-contents xref-persist--source)
                 (xref-persist--parse-buffer))))
    (xref-persist--insert-items items)
    (setq-local xref--fetcher (lambda () items))))

;;;###autoload
(define-derived-mode xref-persist-mode xref--xref-buffer-mode "Xref-Persist"
  "Major mode for viewing xref results saved to a `.xref' file.
Behaves like a normal xref results buffer (RET, `n', `p', `g',
etc. all work); the difference is that the results were loaded
from a file instead of a live search.  See `xref-persist-save'
to create such a file from a live xref buffer."
  (let ((file buffer-file-name)
        (items (xref-persist--parse-buffer)))
    (xref-persist--insert-items items)
    ;; Detach from the file so a stray `save-buffer' can't clobber the
    ;; saved data with the human-readable rendering.
    (setq buffer-file-name nil)
    (setq-local xref-persist--source file)
    (setq-local xref--fetcher (lambda () items))
    (setq-local revert-buffer-function #'xref-persist--revert)
    (set-buffer-modified-p nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xref\\'" . xref-persist-mode))

(provide 'xref-persist)
;;; xref-persist.el ends here
