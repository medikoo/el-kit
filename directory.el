;; el-kit/directory.el --- Custom functions that helps dealing with directories

;; Author:	Mariusz Nowak <mariusz+emacs.my@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+emacs.my@medikoo.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.	 See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;;###autoload
(defun el-kit-directory-files-sorted (directory sort &optional full match)
	"Return `directory-files' sorted with SORT function"
	(let ((result (sort (directory-files directory t match) sort)))
		(if full
			result
			(mapcar 'file-name-nondirectory result))))

;; Copyright (C) 2009 Xah Lee
;; http://xahlee.org/emacs/modernization_elsip_copy_dir.html
;;;###autoload
(defun el-kit-directory-copy (source destination)
	"Copies directory recursively"
	(require 'dired-aux)
	(dired-copy-file-recursive source destination nil nil nil 'always))

(provide 'el-kit/directory)
