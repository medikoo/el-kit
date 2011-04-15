;; el-kit/directory.el --- Custom functions that helps dealing with directories

;; Author:	Mariusz Nowak <medikoo+el-kit@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <medikoo+el-kit@medikoo.com>

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

(require 'el-kit/file nil t)

;;;###autoload
(defun el-kit-directory-files (directory &optional match sort convert)
	"Return `directory-files' that match regexp MATCH are sorted with SORT.
	Covert its names with CONVERT."
	(let ((result (directory-files directory t (or match "^[^.]"))))
		(if sort
			(setq result (sort result sort)))
		(if convert
			(mapcar convert result)
			result)))

;;;###autoload
(defun el-kit-directory-common-extension (directory &optional threshold)
	"Detect most common file extension in given DIRECTORY.
	If THRESHOLD is given, then extension is returned only if files with this
	extension are common enough, decided on THRESHOLD value.
	e.g. THRESHOLD of 0.5 will return extension only if at least half of files in
	DIRECTORY have this extension"
	(let* ((files (el-kit-directory-files directory))
			(extcons (car (el-kit-file-list-extension-stats files t))))
		(if (and extcons
				(or (not threshold) (>= (cdr extcons) (* (length files) 0.5))))
			(concat "." (symbol-name (car extcons)))
			"")))

;;;###autoload
(defun el-kit-directory-extension-stats (directory &optional dosort)
	"Detect most common file extensions in given DIRECTORY."
	(el-kit-file-list-extension-stats (el-kit-directory-files directory) dosort))

;; Copyright (C) 2009 Xah Lee
;; http://xahlee.org/emacs/modernization_elsip_copy_dir.html
;;;###autoload
(defun el-kit-directory-copy (source destination)
	"Copies directory recursively"
	(require 'dired-aux)
	(dired-copy-file-recursive source destination nil nil nil 'always))

(provide 'el-kit/directory)
