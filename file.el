;; el-kit/file.el --- Custom functions that operate of files

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

(require 'el-kit/list nil t)

;;;###autoload
(defun el-kit-file-write (file content &optional nowarn)
	"Writes CONTENT to FILE.
	If NOWARN is nil signal an error on failure."
	(if (file-writable-p file)
		(with-temp-buffer
			(insert content)
			(write-region (point-min) (point-max) file))
		(if nowarn
				nil (error "Could not write file"))))

;;;###autoload
(defun el-kit-file-read (file &optional nowarn)
	"Returns FILE content as string.
	If NOWARN is nil singal an error on failure."
	(if (file-readable-p file)
		(with-temp-buffer
			(insert-file-contents file)
			(buffer-string))
		(if nowarn
			nil (error "Could not read file"))))

;;;###autoload
(defun el-kit-file-list-extension-stats (filelist &optional dosort)
	"Detect most common file extensions in given LIST.
	Return association list where each cons is extension and count number.
	If DOSORT, list is sorted by count."
	(el-kit-list-stats-count (mapcar (lambda (filename)
				(intern (file-name-extension filename)))
			list) dosort))

;;;###autoload
(defun el-kit-file-name-nondirectory-sans-extension (filename)
	"Returns FILENAME base name."
	(file-name-sans-extension (file-name-nondirectory filename)))

;; Copyright (C) 2008, 2009 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el
;;;###autoload
(defun el-kit-file-recent-ido ()
	"Find a recent file using ido."
	(interactive)
	(let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
		(when file
			(find-file file))))

;;;###autoload
(defun el-kit-file-access-date-sort (a b)
	"True if A was accessed later than B.
	To be used with `sort' function."
	(> (float-time (nth 4 (file-attributes a)))
		(float-time (nth 4 (file-attributes b)))))

;;;###autoload
(defun el-kit-file-modification-date-sort (a b)
	"True if A was modified later than B.
	To be used with `sort' function."
	(> (float-time (nth 5 (file-attributes a)))
		(float-time (nth 5 (file-attributes b)))))

;;;###autoload
(defalias 'el-kit-file-name-sort 'string<)

(provide 'el-kit/file)
