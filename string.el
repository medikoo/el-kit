;; el-kit/string.el --- Cutom string related functions

;; Author:	Mariusz Nowak <medikoo+el-kit@medikoo.com>
;; Copyright (C) 2010, 2011 Mariusz Nowak <medikoo+el-kit@medikoo.com>

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
(defun el-kit-string-repeat (string n)
	"Repeat STRING N times"
	(let ((newstr ""))
		(while (> n 0)
			(setq newstr (concat newstr string))
			(setq n (- n 1)))
		newstr))

;;;###autoload
(defun el-kit-string-index-of (haystack needle)
	"Return the index of the first occurrenc of NEEDLE within HAYSTACK."
	(with-temp-buffer
		(insert haystack)
		(goto-char 0)
		(let ((index (search-forward needle nil t)))
			(if index
				(- index (length needle) 1)))))

;;;###autoload
(defun el-kit-string-last-index-of (haystack needle)
	"Return the index of the last occurrenc of NEEDLE within HAYSTACK."
	(with-temp-buffer
		(insert haystack)
		(goto-char (point-max))
		(let ((index (search-backward needle nil t)))
			(if index
				(- index (length needle))))))

(provide 'el-kit/string)
