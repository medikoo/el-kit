;; el-kit/html.el --- Custom html-mode functions

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

(require 'el-kit/sgml nil t)

;;;###autoload
(defun el-kit-html-clear-hrefs (&optional url)
	"Makes all anchors hrefs and form actions point to given URL.
	If no URL given then point to current document name (works ok with html
	files)."
	(interactive)
	(let ((filename (or filename (file-name-nondirectory (buffer-file-name)))))
		(el-kit-sgml-replace-attributes "a" "href" filename)
		(el-kit-sgml-replace-attributes "form" "action" filename)))

(provide 'el-kit/html)
