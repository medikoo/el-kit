;; el-kit/assoc.el --- Association list related functions

;; Author:	Mariusz Nowak <medikoo+el-kit@medikoo.com>
;; Copyright (C) 2011 Mariusz Nowak <medikoo+el-kit@medikoo.com>

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
(defun el-kit-assoc-transpose (assoc-list)
	"Transpose ASSOC-LIST into two lists, names and values."
	(let (names values)
		(dolist (assoc assoc-list)
			(setq names (nconc names (list (car assoc))))
			(setq values (nconc values (list (cdr assoc)))))
		(cons names values)))

(provide 'el-kit/assoc)
