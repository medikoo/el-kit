;; el-kit/list.el --- Custom list related functions

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
(defun el-kit-list-replace (list search replace)
	"In given LIST Replaces all instances of SEARCH with REPLACE."
	(unless (eq search replace)
		(let (index)
			(while (setq index  (el-kit-list-index-of list search))
				(setf (nth index list) replace))))
	list)

;;;###autoload
(defun el-kit-list-index-of (list needle)
	"Return index of NEEDLE in given LIST."
	(let ((index -1) (length (length list)) found)
		(while (and (not found) (< (setq index (+ index 1)) length))
			(if (eq needle (nth index list))
				(setq found t)))
		(if found index)))

;;;###autoload
(defun el-kit-list-next (list needle &optional loop)
	"Return LIST element that is next of NEEDLE.
	If LOOP then return first if NEEDLE is last.
	If NEEDLE not found return nil."
	(let ((index (el-kit-list-index-of list needle)))
		(when index
			(setq index (+ index 1))
			(if (< index (length list))
				(nth index list)
				(if loop
					(car list))))))

;;;###autoload
(defun el-kit-list-previous (list needle &optional loop)
	"Return LIST element that is previous of NEEDLE.
	If LOOP then return last if NEEDLE is first.
	If NEEDLE not found return nil."
	(let ((index (el-kit-list-index-of list needle)))
		(when index
			(setq index (- index 1))
			(if (< index 0)
				(if loop
					(car (last list)))
				(nth index list)))))

;;;###autoload
(defun el-kit-list-set (list replace)
	"Makes given LIST content same as in REPLACE list.
	Useful when we don't want to create new object
	but keep references to existing one.
	Returns LIST."
	(setcar list (car replace))
	(setcdr list (cdr replace))
	list)

;;;###autoload
(defun el-kit-list-move-first-to-end (list)
	"First element of LIST becomes last."
	(let ((first (pop list)))
		(nconc list (list first))))

(provide 'el-kit/list)
