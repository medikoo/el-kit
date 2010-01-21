;; my/list.el --- list helpers

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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defun my-list-replace (list search replace)
	"In given LIST Replaces all instances of SEARCH with REPLACE."
	(let ((index -1) (length (length list)))
		(while (< (setq index (+ index 1)) length)
			(if (eq search (nth index list))
				(setf (nth index list) replace)))))

(defun my-list-set (list replace)
	"Makes given LIST content same as in REPLACE list.
	Useful when we don't want to create new object
	but keep references to existing one.
	Returns LIST."
	(setcar list (car replace))
	(setcdr list (cdr replace))
	list)

(provide 'my/list)
