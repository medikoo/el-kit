;; my/regexp.el --- regexp helpers

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

(defun my-regexp-group (list)
	"Returns regexp that is buld of strings in LIST.
	Regexp will match any string found in a LIST.
	This shortcut allows crating such regexps in more readable way."
	(concat "\\(" (mapconcat 'identity list "\\)\\|\\(") "\\)"))

(provide 'my/regexp)