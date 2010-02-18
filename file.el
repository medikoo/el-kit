;; my/file.el --- file operation helpers

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
(defun my-file-write (file content &optional nowarn)
	"Writes CONTENT to FILE.
	If NOWARN is nil signal an error on failure."
	(if (file-writable-p file)
		(with-temp-buffer
			(insert content)
			(write-region (point-min) (point-max) file))
		(if nowarn
				nil (error "Could not write file"))))

;;;###autoload
(defun my-file-read (file &optional nowarn)
	"Returns FILE content as string.
	If NOWARN is nil singal an error on failure."
	(if (file-readable-p file)
		(with-temp-buffer
			(insert-file-contents file)
			(buffer-string))
		(if nowarn
			nil (error "Could not read file"))))

;; Copyright (C) 2008, 2009 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el
;;;###autoload
(defun my-file-recent-ido ()
	"Find a recent file using ido."
	(interactive)
	(let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
		(when file
			(find-file file))))

;;;###autoload
(defun my-file-modification-date-sort (a b)
	"True if A was modified later than B.
	To be used with `sort' function."
	(> (float-time (nth 5 (file-attributes a)))
		(float-time (nth 5 (file-attributes b)))))

(provide 'my/file)