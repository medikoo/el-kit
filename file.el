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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defun my-file-write (file content &optional nowarn)
	"Writes CONTENT to FILE.
	If NOWARN is nil signal an error on failure."
	(if (file-writable-p file)
		(with-temp-buffer
			(insert content)
			(write-region (point-min) (point-max) file))
		(if nowarn
				nil (error "Could not write file"))))

(defun my-file-read (file &optional nowarn)
	"Returns FILE content as string.
	If NOWARN is nil singal an error on failure."
	(if (file-readable-p file)
		(with-temp-buffer
			(insert-file-contents file)
			(buffer-string))
		(if nowarn
			nil (error "Could not read file"))))

(provide 'my/file)
