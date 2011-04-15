;; el-kit/window --- Custom window related functions

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

;;;###autoload
(defun el-kit-window-serialize (&optional window)
	"Serializes WINDOW characterstic to string.
	Useful when we want to keep such configuration in file between sessions.
	If WINDOW is nil then current window is used."
	(if (not window)
		(setq window (selected-window)))
	(let ((buffer (window-buffer window)))
		(list
			(list 'edges (window-edges window))
			(list 'height (window-height window))
			(list 'width (window-width window))
			(list 'buffer-name (buffer-name buffer))
			(list 'buffer-file-name (buffer-file-name buffer))
			(list 'point (window-point window))
			(list 'selected (eq window (selected-window)))
			(list 'display (list (window-start window) (window-end window))))))

;;;###autoload
(defun el-kit-window-unserialize (data &optional window)
	"Unserializes WINDOW from DATA string (created by `el-kit-window-unserialize').
	If WINDOW is nil then current window is used."
	(if (not window)
		(setq window (selected-window)))
	(set-window-buffer window
		(or
			(if (second (assoc 'buffer-file-name data))
				(find-file-noselect (second (assoc 'buffer-file-name data)))
				(get-buffer (second (assoc 'buffer-name data))))
			(get-buffer "*scratch*")
			(current-buffer)))
	(set-window-point window (second (assoc 'point data)))
	(set-window-start window (car (second (assoc 'display data)))))

(provide 'el-kit/window)
