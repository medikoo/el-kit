;; my/frame.el --- frame helpers

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

(require 'my/window)

(defun my-frame-serialize (&optional frame)
	(list
		(list 'left (frame-parameter frame 'left))
		(list 'top (frame-parameter frame 'top))
		(list 'height (frame-parameter frame 'height))
		(list 'width (frame-parameter frame 'width))
		(list 'windows
			(let* (
					(first-window (frame-first-window frame))
					(current-window first-window)
					(windows (list (my-window-serialize current-window))))

				(while
					(not
						(eq first-window
							(setq current-window (next-window current-window))))
					(nconc windows (list (my-window-serialize current-window))))
				windows))))

(defun my-frame-unserialize (data &optional frame)
	;; (message "--- UNSERIALIZE ---")
	(if (not frame)
		(setq frame (window-frame (selected-window))))

	(let* (
			(h-factor (/ (float (frame-parameter frame 'height)) (second (assoc 'height data))))
			(w-factor (/ (float (frame-parameter frame 'width)) (second (assoc 'width data))))
			(first-window (frame-first-window frame))
			(window first-window)
			(windows-data (second (assoc 'windows data)))
			previous
			selected-window
			(selected-frame (window-frame (selected-window))))

		(delete-other-windows first-window)
		(dolist (current windows-data)
			(if previous
				(progn (split-window window nil (eq (second (second (assoc 'edges current))) (second (second (assoc 'edges previous)))))
					(setq window (next-window window))))
			(setq previous current))
		(setq window first-window)

		(select-frame frame)
		(select-window first-window)
		(dolist (current windows-data)
			;; (message "ENLARGE HEIGHT: %S %S %S %S" h-factor (window-height) (second (assoc 'height current)) (- (* h-factor (second (assoc 'height current))) (window-height)))
			;; (message "ENLARGE WIDTH: %S %S %S %S" w-factor (window-width) (second (assoc 'width current)) (- (* w-factor (second (assoc 'width current))) (window-width)))
			(ignore-errors (enlarge-window
				(- (round (* h-factor (second (assoc 'height current)))) (window-height))))
			(ignore-errors (enlarge-window-horizontally
				(- (round (* w-factor (second (assoc 'width current)))) (window-width))))
			(my-window-unserialize current)
			(if (second (assoc 'selected current))
					(setq selected-window (selected-window)))
			(select-window (next-window)))
		(select-window selected-window)
		(select-frame selected-frame)))

(provide 'my/frame)
