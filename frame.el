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
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

(require 'my/window)
(require 'my/edges)

(defun my-frame-serialize-split (split)
	"Replaces window objects with window serialization.
	Function is firstly invoked by `my-frame-serialize'.
	SPLIT is split information output by `window-tree'."
	(if (windowp split)
		(my-window-serialize split)
		(let ((index 2) (length (length split)))
			(while (< index length)
				(setf (nth index split) (my-frame-serialize-split (nth index split)))
				(setq index (+ index 1)))
			split)))

(defun my-frame-serialize (&optional frame)
	"Serializes FRAME windows configuration to string.
	It's helpful when we want to save configuration to file.
	If FRAME is nil then current frame is serialized."
	(list
		(list 'left (frame-parameter frame 'left))
		(list 'top (frame-parameter frame 'top))
		(list 'height (frame-parameter frame 'height))
		(list 'width (frame-parameter frame 'width))
		(list 'tree (my-frame-serialize-split (car (window-tree frame))))))

(defun my-frame-unserialize-split (split &optional window)
	"Unserializes SPLIT into given WINDOW.
	Function is firstly invoked by `my-frame-unserialize'."
	(if (not window)
		(setq window (selected-window)))
	(let ((queue (list t)) (index 3) (length (length split))
			(edges (window-edges window)))
		(if (or (not (car split)) (eq t (car split)))
			(progn
				(ignore-errors (enlarge-window
						(- (my-edges-height (second split))
							(my-edges-height edges))))
				(ignore-errors (enlarge-window-horizontally
						(- (my-edges-width (second split))
							(my-edges-width edges))))
				(nconc queue (list (list (third split) window)))
				(while (< index length)
					(setq window (split-window window nil (not (car split))))
					(nconc queue (list (list (nth index split) window)))
					(setq index (+ index 1)))
				(if (> (length queue) 1)
					(progn
						(pop queue)
						(dolist (data queue)
							(select-window (second data))
							(my-frame-unserialize-split (car data) (second data))))))
			(ignore-errors (enlarge-window
					(- (my-edges-height (second (assoc 'edges split)))
						(my-edges-height edges))))
			(ignore-errors (enlarge-window-horizontally
					(- (my-edges-width (second (assoc 'edges split)))
						(my-edges-width edges))))
			(my-window-unserialize split window)
			(if (second (assoc 'selected split))
				(select-window window)))))

(defun my-frame-unserialize (data &optional frame)
	"Unserializes configuration saved by `my-frame-serialize' into FRAME.
	DATA is configuration string. If FRAME is nil then current frame is taken."
	(if (not frame)
		(setq frame (window-frame (selected-window))))

	(set-frame-width frame (second (assoc 'width data)))
	(set-frame-height frame (second (assoc 'height data)))
	(let* (
			(window (frame-first-window frame))
			(selected-frame (window-frame (selected-window))))
		(select-frame frame)
		(delete-other-windows window)
		(my-frame-unserialize-split (second (assoc 'tree data)) window)
		(select-frame selected-frame)))

(defun my-frame-short-layout-info (&optional frame)
	"Returns string with short FRAME layout info (concancenated edges).
	Useful in debugging."
	(if (not frame)
		(setq frame (window-frame (selected-window))))
	(let* (
			(first-window (frame-first-window frame))
			(window first-window)
			(edges (window-edges window))
			(data (concat
					(number-to-string (car edges)) "-"
					(number-to-string (second edges)) "-"
					(number-to-string (third edges)) "-"
					(number-to-string (fourth edges)))))
		(while (not (eq (setq window (next-window window)) first-window))
			(setq edges (window-edges window))
			(setq data (concat data "|"
					(number-to-string (car edges)) "-"
					(number-to-string (second edges)) "-"
					(number-to-string (third edges)) "-"
					(number-to-string (fourth edges)))))
		data))

(defun my-frame-reasonable-split (&optional frame)
	"Split FRAME into reasonable number of windows.
	Reasonable is that each window has at least 90 characters in width."
	(if (not frame)
		(setq frame (window-frame (selected-window))))
	(let* (
			(window (frame-first-window frame))
			(tree (car (window-tree frame)))
			(to-split (- (floor (/ (frame-parameter frame 'width) 90))
					(if (or (not (listp tree)) (car tree))
						1
						(- (length tree) 2)))))
		(while (> to-split 0)
			(split-window window nil t)
			(setq to-split (- to-split 1)))
		(balance-windows frame)))

(provide 'my/frame)
