;; el-kit/buffer.el --- Buffer related custom functions

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

(require 'el-kit/string nil t)

;; Copyright (C) 2008, 2009 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el
;;;###autoload
(defun el-kit-buffer-rename-file-or-buffer ()
	"Renames file or buffer (if current buffer doesn't address file)."
	(interactive)
	(if (not (buffer-file-name))
		(call-interactively 'rename-buffer)
		(let ((file (buffer-file-name)))
			(with-temp-buffer
				(set-buffer (dired-noselect file))
				(dired-do-rename)
				(kill-buffer nil))))
	nil)

(defun el-kit-buffer-process-text (regexp process-method)
	"Process string with PROCESS-METHOD. Repeat until string is not found."
	(save-excursion
		(goto-char 0)
		(let (end-point)
			(while (setq end-point (re-search-forward regexp nil t))
				(funcall process-method (match-beginning 0) end-point)))))

;;;###autoload
(defun el-kit-buffer-untabify ()
	"Replace all tabs at beginning of lines with spaces."
	(el-kit-buffer-process-text "\n *\t+[ \t]*" 'untabify))

;;;###autoload
(defun el-kit-buffer-tabify ()
	"Replace all spaces at beginning of lines with tabs."
	(el-kit-buffer-process-text "\n\t* +[ \t]*" 'tabify))

;;;###autoload
(defun el-kit-buffer-indent ()
	"Indents buffer according to current indent rules."
	(indent-region (point-min) (point-max)))

;;;###autoload
(defun el-kit-buffer-whitespace-cleanup ()
	"Clean buffer whitespace incosistencies."
	(interactive)
	(el-kit-buffer-indent)
	(if (eq indent-tabs-mode nil)
		(el-kit-buffer-untabify)
		(el-kit-buffer-tabify))
	(delete-trailing-whitespace))

;;;###autoload
(defun el-kit-buffer-print-file-name ()
	"Print file name of current buffer."
	(interactive)
	(message (buffer-file-name)))

;;;###autoload
(defun el-kit-buffer-print-indent-line-function ()
	"Print current `indent-line-function'"
	(interactive)
	(message (symbol-name indent-line-function)))

;;;###autoload
(defun el-kit-buffer-print-indent-region-function ()
	"Print current `indent-region-function'"
	(interactive)
	(message "%S" indent-region-function))

;;;###autoload
(defun el-kit-buffer-insert-lorem ()
	"Insert a lorem ipsum text."
	(interactive)
	(insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
		"eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
		"ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
		"aliquip ex ea commodo consequat. Duis aute irure dolor in "
		"reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
		"pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
		"culpa qui officia deserunt mollit anim id est laborum."))

;; Copyright (C) 2008, 2009 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el
;;;###autoload
(defun el-kit-buffer-switch-or-start (function buffer)
	"If the buffer is current, bury it, otherwise invoke the function."
	(if (equal (buffer-name (current-buffer)) buffer)
		(bury-buffer)
		(if (get-buffer buffer)
			(switch-to-buffer buffer)
			(funcall function))))

;;;###autoload
(defun el-kit-buffer-ltrim-line ()
	"Trim whitespace at beginning of line."
	(interactive)
	(save-excursion
		(beginning-of-line)
		(if (looking-at "[ \t]")
			(let ((pre (point)))
				(skip-chars-forward " \t")
				(kill-region pre (point))))))

;;;###autoload
(defun el-kit-buffer-insert-tab-space ()
	"Insert `tab-width' spaces."
	(interactive)
	(insert
		(el-kit-string-repeat " "
			(- tab-width
				(%
					(if (looking-at "[^ \t]")
						(current-column)
						(save-excursion
							(- (or (and (re-search-forward "[^ \t]" nil t) (current-column)) 1) 1)))
					tab-width)))))

(provide 'el-kit/buffer)
