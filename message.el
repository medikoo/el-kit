;; my/message.el --- general message helpers

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
(defun my-message-comment-all ()
	"Comment all lines that starts with `message'."
	(interactive)
	(save-excursion
		(goto-char 0)
		(while (re-search-forward "\n[ \t]*\\(\(message\\)" nil t)
			(goto-char (match-beginning 1))
			(insert ";; "))))

;;;###autoload
(defun my-message-uncomment-all ()
	"Uncomment all lines that starts with `message'."
	(interactive)
	(save-excursion
		(goto-char 0)
		(while (re-search-forward "\n[ \t]*\\(;+[ \t]*\\)\(message" nil t)
			(goto-char (match-beginning 1))
			(delete-char (length (match-string 1))))))

(provide 'my/message)