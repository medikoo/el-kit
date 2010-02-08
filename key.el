;; my/key.el --- operations related to key bindings

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

(defvar my-key-defaults (make-hash-table)
	"Key binding defaults")

(defvar my-key-minibuffer-defaults (make-hash-table)
	"Key binding mini buffer defaults")

;;;###autoload
(defun my-key-set (key command)
	"Give KEY a global binding as COMMAND."
	(when (not (gethash key my-key-defaults))
		(puthash key (lookup-key global-map key) my-key-defaults)
		(puthash key (lookup-key minibuffer-local-map key) my-key-minibuffer-defaults))
	(global-set-key key command)
	(define-key minibuffer-local-map key 'undefined))

;;;###autoload
(defun my-key-remove (key)
	"Remove KEY current binding and revert to default binding."
	(when (gethash key my-key-defaults)
		(global-set-key key (gethash key my-key-defaults))
		(remhash key my-key-defaults)
		(define-key minibuffer-local-map key (gethash my-key-minibuffer-defaults))
		(remhash key my-key-minibuffer-defaults)))

;;;###autoload
(defun my-key-replace (oldkey newkey &optional command)
	"Change OLDKEY command binding to NEWKEY binding."
	(let ((command (or command (lookup-key global-map oldkey))))
		(my-key-remove oldkey)
		(my-key-set newkey command)))

(provide 'my/key)