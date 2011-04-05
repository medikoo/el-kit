;; el-kit/xml.el --- Custom xml-mode related functions

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
(defun el-kit-xml-replace-attributes (element attribute newvalue
		&optional oldvalue)
	"Replaces all ELEMENTs ATTRIBUTE OLDVALUEs with NEWVALUE.
	ELEMENT, ATTRIBUTE and OLDVALUE are regexps.
	OLDVALUE is optional. If not given then all all found attributes have text
	NEWVALUE or OLDVALUE can be:
	nil - (only for NEWVALUE) remove ATTRIBUTE
	any string (including empty) - attribute value."
	(save-excursion
		(while (re-search-forward
				(concat "<" element "\\([^> \t\n]+\\|[ \t\n]\\(" attribute "\\)=\"\\("
					(or oldvalue "[^\"]*") "\\)\"\\|[ \t\n]\\)+") nil t)
			(when (match-string 3)
				(if newvalue
					(progn
						(goto-char (match-beginning 3))
						(if (eq (match-beginning 3) (match-end 3))
							(insert newvalue)
							(re-search-forward "[^\"]+" nil t)
							(replace-match newvalue nil nil)))
					(goto-char (- (match-beginning 2) 1))
					(delete-char (+
							1
							(length (buffer-substring-no-properties
									(match-beginning 2) (match-end 2)))
							2
							(length (buffer-substring-no-properties
									(match-beginning 3) (match-end 3)))
							1)))))))

(provide 'el-kit/xml)
