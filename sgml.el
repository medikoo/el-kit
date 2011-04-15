;; el-kit/sgml.el --- Custom sgml-mode related functions

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
(defun el-kit-sgml-replace-attributes (element attribute newvalue
		&optional oldvalue)
	"Replaces all ELEMENTs ATTRIBUTE OLDVALUEs with NEWVALUE.
	ELEMENT, ATTRIBUTE and OLDVALUE are regexps.
	OLDVALUE is optional. If not given then all found attributes are replaced.
	NEWVALUE or OLDVALUE can be:
	nil - (only for NEWVALUE) remove ATTRIBUTE
	0 - attribute without value (e.g. <input disabled />).
	any string (including empty) - attribute value."
	(let (attrname wrapper value endpoint)
		(save-excursion
			(while (re-search-forward
					(concat "<" element
						"[ \t\n]+\\(\\(" attribute "\\)[ \t\n=]\\|[^> \t\n]+\\|[ \t\n]+\\)+") nil t)
				(when (match-string 2)
					(setq attrname (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
					(goto-char (match-end 2))
					(save-excursion
						(if (looking-at "=")
							(progn
								(forward-char 1)
								(setq wrapper
									(if (looking-at "\"")
										(progn
											(re-search-forward "\"\\([^\"]*\\)\"")
											"\"")
										(if (looking-at "'")
											(progn
												(re-search-forward "'\\([^']*\\)'")
												"'")
											(re-search-forward "\\([^ \t\n\"']*\\)")
											"")))
								(setq value (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
							(setq wrapper "\"")
							(setq value 0))
						(setq endpoint (point)))
					(when (or (not oldvalue)
							(string-match (concat "^" oldvalue "$") value))
						(delete-char (- endpoint (point)))
						(cond
							((eq newvalue nil)
								(delete-char (- (+ 1 (length attrname)))))
							((eq newvalue 0)
								nil)
							(t
								(if (and (eq wrapper "")
										(not (string-match "^[a-zA-Z0-9-._:]*$" newvalue)))
									(setq wrapper "\""))
								(insert "=" wrapper newvalue wrapper)))))))))

(provide 'el-kit/sgml)
