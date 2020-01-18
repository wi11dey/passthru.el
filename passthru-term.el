;;; passthru-term.el --- summary -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords

;; This file is not yet part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(defun forward-input-term (&optional keybinds)
  "Returns a keymap to replace `term-raw-map'."
  (let ((map (make-sparse-keymap)))
    (funcall #'passthru
	     map
	     #'term-send-raw
	     simulation-keys)))

(provide 'forward-input-term)

;;; forward-input-term.el ends here
