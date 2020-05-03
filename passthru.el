;;; passthru.el --- summary -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/wi11dey/passthru.el
;; Keywords: keywords

;; This file is not part of GNU Emacs

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

;; Generate README:
;;; Commentary:

;; commentary

;;; Code:

(defgroup passthru nil
  "")

(defface passthru-keyboard
  '((t))
  "")

(defun passthru--read-key-forward (prompt forward-func)
  (let (keys)
    (let ((overriding-terminal-local-map '(keymap (t . ignore))))
      (setq keys (read-key-sequence-vector prompt nil 'dont-downcase-last)))
    (message nil)
    (command-execute forward-func nil keys)
    keys))

(defun passthru--forward-next-key-closure (forward-func)
  ;; Closure:
  (lambda (times)
    "Forward the next TIMES keys directly to the application without being consumed by Emacs."
    (interactive "p")
    (let (key
	  (keys-description ""))
      (dotimes (i times)
        (setq key (passthru--read-key-forward (format "Forward keys (%d/%d): %s"
						      (1+ i)
						      times
						      keys-description)
					      forward-func)
	      keys-description (concat keys-description
				       (propertize (key-description key)
						   'face 'passthru-keyboard)
				       " "))))))

;; TODO Use this instead
(defun passthru--simulated-keys-lambda (forward-func keys)
  (let ((description (if (vectorp keys)
			 (key-description keys)
		       keys))
	(list (listify-key-sequence (if (stringp keys)
					(kbd keys)
				      keys)))
	body))
  `(lambda ()
     ,(format "Send the key sequence %s to the application." description)
     (interactive)
     ,@(progn
	 (dolist (key list)
	   (push `(push ,key unread-command-events) body)
	   (push `(passthru--read-key-forward nil #',forward-func) body))
	 (nreverse body))))

(defun passthru--simulated-keys-closure (forward-func keys)
  (setq keys (listify-key-sequence (if (stringp keys)
				       (kbd keys)
				     keys)))
  ;; Closure:
  (lambda ()
    "Forwarded keyboard macro. Every key used in this sequence will be forwarded, in order, to the application without being consumed by Emacs."
    (interactive)
    (dolist (key keys)
      (if (functionp key)
	  (funcall key)
	(push key unread-command-events)
	(passthru--read-key-forward nil forward-func)))))

;;;###autoload
(defun passthru (map &optional forward-func keybinds)
  (let ((fallback-map (make-sparse-keymap)))
    (if forward-func
	(define-key map [remap self-insert-command] forward-func)
      (setq forward-func #'self-insert-command))
    (define-key fallback-map [t] forward-func)
    (set-keymap-parent map
		       (make-composed-keymap (list (current-global-map)
						   fallback-map)))
    (define-key map [remap quoted-insert] (passthru--forward-next-key-closure forward-func))
    (dolist (bind keybinds)
      (let ((key   (car bind))
	    (value (cdr bind)))
	(if (commandp value)
	    (define-key map
	      (if (stringp key)
		  (kbd key)
		key)
	      (if (commandp value :for-call-interactively)
		  value
		(passthru--simulated-keys-closure forward-func value)))
	  (display-warning 'passthru (format-message "`%S' is not an acceptable keybinding value. Must be a string or vector keyboard macro, or a function value that satisfies `commandp'" nil) :error))))
    map))

(provide 'passthru)
