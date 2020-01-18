;;; passthru-exwm.el --- Improved input system for EXWM -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ((passthru "1.0.0"))
;; Homepage: https://github.com/wi11dey/passthru.el
;; Keywords: keywords

;; This file is not part of GNU Emacs, nor a part of EXWM (yet).

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

(require 'passthru)
(require 'exwm-input)

(defun passthru-exwm-forward ()
  "Forwards this key to the EXWM window."
  (interactive)
  (exwm-input--fake-key (aref (this-command-keys-vector) 0)))

;;;###autoload
(defun passthru-exwm (&optional simulation-keys)
  "Returns a keymap to replace `exwm-mode-map'.
Recommended to also call `passthru-exwm-passthrough-all' to ensure Emacs keymaps receive and handle the event before sending it to windows."
  (let ((map (make-sparse-keymap)))
    (funcall #'passthru
	     map
	     #'passthru-exwm-forward
	     simulation-keys)))


;;; Passthrough all events to Emacs

;; (defun passthru-exwm--ButtonPress-override (buffer button-event)
;;   (with-current-buffer buffer
;;     (let ((read-event (exwm-input--mimic-read-event button-event)))
;;       (exwm--log "%s" read-event)
;;       (if (and read-event
;; 	       (or exwm-input--during-command
;; 		   ;; Forward the event when there is an incomplete key sequence or when the minibuffer is active:
;; 		   exwm-input--line-mode-cache
;; 		   (eq (active-minibuffer-window) (selected-window))
;; 		   (lookup-key (current-local-map) (vector read-event))))
;;           ;; Forward event to Emacs:
;;           (progn
;;             (exwm-input--cache-event read-event)
;;             (exwm-input--unread-event button-event)
;;             xcb:Allow:SyncPointer)
;;         ;; Replay event:
;;         xcb:Allow:ReplayPointer))))

(defun passthru-exwm--ButtonPress-override (_buffer _button-event)
  xcb:Allow:ReplayPointer)

;;;###autoload
(define-minor-mode passthru-exwm-all-mode
  ""
  :global t
  :lighter " EXWM>"
  (setq exwm-input-line-mode-passthrough nil)
  (advice-remove 'exwm-input--on-ButtonPress-line-mode
		 #'passthru-exwm--ButtonPress-override)
  (when passthru-exwm-all-mode
    (setq exwm-input-line-mode-passthrough t)
    (advice-add 'exwm-input--on-ButtonPress-line-mode :override
		#'passthru-exwm--ButtonPress-override)))

(provide 'passthru-exwm)
