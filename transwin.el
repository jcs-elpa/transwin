;;; transwin.el --- Make window/frame transparent  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-25 01:42:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Make window/frame transparent.
;; Keyword: window transparent frame
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/transwin

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Make window/frame transparent.
;;

;;; Code:


(defvar jcs-current-frame-transparency 100
  "Current active frame transparency.")

(defvar jcs-record-toggle-frame-transparency 85
  "Record toggle frame transparency.")

(defvar jcs-default-delta-transparency 5
  "Delta increament/decreament transparency value.")


;;;###autoload
(defun jcs-ask-set-transparency (alpha-level)
  "Set the frame transparency by ALPHA-LEVEL."
  (interactive "p")
  ;; SOURCE: https://gist.github.com/benzap/89759928060f4578c063
  (let ((alpha-level (if (< alpha-level 2)
                         (read-number "Opacity percentage: " jcs-record-toggle-frame-transparency)
                       alpha-level)))
    (jcs-set-transparency alpha-level)))

(defun jcs-set-transparency (alpha-level)
  "Set the frame transparency by ALPHA-LEVEL."
  (set-frame-parameter nil 'alpha alpha-level)
  (message (format "[INFO] Frame alpha level is %d" (frame-parameter nil 'alpha)))
  (setq jcs-current-frame-transparency alpha-level)
  (unless (= alpha-level 100)
    (setq jcs-record-toggle-frame-transparency alpha-level)))

;;;###autoload
(defun jcs-toggle-transparent-frame ()
  "Toggle frame's transparency between `recorded'% and 100%."
  (interactive)
  (if (= jcs-current-frame-transparency 100)
      (jcs-set-transparency jcs-record-toggle-frame-transparency)
    (jcs-set-transparency 100)))


(defun jcs-delta-frame-transparent (del-trans)
  "Delta changes the frame transparency by a certain percentage, DEL-TRANS."
  (let ((alpha (frame-parameter nil 'alpha))
        (current-transparency jcs-default-delta-transparency))

    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    (setq current-transparency (+ current-transparency del-trans))
    (setq current-transparency (jcs-clamp-integer current-transparency 5 100))

    ;; Apply the value to frame.
    (jcs-set-transparency current-transparency)))

;;;###autoload
(defun jcs-increment-frame-transparent (&optional del-trans)
  "Increment the frame transparency by a certain percentage, DEL-TRANS."
  (interactive)
  (unless del-trans
    (setq del-trans (jcs-to-positive jcs-default-delta-transparency)))
  (jcs-delta-frame-transparent del-trans))

;;;###autoload
(defun jcs-decrement-frame-transparent (&optional del-trans)
  "Decrement the frame transparency by a certain percentage, DEL-TRANS."
  (interactive)
  (unless del-trans
    (setq del-trans (jcs-to-negative jcs-default-delta-transparency)))
  (jcs-delta-frame-transparent del-trans))


(provide 'transwin)
;;; transwin.el ends here
