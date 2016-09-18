;;; package-demo.el --- scripted emacs sessions      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

;; Author: Sean Allred <sean@SMBP.local>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Package Demo allows you to demonstrate package features in a
;; predictable way.  Think of it like a glorified keyboard macro that
;; runs at believably-human speeds.
;;
;; Example:
;;
;;     (package-demo-define-demo do-something
;;       (typewriter "Hello, world!")
;;       (pause 1)
;;       (typewriter "\nI'm package-demo.el!")
;;       (M-x #'save-buffer))

;;; Code:

(require 'cl-lib)

(defvar package-demo-actions (make-hash-table))
(defvar package-demo-demos (make-hash-table))
(defvar package-demo-default-arguments
  '((typewriter :speed 20)))

(defun package-demo-get-argument (action arg plist)
  (or (plist-get plist arg)
      (plist-get (cdr (assoc action package-demo-default-arguments)) arg)))

(defmacro package-demo-define-action (action args &rest body)
  (declare (indent defun))
  (puthash action `(lambda ,args ,@body) package-demo-actions)
  package-demo-actions)

(defun package-demo-do-keys (keys)
  (message "And here's where I'd press %S..." keys))

(defun package-demo-do (action &optional args)
  (if action
      (let ((func (gethash action package-demo-actions)))
        (unless func (error "No such action: %S" action))
        (apply func args))
    (package-demo-do-keys args)))

(defun package-demo--verify-form (form)
  "Ensure all forms in FORM can be run."
  (let (e)
    (while form
      (setq e (car form))
      (when (consp e)
        (unless (gethash (car e) package-demo-actions)
          (user-error "No such action: %S" (car e))))
      (setq form (cdr form)))))

(defun package-demo--run-demo (form)
  "Run FORM."
  (let (action args)
    (while form
      (setq e (car form))
      (when (consp e)
        (setq action (car e)
              args (cdr e)))
      (package-demo-do action (if action args e))
      (setq form (cdr form)))))

(defmacro package-demo-define-demo (demo &rest body)
  "Define a function DEMO that executes BODY."
  (declare (indent defun))
  (package-demo--verify-form body)
  `(defun ,demo ()
     (interactive)
     (package-demo--run-demo ',body)))

(package-demo-define-action pause (seconds)
  (sit-for seconds))

(package-demo-define-action M-x (func &rest keys)
  ;; cannot handle prefix arguments or (interactive) forms
  (funcall (eval func)))

(package-demo-define-action insert (text)
  (insert text))

(package-demo-define-action typewriter (text &rest keys)
  (let ((speed (package-demo-get-argument 'typewriter :speed keys)))
    (while (not (string-equal "" text))
      (insert (substring text 0 1))
      (sit-for (/ 1.0 speed))
      (setq text (substring text 1)))))

(provide 'package-demo)
;;; package-demo.el ends here
