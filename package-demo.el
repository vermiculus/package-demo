;;; package-demo.el --- scripted emacs sessions      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Package-Requires: ((cl-lib "0.5") (dash "2.13.0") (seq "2.16"))
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
(require 'dash)
(require 'seq)

(defvar package-demo-actions (make-hash-table))
(defvar package-demo-default-arguments
  '((typewriter :speed 20)))

(defun package-demo-get-argument (action arg plist)
  (or (plist-get plist arg)
      (plist-get (cdr (assoc action package-demo-default-arguments)) arg)))

(defmacro package-demo-define-action (action args &rest body)
  (declare (indent defun))
  (puthash action `(lambda ,args ,@body) package-demo-actions)
  package-demo-actions)

(defun package-demo-do (action &rest args)
  (when-let (func (gethash action package-demo-actions))
    (apply func args)))

(defun package-demo-do- (action &optional args)
  (apply #'package-demo-do action args))

(defun package-demo--verify-form (form)
  "Ensure all forms in FORM can be run."  ;
  (dolist (e form)
    (let ((action (car e)))
      (unless (gethash action package-demo-actions)
        (user-error "No such action: %S" action)))))

(defun package-demo--run-demo (form)
  "Run FORM."
  (dolist (e form)
    (let ((action (car e)) (args (cdr e)))
      (apply #'package-demo-do action args))))

(defun package-demo--fire-event (event)
  (execute-kbd-macro (vector event)))

(defun package-demo--fire-key (key)
  (execute-kbd-macro (kbd key)))

(defun package-demo--fire-all-keys (keys speed)
  (cl-loop for event being the elements of (kbd keys) do
           (execute-kbd-macro (vector event))
           (redisplay)
           (sit-for 0.5)))

(defmacro package-demo-define-demo (demo &rest body)
  "Define a function DEMO that executes BODY."
  (declare (indent defun))
  (package-demo--verify-form body)
  `(defun ,demo ()
     (interactive)
     (package-demo--run-demo ',body)))

(defun package-demo-action:pause (seconds &rest keys)
  (sit-for seconds)
  (when (plist-member keys :callback)
    (run-with-timer 1 nil #'package-demo--run-demo
                    (plist-get keys :callback))))

(defun package-demo-action:M-x (func &rest keys)
  ;; cannot handle prefix arguments or (interactive) forms
  (run-with-timer
   1 nil #'package-demo-action:M-x:insert-command
   func (plist-get keys :callback))
  (call-interactively #'execute-extended-command))

(defun package-demo-action:M-x:insert-command (func callback)
  (cl-loop for c being the elements of (symbol-name func) do
           (execute-kbd-macro (vector c))
           (sit-for 0.05))
  (sit-for 1)
  (when callback (run-with-timer 1 nil #'package-demo--run-demo callback))
  (execute-kbd-macro (kbd "RET")))

(dolist (e '(M-x pause))
  (puthash e (intern (concat "package-demo-action:" (symbol-name e)))
           package-demo-actions))

(package-demo-define-action kbd (key &rest keys)
  ;; cannot handle prefix arguments or (interactive) forms
  (package-demo--fire-key key))

(package-demo-define-action insert (text &rest keys)
  (insert text))

(package-demo-define-action typewriter (text &rest keys)
  (let ((speed (package-demo-get-argument 'typewriter :speed keys)))
    (seq-doseq (c text)
      (insert-char c)
      (sit-for (/ 1.0 speed)))))

(package-demo-define-action typewriter-key-events (events &rest keys)
  (package-demo--typewriter-key-events-helper
   (-flatten (mapcar #'listify-key-sequence events))
   (package-demo-get-argument 'typewriter :speed keys)))

(defun package-demo--typewriter-key-events-helper (raw-events speed)
  (when raw-events
    (package-demo--fire-event (car raw-events))
    (run-with-idle-timer
     (/ 1.0 speed) nil
     #'package-demo--typewriter-key-events-helper
     (cdr raw-events)
     speed)))

(provide 'package-demo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; package-demo.el ends here
