;;; simple-pass.el --- Pass Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL:

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2"))

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

;; Under Development

;;; Code:

(require 'auth-source-pass)

(defgroup simple-pass nil
  "A small Emacs front end for pass."
  :group 'external)

(defcustom simple-pass-password-store-directory nil
  "Directory containing the password-store entries.

When nil, use `auth-source-pass-filename' when that variable is
available, falling back to `~/.password-store'."
  :type '(choice (const :tag "Use auth-source-pass" nil)
                 directory)
  :group 'simple-pass)

(defun simple-pass--password-store-directory ()
  "Return the expanded password-store directory name."
  (file-name-as-directory
   (expand-file-name
    (or simple-pass-password-store-directory
        (and (boundp 'auth-source-pass-filename)
             auth-source-pass-filename)
        "~/.password-store"))))

(defun simple-pass--entry-name (file directory)
  "Return the extension-free entry name for FILE under DIRECTORY."
  (file-name-sans-extension (file-relative-name file directory)))

(defun simple-pass-entries ()
  "Return sorted, extension-free names of all pass entries.

Signal a distinct error when the configured password store is missing or
unreadable."
  (let ((directory (simple-pass--password-store-directory)))
    (cond
     ((not (file-directory-p directory))
      (error "Password store does not exist: %s" directory))
     ((not (file-readable-p directory))
      (error "Password store is not readable: %s" directory))
     (t
      (sort
       (delete-dups
        (mapcar (lambda (file) (simple-pass--entry-name file directory))
                (directory-files-recursively directory "\\.gpg\\'")))
       #'string-lessp)))))

(defun simple-pass-copy (&optional entry)
  "Add ENTRY to the kill ring.

Entry will be deleted from the kill ring within 30 seconds."
  (interactive)
  (let* ((entry (or entry (completing-read "Select entry: " (simple-pass-entries))))
	 (pass (auth-source-pass-get 'secret entry)))
    (kill-new pass)
    (run-at-time 30 nil (lambda () (setq kill-ring (delete pass kill-ring))))))

(defun simple-pass-generate ()
  "Generate new entry."
  (interactive)
  (let* ((entry (completing-read "New entry: " (simple-pass-entries))))
    (if (member entry (simple-pass-entries))
	(error "Entry already exists")
      (shell-command (format "pass generate %s %d" entry (+ 14 (random 25))))
      (simple-pass-copy entry))))

(defun simple-pass-edit (&optional entry)
  "Edit pass ENTRY."
  (interactive)
  (let ((entry (or entry (completing-read "Select entry: " (simple-pass-entries)))))
    (with-editor-async-shell-command (format "pass edit %s" entry))))

(defun simple-pass-autotype (&optional entry)
  "Autotype password ENTRY."
  (let* ((entry (or entry (completing-read "Select entry: " (simple-pass-entries))))
	 (user (or (auth-source-pass-get "user" entry) (file-name-base entry)))
	 (pass (auth-source-pass-get 'secret entry)))
    (start-process-shell-command
     "wtype" nil
     (format "wtype -s 300 %s -P tab %s"
	     (shell-quote-argument user)
	     (shell-quote-argument pass)))))

(defun simple-pass-get-otp (&optional entry)
  "Get OTP for ENTRY."
  (interactive)
  (let* ((entry (or entry (completing-read "Select otp entry: " (simple-pass-entries)))))
    (kill-new (shell-command-to-string (format "pass otp show %s" entry)))))

(defmacro simple-pass-make-frame (name &rest body)
  "Execute BODY in a temporary frame named NAME.
Frame is automatically deleted after BODY execution."
  (declare (indent 1))
  `(let ((frame (make-frame '((name . ,name)
                              (fullscreen . 0)
                              (undecorated . t)
                              (minibuffer . only)
                              (width . 70)
                              (height . 30)))))
     (unwind-protect
         (with-selected-frame frame
           ,@body)
       (delete-frame frame))))

(defun simple-pass-launcher ()
  "Launch an Emacs frame as a front-end for pass."
  (interactive)
  (simple-pass-make-frame "emacs-float"
    (let* ((choice (completing-read "Choose an action: "
				    '("AUTO" "COPY PASS" "GENERATE")))
	   (action (pcase choice
		     ("AUTO" #'simple-pass-autotype)
		     ("COPY PASS" #'simple-pass-copy)
		     ("GENERATE" #'simple-pass-generate))))
      (funcall action (completing-read "Search: " (simple-pass-entries))))))

(provide 'simple-pass)
;;; simple-pass.el ends here
