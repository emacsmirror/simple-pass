;;; simple-pass.el --- Pass Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://codeberg.org/ThanosApollo/simple-pass

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (with-editor "2.5.0"))

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

;; simple-pass provides a small interactive front end for the `pass'
;; password-store utility.  It discovers entries through Emacs file APIs,
;; retrieves secrets through `auth-source-pass', and offers commands for
;; copying passwords, retrieving OTPs, editing entries, and autotyping them.
;; Copied secrets are removed from the kill ring after
;; `simple-pass-clipboard-timeout' seconds.

;;; Code:

(require 'auth-source-pass)
(require 'subr-x)

(declare-function with-editor-async-shell-command "with-editor" (command))

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

(defcustom simple-pass-clipboard-timeout 30
  "Seconds before a copied secret is removed from the kill ring.

The timeout applies independently to each password or OTP copy."
  :type 'number
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

(defun simple-pass--require-entry (entry)
  "Return ENTRY, or signal a user error when it is empty."
  (if (and entry (not (string= entry "")))
      entry
    (user-error "No pass entry selected")))

(defun simple-pass--process-error (program status)
  "Signal a clear error for PROGRAM terminating with STATUS."
  (unless (and (integerp status) (zerop status))
    (user-error "%s failed (exit status %s)" program status)))

(defun simple-pass--cleanup-kill-ring-cell (cell)
  "Remove the exact kill-ring cons CELL and release its secret value."
  (if (eq kill-ring cell)
      (setq kill-ring (cdr cell))
    (let ((tail kill-ring))
      (while (and (consp tail) (not (eq (cdr tail) cell)))
        (setq tail (cdr tail)))
      (when (eq (cdr tail) cell)
        (setcdr tail (cdr cell)))))
  (setcar cell nil))

(defun simple-pass--copy-to-kill-ring (secret)
  "Copy SECRET to the kill ring and schedule bounded cleanup.

Return SECRET.  Cleanup tracks the exact kill-ring cell introduced by
`kill-new', rather than deleting equal-looking entries later."
  (let ((old-kill-ring kill-ring))
    (kill-new secret)
    (unless (eq old-kill-ring kill-ring)
      (run-at-time simple-pass-clipboard-timeout nil
                   #'simple-pass--cleanup-kill-ring-cell
                   kill-ring)))
  secret)

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
  "Add the secret for ENTRY to the kill ring.

Signal a user error naming ENTRY when it has no secret.  The copied secret
will be deleted from the kill ring after
`simple-pass-clipboard-timeout' seconds."
  (interactive)
  (let* ((entry (simple-pass--require-entry
                 (or entry (completing-read "Select entry: " (simple-pass-entries)))))
         (pass (auth-source-pass-get 'secret entry)))
    (unless pass
      (user-error "No secret found for pass entry: %s" entry))
    (simple-pass--copy-to-kill-ring pass)))

(defun simple-pass-generate (&optional entry)
  "Generate new pass ENTRY.

When ENTRY is nil, prompt for the new entry name."
  (interactive)
  (let* ((entries (simple-pass-entries))
         (entry (simple-pass--require-entry
                 (or entry (read-string "New entry: "))))
         (length (+ 14 (random 25))))
    (if (member entry entries)
        (error "Entry already exists")
      (simple-pass--process-error
       "pass generate"
       (process-file "pass" nil nil nil "generate" entry
                     (number-to-string length)))
      (simple-pass-copy entry))))

(defun simple-pass-edit (&optional entry)
  "Edit pass ENTRY."
  (interactive)
  (let ((entry (simple-pass--require-entry
                (or entry (completing-read "Select entry: " (simple-pass-entries))))))
    (with-editor-async-shell-command
     (format "pass edit %s" (shell-quote-argument entry)))))

(defun simple-pass-autotype (&optional entry)
  "Autotype password ENTRY.
Signal a user error when ENTRY has no secret."
  (let* ((entry (simple-pass--require-entry
                 (or entry (completing-read "Select entry: " (simple-pass-entries)))))
	 (user (or (auth-source-pass-get "user" entry) (file-name-base entry)))
	 (pass (auth-source-pass-get 'secret entry)))
    (unless pass
      (user-error "No password found for %s" entry))
    (start-process "wtype" nil "wtype" "-s" "300" user "-P" "tab" pass)))

(defun simple-pass-get-otp (&optional entry)
  "Get OTP for ENTRY."
  (interactive)
  (let* ((entry (simple-pass--require-entry
                 (or entry (completing-read "Select otp entry: " (simple-pass-entries)))))
         (buffer (generate-new-buffer " *simple-pass-otp*")))
    (unwind-protect
        (progn
          (simple-pass--process-error
           "pass otp show"
           (process-file "pass" nil buffer nil "otp" "show" entry))
          (with-current-buffer buffer
            (let ((otp (string-trim (buffer-string))))
              (if (string-empty-p otp)
                  (user-error "No OTP returned for %s" entry)
                (simple-pass--copy-to-kill-ring otp)))))
      (kill-buffer buffer))))

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

(defun simple-pass--launcher-action (choice)
  "Return the command corresponding to launcher CHOICE."
  (pcase choice
    ("AUTO" #'simple-pass-autotype)
    ("COPY PASS" #'simple-pass-copy)
    ("GENERATE" #'simple-pass-generate)))

(defun simple-pass-launcher ()
  "Launch an Emacs frame as a front-end for pass."
  (interactive)
  (simple-pass-make-frame "emacs-float"
    (let* ((choice (completing-read "Choose an action: "
				    '("AUTO" "COPY PASS" "GENERATE")))
	   (action (simple-pass--launcher-action choice)))
      (if (eq action #'simple-pass-generate)
          (funcall action)
        (funcall action (completing-read "Search: " (simple-pass-entries)))))))

(provide 'simple-pass)
;;; simple-pass.el ends here
