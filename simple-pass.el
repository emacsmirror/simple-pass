;;; simple-pass.el --- Pass Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/simple-pass/

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
;; `simple-pass-clipboard-timeout' seconds.  When the cleaned cell is still
;; at the head of the kill ring, the system selection is cleared as well
;; when `interprogram-cut-function' is available.

;;; Code:

(require 'auth-source-pass)
(require 'subr-x)
(require 'with-editor)

(defgroup simple-pass nil
  "A small Emacs front end for pass."
  :group 'external)

(defcustom simple-pass-password-store-directory nil
  "Directory containing the password-store entries.

When nil, use `auth-source-pass-filename' when that variable is
available, falling back to `~/.password-store'.  Non-nil values are
used for entry discovery, `auth-source-pass' lookups, and the
`PASSWORD_STORE_DIR' environment variable for the pass CLI."
  :type '(choice (const :tag "Use auth-source-pass" nil)
                 directory)
  :group 'simple-pass)

(defcustom simple-pass-clipboard-timeout 30
  "Seconds before a copied secret is removed from the kill ring.

The timeout applies independently to each password or OTP copy.  When
the cleaned kill-ring cell is still at the head of the kill ring and
`interprogram-cut-function' is non-nil, the system selection is
cleared as well."
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

(defun simple-pass--with-store-env (body)
  "Call BODY with store path bound for auth-source and pass CLI."
  (let* ((directory (directory-file-name
                     (simple-pass--password-store-directory)))
         (auth-source-pass-filename directory)
         (process-environment
          (cons (format "PASSWORD_STORE_DIR=%s" directory)
                process-environment)))
    (funcall body)))

(defun simple-pass--require-executable (program)
  "Signal a user error when PROGRAM is not on `exec-path'."
  (unless (executable-find program)
    (user-error "Executable not found: %s" program)))

(defun simple-pass--entry-name (file directory)
  "Return the extension-free entry name for FILE under DIRECTORY."
  (file-name-sans-extension (file-relative-name file directory)))

(defun simple-pass--require-entry (entry)
  "Return ENTRY, or signal a user error when it is empty."
  (if (and entry (not (string= entry "")))
      entry
    (user-error "No pass entry selected")))

(defun simple-pass--read-entry (prompt)
  "Read an existing pass entry with PROMPT, requiring a match."
  (simple-pass--require-entry
   (completing-read prompt (simple-pass-entries) nil t)))

(defun simple-pass--process-error (program status &optional body)
  "Signal a clear error for PROGRAM terminating with STATUS.

When BODY is non-empty, include a short snippet in the message."
  (unless (and (integerp status) (zerop status))
    (let* ((snippet (and body
                         (let ((text (string-trim body)))
                           (unless (string-empty-p text)
                             (replace-regexp-in-string
                              "[\n\r]+" " "
                              (if (> (length text) 200)
                                  (substring text 0 200)
                                text))))))
           (detail (if snippet
                       (format "%s failed (exit status %s): %s"
                               program status snippet)
                     (format "%s failed (exit status %s)"
                             program status))))
      (user-error "%s" detail))))

(defun simple-pass--call-pass (&rest args)
  "Run pass with ARGS under the configured store directory.

Return (STATUS STDOUT STDERR).  Signal a user error when the pass
executable is missing.  Stdout and stderr stay separate so success
paths never treat diagnostics as payload and failure messages never
echo secrets from stdout."
  (simple-pass--require-executable "pass")
  (simple-pass--with-store-env
   (lambda ()
     (with-temp-buffer
       (let ((stdout-buffer (current-buffer)))
         (with-temp-buffer
           (let* ((stderr-buffer (current-buffer))
                  (status (apply #'process-file "pass" nil
                                 (list stdout-buffer stderr-buffer)
                                 nil args))
                  (stdout (with-current-buffer stdout-buffer
                            (buffer-string)))
                  (stderr (buffer-string)))
             (list status stdout stderr))))))))

(defun simple-pass--cleanup-kill-ring-cell (cell)
  "Remove the exact kill-ring cons CELL and release its secret value.

When CELL is at the head of the kill ring and
`interprogram-cut-function' is non-nil, also clear the system
selection."
  (let ((was-head (eq kill-ring cell)))
    (if (eq kill-ring cell)
        (setq kill-ring (cdr cell))
      (let ((tail kill-ring))
        (while (and (consp tail) (not (eq (cdr tail) cell)))
          (setq tail (cdr tail)))
        (when (eq (cdr tail) cell)
          (setcdr tail (cdr cell)))))
    (setcar cell nil)
    (when (and was-head
               (boundp 'interprogram-cut-function)
               interprogram-cut-function)
      (ignore-errors (funcall interprogram-cut-function "")))))

(defun simple-pass--copy-to-kill-ring (secret)
  "Copy SECRET to the kill ring and schedule bounded cleanup.

Return SECRET.  Cleanup tracks the kill-ring cell that holds SECRET
after `kill-new', including when `kill-do-not-save-duplicates'
suppresses a new push."
  (let ((old-kill-ring kill-ring))
    (kill-new secret)
    (let ((cell (cond
                 ((not (eq old-kill-ring kill-ring)) kill-ring)
                 ((and (consp kill-ring)
                       (equal (car kill-ring) secret))
                  kill-ring))))
      (when cell
        (run-at-time simple-pass-clipboard-timeout nil
                     #'simple-pass--cleanup-kill-ring-cell
                     cell))))
  secret)

(defun simple-pass--get-secret (entry &optional field)
  "Return FIELD (default secret) for ENTRY under the configured store."
  (simple-pass--with-store-env
   (lambda ()
     (auth-source-pass-get (or field 'secret) entry))))

(defun simple-pass-entries ()
  "Return sorted, extension-free names of all pass entries.

Signal a user error when the configured password store is missing or
unreadable."
  (let ((directory (simple-pass--password-store-directory)))
    (cond
     ((not (file-directory-p directory))
      (user-error "Password store does not exist: %s" directory))
     ((not (file-readable-p directory))
      (user-error "Password store is not readable: %s" directory))
     (t
      (sort
       (delete-dups
        (mapcar (lambda (file) (simple-pass--entry-name file directory))
                (directory-files-recursively directory "\\.gpg\\'")))
       #'string-lessp)))))

(defun simple-pass--resolve-entry (entry prompt)
  "Return ENTRY after validation, or read one with PROMPT when nil."
  (if entry
      (simple-pass--require-entry entry)
    (simple-pass--read-entry prompt)))

(defun simple-pass-copy (&optional entry)
  "Add the secret for ENTRY to the kill ring.

Signal a user error naming ENTRY when it has no secret.  The copied
secret is removed from the kill ring after
`simple-pass-clipboard-timeout' seconds."
  (interactive)
  (let* ((entry (simple-pass--resolve-entry entry "Select entry: "))
         (pass (simple-pass--get-secret entry)))
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
        (user-error "Entry already exists")
      (pcase-let ((`(,status ,_stdout ,stderr)
                   (simple-pass--call-pass
                    "generate" entry (number-to-string length))))
        (simple-pass--process-error "pass generate" status stderr)
        (simple-pass-copy entry)))))

(defun simple-pass-edit (&optional entry)
  "Edit pass ENTRY."
  (interactive)
  (let ((entry (simple-pass--resolve-entry entry "Select entry: ")))
    (simple-pass--require-executable "pass")
    (simple-pass--with-store-env
     (lambda ()
       (with-editor-async-shell-command
        (format "pass edit %s" (shell-quote-argument entry)))))))

(defun simple-pass-autotype (&optional entry)
  "Autotype password ENTRY.

Signal a user error when ENTRY has no secret, when wtype is missing,
or when wtype exits nonzero."
  (interactive)
  (let* ((entry (simple-pass--resolve-entry entry "Select entry: "))
         (user (or (simple-pass--get-secret entry "user")
                   (file-name-base entry)))
         (pass (simple-pass--get-secret entry)))
    (unless pass
      (user-error "No password found for %s" entry))
    (simple-pass--require-executable "wtype")
    (simple-pass--process-error
     "wtype"
     (call-process "wtype" nil nil nil
                   "-s" "300" user "-P" "tab" pass))))

(defun simple-pass-get-otp (&optional entry)
  "Get OTP for ENTRY."
  (interactive)
  (let ((entry (simple-pass--resolve-entry entry "Select otp entry: ")))
    (pcase-let ((`(,status ,stdout ,stderr)
                 (simple-pass--call-pass "otp" "show" entry)))
      (simple-pass--process-error "pass otp show" status stderr)
      (let ((otp (string-trim stdout)))
        (if (string-empty-p otp)
            (user-error "No OTP returned for %s" entry)
          (simple-pass--copy-to-kill-ring otp))))))
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
  "Return the command corresponding to launcher CHOICE.

Signal a user error for unknown CHOICE."
  (pcase choice
    ("AUTO" #'simple-pass-autotype)
    ("COPY PASS" #'simple-pass-copy)
    ("GENERATE" #'simple-pass-generate)
    (_ (user-error "Unknown launcher action: %s" choice))))

(defun simple-pass-launcher ()
  "Launch an Emacs frame as a front-end for pass."
  (interactive)
  (simple-pass-make-frame "emacs-float"
    (let* ((choice (completing-read "Choose an action: "
                                    '("AUTO" "COPY PASS" "GENERATE")
                                    nil t))
           (action (simple-pass--launcher-action choice)))
      (if (eq action #'simple-pass-generate)
          (funcall action)
        (funcall action (simple-pass--read-entry "Search: "))))))

(provide 'simple-pass)
;;; simple-pass.el ends here
