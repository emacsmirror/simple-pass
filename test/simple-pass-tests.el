;;; simple-pass-tests.el --- Tests for simple-pass  -*- lexical-binding: t; -*-

(require 'ert)
(require 'simple-pass)

(defun simple-pass-test--write-entry (directory name)
  "Create an empty pass entry NAME under DIRECTORY."
  (let ((file (expand-file-name name directory)))
    (make-directory (file-name-directory file) t)
    (let ((file-name-handler-alist nil))
      (make-empty-file file))))

(ert-deftest simple-pass-entries-discovers-normalized-sorted-unique-entries ()
  "Entries are recursive, extension-free, deterministic, and unique."
  (let ((directory (make-temp-file "simple-pass-" t)))
    (unwind-protect
        (progn
          (simple-pass-test--write-entry directory "zeta.gpg")
          (simple-pass-test--write-entry directory "nested/alpha.gpg")
          (simple-pass-test--write-entry directory "nested/alpha.txt")
          (simple-pass-test--write-entry directory "nested/omega.gpg.gpg")
          (let ((simple-pass-password-store-directory directory))
            (should
             (equal '("nested/alpha" "nested/omega.gpg" "zeta")
                    (simple-pass-entries)))))
      (delete-directory directory t))))

(ert-deftest simple-pass-entries-uses-auth-source-pass-directory ()
  "Nil custom directory honors `auth-source-pass-filename'."
  (let ((directory (make-temp-file "simple-pass-" t))
        (simple-pass-password-store-directory nil)
        (auth-source-pass-filename nil))
    (unwind-protect
        (progn
          (simple-pass-test--write-entry directory "account.gpg")
          (setq auth-source-pass-filename directory)
          (should (equal '("account") (simple-pass-entries))))
      (delete-directory directory t))))

(ert-deftest simple-pass-entries-reports-missing-store ()
  "A missing store gets a clear error."
  (let ((directory (expand-file-name "simple-pass-does-not-exist" temporary-file-directory))
        (simple-pass-password-store-directory nil))
    (let ((auth-source-pass-filename directory))
      (should-error (simple-pass-entries)
                    :type 'error)
      (condition-case error-data
          (simple-pass-entries)
        (error
         (should (string-match-p "Password store does not exist"
                                 (error-message-string error-data))))))))

(ert-deftest simple-pass-entries-reports-unreadable-store ()
  "An unreadable store gets a distinct clear error."
  (let ((directory (make-temp-file "simple-pass-" t)))
    (unwind-protect
        (progn
          (set-file-modes directory #o000)
          (unless (file-readable-p directory)
            (let ((simple-pass-password-store-directory directory))
              (condition-case error-data
                  (simple-pass-entries)
                (error
                 (should (string-match-p "Password store is not readable"
                                         (error-message-string error-data))))))))
      (set-file-modes directory #o700)
      (delete-directory directory t))))

;;; simple-pass-tests.el ends here
