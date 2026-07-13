;;; simple-pass-tests.el --- Tests for simple-pass  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(ert-deftest simple-pass-rejects-empty-entry ()
  "An empty selection is rejected before any external command runs."
  (should-error (simple-pass-copy "") :type 'user-error)
  (should-error (simple-pass-edit "") :type 'user-error)
  (should-error (simple-pass-autotype "") :type 'user-error)
  (should-error (simple-pass-get-otp "") :type 'user-error))

(ert-deftest simple-pass-generate-uses-argv-and-reports-failure ()
  "Generation passes entry names as argv and reports process failures."
  (let (arguments)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "weird;entry"))
              ((symbol-function 'simple-pass-entries) (lambda () nil))
              ((symbol-function 'process-file)
               (lambda (program infile destination display &rest args)
                 (setq arguments (cons program args))
                 17)))
      (should-error (simple-pass-generate) :type 'user-error)
      (should (equal '("pass" "generate" "weird;entry")
                     (butlast arguments 1)))
      (should (string-match-p "^[0-9]+$" (car (last arguments)))))))

(ert-deftest simple-pass-get-otp-uses-argv-and-trims-output ()
  "OTP retrieval does not build a shell command and trims its output."
  (let (arguments otp)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program infile destination display &rest args)
                 (setq arguments (cons program args))
                 (with-current-buffer destination
                   (insert "123456\n"))
                 0))
              ((symbol-function 'kill-new)
               (lambda (value) (setq otp value))))
      (simple-pass-get-otp "weird;entry")
      (should (equal '("pass" "otp" "show" "weird;entry") arguments))
      (should (equal "123456" otp)))))

(ert-deftest simple-pass-autotype-uses-argv ()
  "Autotype passes user and password as process arguments, not shell text."
  (let (arguments)
    (cl-letf (((symbol-function 'auth-source-pass-get)
               (lambda (field _entry)
                 (if (equal field "user") "user;name" "pass$word")))
              ((symbol-function 'start-process)
               (lambda (name buffer program &rest args)
                 (setq arguments (cons program args)))))
      (simple-pass-autotype "entry;name")
      (should (equal '("wtype" "-s" "300" "user;name" "-P" "tab" "pass$word")
                     arguments)))))

;;; simple-pass-tests.el ends here
