;;; simple-pass-tests.el --- Tests for simple-pass  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Provide a minimal with-editor stub when the real package is absent so
;; batch tests match the documented load-path install path.
(unless (require 'with-editor nil t)
  (defun with-editor-async-shell-command (command)
    "Test stub for with-editor when the package is not installed."
    command)
  (provide 'with-editor))

(require 'simple-pass)

(defun simple-pass-test--write-entry (directory name)
  "Create an empty pass entry NAME under DIRECTORY."
  (let ((file (expand-file-name name directory)))
    (make-directory (file-name-directory file) t)
    (let ((file-name-handler-alist nil))
      (make-empty-file file))))

(defun simple-pass-test--write-process-destination (destination stdout &optional stderr)
  "Write STDOUT/STDERR into DESTINATION from `process-file'.

DESTINATION is either a buffer or (REAL-BUFFER STDERR-FILE) as used by
`call-process'/`process-file'."
  (cond
   ((bufferp destination)
    (when stdout
      (with-current-buffer destination
        (insert stdout))))
   ((consp destination)
    (let ((stdout-dest (car destination))
          (stderr-dest (cadr destination)))
      (when (and stdout (eq stdout-dest t))
        (insert stdout))
      (when (and stdout (buffer-live-p stdout-dest))
        (with-current-buffer stdout-dest
          (insert stdout)))
      (when (and stderr (stringp stderr-dest))
        (with-temp-file stderr-dest
          (insert stderr)))))))

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
  "A missing store gets a clear user-error."
  (let ((directory (expand-file-name "simple-pass-does-not-exist" temporary-file-directory))
        (simple-pass-password-store-directory nil))
    (let ((auth-source-pass-filename directory))
      (should-error (simple-pass-entries)
                    :type 'user-error)
      (condition-case error-data
          (simple-pass-entries)
        (user-error
         (should (string-match-p "Password store does not exist"
                                 (error-message-string error-data))))))))

(ert-deftest simple-pass-entries-reports-unreadable-store ()
  "An unreadable store gets a distinct clear user-error."
  (let ((directory (make-temp-file "simple-pass-" t)))
    (unwind-protect
        (progn
          (set-file-modes directory #o000)
          (unless (file-readable-p directory)
            (let ((simple-pass-password-store-directory directory))
              (condition-case error-data
                  (simple-pass-entries)
                (user-error
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

(ert-deftest simple-pass-copy-rejects-missing-secret ()
  "A missing secret names its entry without touching the kill ring."
  (let (kill-new-called)
    (cl-letf (((symbol-function 'auth-source-pass-get)
               (lambda (_field _entry) nil))
              ((symbol-function 'kill-new)
               (lambda (_string &optional _replace)
                 (setq kill-new-called t))))
      (let ((error-data
             (should-error (simple-pass-copy "missing/account")
                           :type 'user-error)))
        (should (string-match-p "missing/account"
                                (error-message-string error-data))))
      (should-not kill-new-called))))

(ert-deftest simple-pass-generate-allows-non-existing-entry-name ()
  "Generation accepts a free-text name outside the existing entries."
  (let (prompt arguments copied)
    (cl-letf (((symbol-function 'read-string)
               (lambda (actual-prompt &rest _)
                 (setq prompt actual-prompt)
                 "new-entry"))
              ((symbol-function 'completing-read)
               (lambda (&rest _)
                 (ert-fail "Generation must use a free-text prompt")))
              ((symbol-function 'simple-pass-entries)
               (lambda () '("existing")))
              ((symbol-function 'executable-find)
               (lambda (program) program))
              ((symbol-function 'process-file)
               (lambda (program _infile _destination _display &rest args)
                 (setq arguments (cons program args))
                 0))
              ((symbol-function 'simple-pass-copy)
               (lambda (entry)
                 (setq copied entry))))
      (simple-pass-generate)
      (should (equal "New entry: " prompt))
      (should (equal '("pass" "generate" "new-entry")
                     (butlast arguments 1)))
      (should (equal "new-entry" copied)))))

(ert-deftest simple-pass-generate-uses-argv-and-reports-failure ()
  "Generation passes entry names as argv and reports process failures."
  (let (arguments)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "weird;entry"))
              ((symbol-function 'simple-pass-entries) (lambda () nil))
              ((symbol-function 'executable-find)
               (lambda (program) program))
              ((symbol-function 'process-file)
               (lambda (program _infile _destination _display &rest args)
                 (setq arguments (cons program args))
                 17)))
      (should-error (simple-pass-generate) :type 'user-error)
      (should (equal '("pass" "generate" "weird;entry")
                     (butlast arguments 1)))
      (should (string-match-p "^[0-9]+$" (car (last arguments)))))))

(ert-deftest simple-pass-generate-rejects-duplicate-entry ()
  "Generation refuses to overwrite an existing entry."
  (let (called)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "existing"))
              ((symbol-function 'simple-pass-entries)
               (lambda () '("existing")))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 (setq called t)))
              ((symbol-function 'executable-find)
               (lambda (_) "pass")))
      (should-error (simple-pass-generate) :type 'user-error)
      (should-not called))))

(ert-deftest simple-pass-generate-dispatches-copy-after-success ()
  "Successful generation copies the new entry's secret."
  (let (arguments copied)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "new-entry"))
              ((symbol-function 'simple-pass-entries)
               (lambda () nil))
              ((symbol-function 'executable-find)
               (lambda (program) program))
              ((symbol-function 'process-file)
               (lambda (program _infile _destination _display &rest args)
                 (setq arguments (cons program args))
                 0))
              ((symbol-function 'simple-pass-copy)
               (lambda (entry)
                 (setq copied entry))))
      (simple-pass-generate)
      (should (equal '("pass" "generate" "new-entry")
                     (butlast arguments 1)))
      (should (equal "new-entry" copied)))))

(ert-deftest simple-pass-generate-surfaces-stderr-on-failure ()
  "Failed generate includes stderr in the user-error, not stdout secrets."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "new-entry"))
            ((symbol-function 'simple-pass-entries) (lambda () nil))
            ((symbol-function 'executable-find)
             (lambda (_) "pass"))
            ((symbol-function 'process-file)
             (lambda (_program _infile destination _display &rest _args)
               (simple-pass-test--write-process-destination
                destination
                "The generated password for new-entry is SuperSecret123\n"
                "gpg: decryption failed\n")
               2)))
    (let ((error-data (should-error (simple-pass-generate) :type 'user-error)))
      (should (string-match-p "decryption failed"
                              (error-message-string error-data)))
      (should-not (string-match-p "SuperSecret123"
                                  (error-message-string error-data))))))

(ert-deftest simple-pass-get-otp-uses-argv-and-trims-output ()
  "OTP retrieval does not build a shell command and trims its output."
  (let (arguments otp)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program) program))
              ((symbol-function 'process-file)
               (lambda (program _infile destination _display &rest args)
                 (setq arguments (cons program args))
                 (simple-pass-test--write-process-destination
                  destination "123456\n")
                 0))
              ((symbol-function 'simple-pass--copy-to-kill-ring)
               (lambda (value) (setq otp value))))
      (simple-pass-get-otp "weird;entry")
      (should (equal '("pass" "otp" "show" "weird;entry") arguments))
      (should (equal "123456" otp)))))

(ert-deftest simple-pass-get-otp-ignores-stderr-noise-on-success ()
  "Successful OTP copies only stdout, ignoring stderr diagnostics."
  (let (otp)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "pass"))
              ((symbol-function 'process-file)
               (lambda (_program _infile destination _display &rest _args)
                 (simple-pass-test--write-process-destination
                  destination "123456\n" "gpg: warning: something\n")
                 0))
              ((symbol-function 'simple-pass--copy-to-kill-ring)
               (lambda (value) (setq otp value))))
      (simple-pass-get-otp "entry")
      (should (equal "123456" otp)))))

(ert-deftest simple-pass-get-otp-rejects-empty-output ()
  "Whitespace-only OTP output is rejected without changing the kill ring."
  (let ((kill-ring '("existing")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "pass"))
              ((symbol-function 'process-file)
               (lambda (_program _infile destination _display &rest _args)
                 (simple-pass-test--write-process-destination
                  destination " \n\t")
                 0))
              ((symbol-function 'run-at-time) #'ignore))
      (let ((error-data
             (should-error (simple-pass-get-otp "empty-otp")
                           :type 'user-error)))
        (should (string-match-p "No OTP returned"
                                (error-message-string error-data))))
      (should (equal '("existing") kill-ring)))))

(ert-deftest simple-pass-get-otp-surfaces-stderr-on-failure ()
  "Failed OTP includes stderr in the user-error."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "pass"))
            ((symbol-function 'process-file)
             (lambda (_program _infile destination _display &rest _args)
               (simple-pass-test--write-process-destination
                destination "" "Error: otp is not in the password store.\n")
               1)))
    (let ((error-data
           (should-error (simple-pass-get-otp "missing")
                         :type 'user-error)))
      (should (string-match-p "not in the password store"
                              (error-message-string error-data))))))

(ert-deftest simple-pass-copy-cleans-up-only-its-kill-ring-cell ()
  "Cleanup preserves unrelated equal-looking kill-ring entries."
  (let ((existing (copy-sequence "same-secret"))
        (unrelated (copy-sequence "same-secret"))
        timer-function timer-arguments
        (kill-ring nil)
        (simple-pass-clipboard-timeout 12)
        (interprogram-cut-function nil))
    (setq kill-ring (list existing unrelated))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest arguments)
                 (setq timer-function function
                       timer-arguments arguments))))
      (simple-pass--copy-to-kill-ring "same-secret")
      (should (= 12 simple-pass-clipboard-timeout))
      (should (eq existing (nth 1 kill-ring)))
      (should (eq unrelated (nth 2 kill-ring)))
      (should (eq (car timer-arguments) kill-ring))
      (funcall timer-function (car timer-arguments))
      (should (equal kill-ring (list existing unrelated)))
      (should-not (car (car timer-arguments))))))

(ert-deftest simple-pass-copy-schedules-each-repeated-copy-independently ()
  "Repeated copies each remove only the cell they introduced."
  (let (timers (kill-ring nil) (interprogram-cut-function nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest arguments)
                 (push (cons function arguments) timers))))
      (simple-pass--copy-to-kill-ring "first")
      (simple-pass--copy-to-kill-ring "second")
      (should (= 2 (length timers)))
      (funcall (caar (last timers)) (cadar (last timers)))
      (should (equal '("second") kill-ring))
      (funcall (caar timers) (cadar timers))
      (should-not kill-ring))))

(ert-deftest simple-pass-copy-schedules-when-duplicates-suppressed ()
  "Cleanup still runs when kill-do-not-save-duplicates suppresses kill-new."
  (let* ((cut-box (list nil))
         timers
         (kill-do-not-save-duplicates t)
         (kill-ring (list (copy-sequence "same")))
         (interprogram-cut-function
          (lambda (text &optional _push)
            (setcar cut-box (cons text (car cut-box)))))
         (simple-pass-clipboard-timeout 5))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest arguments)
                 (push (cons function arguments) timers))))
      (simple-pass--copy-to-kill-ring "same")
      (should (= 1 (length timers)))
      (should (equal '("same") kill-ring))
      (funcall (caar timers) (cadar timers))
      (should-not kill-ring)
      (should (member "" (car cut-box))))))

(ert-deftest simple-pass-copy-clears-interprogram-cut-when-head ()
  "Timeout clears system selection when the cleaned cell was head."
  (let* ((cut-box (list nil))
         timer-function timer-arguments
         (kill-ring nil)
         (interprogram-cut-function
          (lambda (text &optional _push)
            (setcar cut-box (cons text (car cut-box)))))
         (simple-pass-clipboard-timeout 3))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest arguments)
                 (setq timer-function function
                       timer-arguments arguments))))
      (simple-pass--copy-to-kill-ring "secret")
      (should (equal '("secret") kill-ring))
      (funcall timer-function (car timer-arguments))
      (should-not kill-ring)
      (should (member "" (car cut-box))))))

(ert-deftest simple-pass-copy-skips-interprogram-clear-when-not-head ()
  "Timeout does not clear system selection when another kill sits on top."
  (let* ((cut-box (list nil))
         timer-function timer-arguments
         (kill-ring nil)
         (interprogram-cut-function
          (lambda (text &optional _push)
            (setcar cut-box (cons text (car cut-box)))))
         (simple-pass-clipboard-timeout 3))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest arguments)
                 (setq timer-function function
                       timer-arguments arguments))))
      (simple-pass--copy-to-kill-ring "secret")
      (let ((owned (car timer-arguments)))
        (kill-new "later")
        (setq cut-box (list nil))
        (funcall timer-function owned)
        (should (equal '("later") kill-ring))
        (should-not (member "" (car cut-box)))))))

(ert-deftest simple-pass-autotype-rejects-missing-secret-before-start-process ()
  "Autotype rejects a missing secret before starting wtype."
  (let (called)
    (cl-letf (((symbol-function 'auth-source-pass-get)
               (lambda (field _entry)
                 (when (equal field "user") "user")))
              ((symbol-function 'call-process)
               (lambda (&rest _)
                 (setq called t)
                 0))
              ((symbol-function 'start-process)
               (lambda (&rest _)
                 (setq called t)))
              ((symbol-function 'executable-find)
               (lambda (_) "wtype")))
      (should-error (simple-pass-autotype "entry") :type 'user-error)
      (should-not called))))

(ert-deftest simple-pass-autotype-uses-argv ()
  "Autotype passes user and password as process arguments, not shell text."
  (let (arguments)
    (cl-letf (((symbol-function 'auth-source-pass-get)
               (lambda (field _entry)
                 (if (equal field "user") "user;name" "pass$word")))
              ((symbol-function 'executable-find)
               (lambda (_) "wtype"))
              ((symbol-function 'call-process)
               (lambda (program _infile _destination _display &rest args)
                 (setq arguments (cons program args))
                 0)))
      (simple-pass-autotype "entry;name")
      (should (equal '("wtype" "-s" "300" "user;name" "-P" "tab" "pass$word")
                     arguments)))))

(ert-deftest simple-pass-autotype-rejects-missing-wtype ()
  "Missing wtype is a user-error before any process starts."
  (let (called)
    (cl-letf (((symbol-function 'auth-source-pass-get)
               (lambda (field _entry)
                 (if (equal field "user") "user" "secret")))
              ((symbol-function 'executable-find)
               (lambda (_) nil))
              ((symbol-function 'call-process)
               (lambda (&rest _)
                 (setq called t)
                 0)))
      (let ((error-data
             (should-error (simple-pass-autotype "entry")
                           :type 'user-error)))
        (should (string-match-p "wtype" (error-message-string error-data))))
      (should-not called))))

(ert-deftest simple-pass-autotype-reports-wtype-failure ()
  "Nonzero wtype exit becomes a user-error."
  (cl-letf (((symbol-function 'auth-source-pass-get)
             (lambda (field _entry)
               (if (equal field "user") "user" "secret")))
            ((symbol-function 'executable-find)
             (lambda (_) "wtype"))
            ((symbol-function 'call-process)
             (lambda (&rest _) 1)))
    (should-error (simple-pass-autotype "entry") :type 'user-error)))

(ert-deftest simple-pass-autotype-is-command ()
  "Autotype is an interactive command."
  (should (commandp 'simple-pass-autotype))
  (should (interactive-form 'simple-pass-autotype)))

(ert-deftest simple-pass-edit-dispatches-quoted-entry-to-with-editor ()
  "Editing dispatches the selected entry through `with-editor'."
  (let (command)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "pass"))
              ((symbol-function 'with-editor-async-shell-command)
               (lambda (value)
                 (setq command value))))
      (simple-pass-edit "weird;entry")
      (should (equal "pass edit weird\\;entry" command)))))

(ert-deftest simple-pass-edit-rejects-missing-pass ()
  "Missing pass binary is a user-error on edit."
  (let (called)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) nil))
              ((symbol-function 'with-editor-async-shell-command)
               (lambda (&rest _)
                 (setq called t))))
      (let ((error-data
             (should-error (simple-pass-edit "entry")
                           :type 'user-error)))
        (should (string-match-p "pass" (error-message-string error-data))))
      (should-not called))))

(ert-deftest simple-pass-generate-rejects-missing-pass ()
  "Missing pass binary is a user-error on generate."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "new-entry"))
            ((symbol-function 'simple-pass-entries) (lambda () nil))
            ((symbol-function 'executable-find)
             (lambda (_) nil))
            ((symbol-function 'process-file)
             (lambda (&rest _)
               (ert-fail "process-file must not run"))))
    (let ((error-data
           (should-error (simple-pass-generate) :type 'user-error)))
      (should (string-match-p "pass" (error-message-string error-data))))))

(ert-deftest simple-pass-launcher-generate-reaches-generation-body ()
  "Launcher-selected generation accepts an entry and runs its body."
  (let (arguments copied)
    (cl-letf (((symbol-function 'simple-pass-entries) (lambda () nil))
              ((symbol-function 'executable-find)
               (lambda (program) program))
              ((symbol-function 'process-file)
               (lambda (program _infile _destination _display &rest args)
                 (setq arguments (cons program args))
                 0))
              ((symbol-function 'simple-pass-copy)
               (lambda (entry)
                 (setq copied entry))))
      (funcall (simple-pass--launcher-action "GENERATE") "new-entry")
      (should (equal '("pass" "generate" "new-entry")
                     (butlast arguments 1)))
      (should (equal "new-entry" copied)))))

(ert-deftest simple-pass-launcher-dispatches-actions ()
  "Launcher choices map to the corresponding public commands."
  (should (eq #'simple-pass-autotype (simple-pass--launcher-action "AUTO")))
  (should (eq #'simple-pass-copy (simple-pass--launcher-action "COPY PASS")))
  (should (eq #'simple-pass-generate
              (simple-pass--launcher-action "GENERATE"))))

(ert-deftest simple-pass-launcher-rejects-unknown-action ()
  "Unknown launcher actions are user-errors before funcall."
  (should-error (simple-pass--launcher-action "NOPE") :type 'user-error)
  (should-error (simple-pass--launcher-action nil) :type 'user-error))

(ert-deftest simple-pass-read-entry-requires-match ()
  "Entry prompts pass require-match to completing-read."
  (let (args)
    (cl-letf (((symbol-function 'simple-pass-entries)
               (lambda () '("alpha" "beta")))
              ((symbol-function 'completing-read)
               (lambda (&rest actual)
                 (setq args actual)
                 "alpha")))
      (should (equal "alpha" (simple-pass--read-entry "Select entry: ")))
      (should (eq t (nth 3 args))))))

(ert-deftest simple-pass-call-pass-sets-store-directory-env ()
  "pass CLI runs with PASSWORD_STORE_DIR from the configured store."
  (let ((directory (make-temp-file "simple-pass-" t))
        env-seen)
    (unwind-protect
        (let ((simple-pass-password-store-directory directory))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_) "pass"))
                    ((symbol-function 'process-file)
                     (lambda (&rest _)
                       (setq env-seen
                             (getenv "PASSWORD_STORE_DIR"))
                       0)))
            (simple-pass--call-pass "generate" "x" "20")
            (should (equal (directory-file-name
                            (expand-file-name directory))
                           env-seen))))
      (delete-directory directory t))))

(ert-deftest simple-pass-get-secret-uses-configured-store ()
  "Secret lookup binds auth-source-pass-filename to the configured store."
  (let ((directory (make-temp-file "simple-pass-" t))
        seen)
    (unwind-protect
        (let ((simple-pass-password-store-directory directory))
          (cl-letf (((symbol-function 'auth-source-pass-get)
                     (lambda (field entry)
                       (setq seen (list auth-source-pass-filename field entry))
                       "secret")))
            (should (equal "secret"
                           (simple-pass--get-secret "acct")))
            (should (equal (list (directory-file-name
                                  (expand-file-name directory))
                                 'secret "acct")
                           seen))))
      (delete-directory directory t))))

(ert-deftest simple-pass-call-pass-separates-real-process-streams ()
  "Unmocked process-file keeps stdout payload and stderr diagnostics apart."
  (let* ((bin (make-temp-file "simple-pass-bin-" t))
         (pass (expand-file-name "pass" bin))
         (exec-path (cons bin exec-path))
         (simple-pass-password-store-directory
          (make-temp-file "simple-pass-store-" t)))
    (unwind-protect
        (progn
          (with-temp-file pass
            (insert "#!/bin/sh\n")
            (insert "echo '123456'\n")
            (insert "echo 'gpg: warning noise' 1>&2\n")
            (insert "exit 0\n"))
          (set-file-modes pass #o700)
          (pcase-let ((`(,status ,stdout ,stderr)
                       (simple-pass--call-pass "otp" "show" "entry")))
            (should (equal 0 status))
            (should (equal "123456\n" stdout))
            (should (string-match-p "warning noise" stderr)))
          (with-temp-file pass
            (insert "#!/bin/sh\n")
            (insert "echo 'The generated password for x is SuperSecret123'\n")
            (insert "echo 'store boom' 1>&2\n")
            (insert "exit 2\n"))
          (set-file-modes pass #o700)
          (pcase-let ((`(,status ,stdout ,stderr)
                       (simple-pass--call-pass "generate" "x" "20")))
            (should (equal 2 status))
            (should (string-match-p "SuperSecret123" stdout))
            (should (string-match-p "store boom" stderr)))
          (let ((error-data
                 (should-error
                  (cl-letf (((symbol-function 'simple-pass-entries)
                             (lambda () nil))
                            ((symbol-function 'read-string)
                             (lambda (&rest _) "x")))
                    (simple-pass-generate))
                  :type 'user-error)))
            (should (string-match-p "store boom"
                                    (error-message-string error-data)))
            (should-not (string-match-p "SuperSecret123"
                                        (error-message-string error-data)))))
      (delete-directory bin t)
      (delete-directory simple-pass-password-store-directory t))))

(ert-deftest simple-pass-loads-with-editor ()
  "with-editor is a hard load dependency."
  (should (featurep 'with-editor))
  (should (fboundp 'with-editor-async-shell-command)))

;;; simple-pass-tests.el ends here
