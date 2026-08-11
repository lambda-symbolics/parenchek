(defpackage #:parenchek/tests
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:parenchek
                #:*lisp-source-balance-warning-character-limit*
                #:*lisp-source-balance-warning-issue-limit*
                #:*lisp-paren-check-after-directory-open-function*
                #:*lisp-paren-check-after-first-read-function*
                #:*lisp-paren-check-file-character-limit*
                #:*lisp-paren-check-file-limit*
                #:*lisp-paren-check-issue-limit-per-file*
                #:*lisp-paren-check-result-character-limit*
                #:*lisp-paren-check-total-character-limit*
                #:lisp-paren-check-error
                #:lisp-paren-check-error-message
                #:lisp-paren-check-path
                #:lisp-source-balance-issue-delimiter
                #:lisp-source-balance-issue-expected-character
                #:lisp-source-balance-issue-kind
                #:lisp-source-balance-issue-opener
                #:lisp-source-balance-issues
                #:lisp-source-balance-language
                #:lisp-source-balance-warning
                #:lisp-source-delimiter-character
                #:lisp-source-delimiter-column
                #:lisp-source-delimiter-line
                #:lisp-source-edit-result-content))

(in-package #:parenchek/tests)

(defvar *test-count* 0
  "The number of assertions executed by the current test run.")

(defun test-assert (condition description)
  "Count and require CONDITION, reporting DESCRIPTION on failure."
  (incf *test-count*)
  (unless condition
    (error "Test failed: ~A" description))
  t)

(defstruct (tool-result
             (:constructor tool-result-create (&key success-p content)))
  "One test-facing result from the public path checker."
  (success-p nil :type boolean)
  (content "" :type string))

(defvar *test-readable-roots* nil
  "Canonical read authority passed through the public checker in tests.")

(defun tests--temporary-directory ()
  "Create and return one unique temporary directory."
  (let ((directory
          (merge-pathnames
           (format nil "parenchek-tests-~D-~D/"
                   (get-universal-time)
                   (random most-positive-fixnum))
           (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "placeholder" directory))
    directory))

;;;; -- Lisp Source Balance Tests --

(-> lisp-source-balance-tests--balanced-p (pathname string) boolean)
(defun lisp-source-balance-tests--balanced-p (path content)
  "Return true when recognized PATH CONTENT has no delimiter issues."
  (null (lisp-source-balance-issues path content)))

(-> test-lisp-source-balance () null)
(defun test-lisp-source-balance ()
  "Test language-aware Lisp-family delimiter analysis and advisory warnings."
  (dolist (case
           '(("sample.ASD" :common-lisp)
             ("sample.cl" :common-lisp)
             ("sample.lisp" :common-lisp)
             ("sample.lsp" :common-lisp)
             ("sample.ros" :common-lisp)
             ("sample.sch" :scheme)
             ("sample.scheme" :scheme)
             ("sample.scm" :scheme)
             ("sample.sld" :scheme)
             ("sample.sls" :scheme)
             ("sample.ss" :scheme)
             ("sample.bb" :clojure)
             ("sample.clj" :clojure)
             ("sample.cljc" :clojure)
             ("sample.cljs" :clojure)
             ("sample.edn" :clojure)
             ("sample.txt" nil)))
    (destructuring-bind (name expected) case
      (test-assert
        (eq (lisp-source-balance-language (pathname name)) expected)
        (format nil "Lisp source language classification recognizes ~A" name))))
  (let ((backslash #\\))
    (dolist
        (case
         `(("Common Lisp strings, characters, symbols, and nested comments"
            ,#P"balanced.lisp"
            ,(format nil
                     "#| ( #| ) |# ) |#~%(list ~S #~C( #~C) '|(|) ; ( ignored~%"
                     ")"
                     backslash
                     backslash))
           ("Scheme strings, characters, escaped identifiers, brackets, datum comments, and block comments"
            ,#P"balanced.scm"
            ,(format nil
                     "#| ( #| ] |# ) |#~%(define value [list #~C( ~S |[|])~%#;(discarded (form))~%(define escaped foo~Cx28;)"
                     backslash
                     ")"
                     backslash))
           ("Clojure strings, regexes, characters, vectors, maps, and sets"
            ,#P"balanced.clj"
            ,(format nil
                     "{:character ~C( :named ~Cnewline :text ~S :items [1 2] :regex #~S :set #{3 4}} ; ]})"
                     backslash
                     backslash
                     "]"
                     "[(]"))))
      (destructuring-bind (label path content) case
        (test-assert
         (lisp-source-balance-tests--balanced-p path content)
         label))))
    (test-assert
     (lisp-source-balance-tests--balanced-p
      #P"escaped.lisp"
      (format nil "(list foo~C( foo~C))" #\\ #\\))
     "Common Lisp single escapes hide delimiter characters in symbols")
    (dolist (path '(#P"script.ros" #P"script.scm" #P"script.bb"))
      (test-assert
       (lisp-source-balance-tests--balanced-p
        path
        (format nil "#!/usr/bin/env lisp (~%(list)"))
       "First-line Lisp-family script headers ignore delimiter characters"))
  (test-assert
   (null
    (lisp-source-balance-issues
     #P"hex-character.scm"
     (format nil "#~Cx3b; (" #\\)))
   "A semicolon after a Scheme hex character literal starts a line comment")
  (let* ((issues
           (lisp-source-balance-issues
            #P"hex-identifier.scm"
            (format nil "foo~Cx28; (" #\\)))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':unclosed-open)
          (= (lisp-source-delimiter-line delimiter) 1)
          (= (lisp-source-delimiter-column delimiter) 10))
     "Scheme inline hex escapes preserve following structural delimiters"))
  (let* ((issues
           (lisp-source-balance-issues
            #P"missing.lisp"
            (format nil "(defun sample ()~%  (list 1)~%")))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':unclosed-open)
          (char= (lisp-source-delimiter-character delimiter) #\()
          (= (lisp-source-delimiter-line delimiter) 1)
          (= (lisp-source-delimiter-column delimiter) 1))
     "Common Lisp analysis locates an unmatched opening parenthesis"))
  (let* ((issues
           (lisp-source-balance-issues #P"extra.scm" "(define x 1))"))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':unexpected-close)
          (char= (lisp-source-delimiter-character delimiter) #\))
          (= (lisp-source-delimiter-line delimiter) 1)
          (= (lisp-source-delimiter-column delimiter) 13))
     "Scheme analysis locates an extra closing parenthesis"))
  (let* ((issues
           (lisp-source-balance-issues #P"mismatch.clj" "{:x [1 2)}"))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue)))
         (opener (and issue (lisp-source-balance-issue-opener issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':mismatched-close)
          (char= (lisp-source-delimiter-character delimiter) #\))
          (char= (lisp-source-delimiter-character opener) #\[)
          (char= (lisp-source-balance-issue-expected-character issue) #\]))
     "Clojure analysis detects delimiter type mismatches"))
  (test-assert
   (= (length (lisp-source-balance-issues #P"discard.scm" "#;(foo")) 1)
   "Scheme datum comments still require a balanced discarded datum")
  (test-assert
   (= (length (lisp-source-balance-issues #P"discard.clj" "#_(")) 1)
   "Clojure discard forms still require a balanced discarded form")
  (test-assert
   (null (lisp-source-balance-issues #P"string.lisp" "\"("))
   "An opening parenthesis inside an unterminated string is not structural")
  (test-assert
   (null (lisp-source-balance-issues #P"plain.txt" ")))"))
   "Unknown file extensions are not analyzed")
  (multiple-value-bind (issues count)
      (parenchek::lisp-source-balance--scan
       ':clojure
       (make-string 4096 :initial-element #\))
       :issue-limit 2)
    (test-assert
     (and (= (length issues) 2)
          (= count 4096))
     "Bounded warning scans retain only their requested diagnostic limit"))
  (let ((warning
          (lisp-source-balance-warning #P"bounded.clj" ")))))))")))
    (test-assert
     (and (search "7 unmatched or mismatched delimiters" warning)
          (search "1 additional issue omitted" warning))
     "Model-facing warnings bound individual diagnostics and report omissions"))
  (let* ((*lisp-source-balance-warning-character-limit* 80)
         (warning
           (lisp-source-balance-warning
            #P"bounded-warning.clj"
            (make-string 200 :initial-element #\)))))
    (test-assert
     (and (<= (length warning) 80)
          (search "delimiter warning truncated" warning))
     "Advisory warnings obey their complete character bound"))
  (let* ((path
           (make-pathname
            :name (format nil "unsafe~%name")
            :type "clj"))
         (warning (lisp-source-balance-warning path ")")))
    (test-assert
     (and (search "unsafe name.clj" warning)
          (not (search (format nil "unsafe~%name.clj") warning)))
     "Advisory warnings render pathnames without injected control lines"))
  (let* ((*lisp-source-balance-warning-character-limit* 180)
         (*lisp-source-balance-warning-issue-limit* "invalid")
         (path
           (make-pathname
            :name (format nil "unsafe~%fallback")
            :type "lisp"))
         (result
           (lisp-source-edit-result-content "Applied." path "(")))
    (test-assert
     (and (<= (length result) (+ (length "Applied.") 2 180))
          (search "could not complete" result)
          (search "unsafe fallback.lisp" result)
          (not (search (format nil "unsafe~%fallback.lisp") result)))
     "Checker-failure advisories remain bounded and control-safe"))
  (test-assert
   (string=
    (lisp-source-edit-result-content "Applied." #P"balanced.lisp" "(list 1)")
    "Applied.")
   "Balanced Lisp-family edit results remain unchanged")
  (test-assert
   (string=
    (lisp-source-edit-result-content "Applied." #P"plain.txt" "))")
    "Applied.")
   "Non-Lisp edit results remain unchanged")
  (let ((content
          (lisp-source-edit-result-content
           "Applied."
           #P"warning.clj"
           "{:value [1 2}")))
    (test-assert
     (and (search "Applied." content)
          (search "WARNING: The edit succeeded" content)
          (search "expected `]`" content))
     "Unbalanced Lisp-family edit results carry a successful advisory warning"))
  nil)


;;;; -- Explicit Parenthesis Check Tool Tests --

(-> test-lisp-paren-check () null)
(defun test-lisp-paren-check ()
  "Test bounded file, recursive directory, failure, and authority behavior."
  (let ((root (tests--temporary-directory)))
    (unwind-protect
         (let ((*test-readable-roots* (list root)))
           (labels ((call (&rest arguments)
                      "Execute the public path checker with string ARGUMENTS."
                      (let* ((position
                               (position "path" arguments :test #'string=))
                             (requested
                               (and position (nth (1+ position) arguments))))
                        (if (not (and (stringp requested)
                                      (plusp (length requested))))
                            (tool-result-create
                             :success-p nil
                             :content
                             "lisp.paren-check requires a non-empty string path.")
                            (handler-case
                                (multiple-value-bind (success-p content)
                                    (lisp-paren-check-path
                                     (pathname requested)
                                     :readable-roots *test-readable-roots*)
                                  (tool-result-create
                                   :success-p success-p
                                   :content content))
                              (lisp-paren-check-error (condition)
                                (tool-result-create
                                 :success-p nil
                                 :content (princ-to-string condition)))))))

                    (write-text (path content)
                      "Replace PATH with UTF-8 CONTENT."
                      (ensure-directories-exist path)
                      (with-open-file (stream path
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create
                                              :external-format :utf-8)
                        (write-string content stream))))
             (let* ((tree (merge-pathnames "source/" root))
                    (nested (merge-pathnames "nested/" tree))
                    (balanced (merge-pathnames "balanced.lisp" tree))
                    (scheme (merge-pathnames "valid.scm" nested))
                    (broken (merge-pathnames "bad.clj" nested))
                    (ignored (merge-pathnames "ignored.txt" tree)))
               (write-text balanced "(defun balanced () 42)")
               (write-text scheme "(define values [1 2 3])")
               (write-text broken "{:x [1 2)}")
               (write-text ignored ")))")
               (let* ((result (call "path" (namestring tree)))
                      (content (tool-result-content result)))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (search "Checked 3 of 3 recognized Lisp-family files" content)
                       (search "Found 1 unmatched or mismatched delimiter" content)
                       (search "nested/bad.clj" content)
                       (search "expected `]`" content)
                       (not (search "ignored.txt" content)))
                  "lisp.paren-check recursively checks recognized files with exact diagnostics"))
               (let* ((result (call "path" (namestring balanced)))
                      (content (tool-result-content result)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "Checked 1 of 1 recognized Lisp-family file" content)
                       (search "No unmatched or mismatched delimiters found" content))
                  "lisp.paren-check accepts one balanced source file"))
               (let* ((*lisp-paren-check-result-character-limit* 120)
                      (missing-root
                        (merge-pathnames "missing-readable-root/" root))
                      (condition
                        (handler-case
                            (progn
                              (lisp-paren-check-path
                               balanced :readable-roots (list missing-root))
                              nil)
                          (lisp-paren-check-error (caught)
                            caught))))
                 (test-assert
                  (and condition
                       (<= (length
                            (lisp-paren-check-error-message condition))
                           120)
                       (search
                        "Readable root"
                        (lisp-paren-check-error-message condition)))
                  "Invalid readable roots signal the bounded checker condition"))
               (let ((condition
                       (handler-case
                           (progn
                             (lisp-paren-check-path
                              balanced :readable-roots (list balanced))
                             nil)
                         (lisp-paren-check-error (caught)
                           caught))))
                 (test-assert
                  (and condition
                       (search
                        "is not a directory"
                        (lisp-paren-check-error-message condition)))
                  "Regular files are rejected as readable directory roots"))
               (write-text broken "{:x [1 2]}")
               (let ((result (call "path" (namestring tree))))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "Checked 3 of 3 recognized Lisp-family files"
                               (tool-result-content result)))
                  "lisp.paren-check succeeds after every recognized file is balanced"))
               (test-assert
                (not (tool-result-success-p
                      (call "path" (namestring ignored))))
                "lisp.paren-check rejects an explicitly unknown file type")
               (test-assert
                (not (tool-result-success-p
                      (call "path"
                            (namestring (merge-pathnames "missing.lisp" root)))))
                "lisp.paren-check rejects a missing path")
               (let* ((*lisp-paren-check-result-character-limit* 60)
                      (missing
                        (merge-pathnames
                         (make-pathname
                          :name
                          (format nil "missing~%~A"
                                  (make-string 120 :initial-element #\x))
                          :type "lisp")
                         root))
                      (result (call "path" (namestring missing)))
                      (content (tool-result-content result)))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (<= (length content) 60)
                       (not (find #\Newline content))
                       (search "result truncated" content))
                  "Direct failure results are bounded and control-safe"))
               (test-assert
                (not (tool-result-success-p (call)))
                "lisp.paren-check requires its path argument")
               (let ((plain-directory (merge-pathnames "plain/" root)))
                 (write-text (merge-pathnames "only.txt" plain-directory) "(")
                 (let ((result (call "path" (namestring plain-directory))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "No recognized Common Lisp, Scheme, or Clojure files"
                                 (tool-result-content result)))
                    "lisp.paren-check refuses to claim success for an empty source set")))
                (let* ((outside (merge-pathnames "outside/" root))
                       (outside-file
                         (merge-pathnames "outside.lisp" outside)))
                  (write-text outside-file "(list 1)")
                  (let ((*test-readable-roots* (list tree)))
                    (test-assert
                     (not (tool-result-success-p
                           (call "path" (namestring outside))))
                     "lisp.paren-check honors restricted workspace read roots"))
                  (let ((escape (merge-pathnames "escape" tree)))
                    (unwind-protect
                         (progn
                           (sb-posix:symlink (namestring outside)
                                             (namestring escape))
                           (let ((result (call "path" (namestring tree))))
                             (test-assert
                              (and (not (tool-result-success-p result))
                                   (search "outside the requested root"
                                           (tool-result-content result)))
                              "recursive checks refuse symbolic-link escapes")))
                      (when (probe-file escape)
                        (sb-posix:unlink (namestring escape)))))
                  (let ((link (merge-pathnames "balanced-link.lisp" tree)))
                    (unwind-protect
                         (progn
                           (sb-posix:symlink (namestring balanced)
                                             (namestring link))
                           (let ((*test-readable-roots* (list tree)))
                             (test-assert
                              (tool-result-success-p
                               (call "path" (namestring link)))
                              "direct checks accept stable source symlinks inside the readable root")))
                      (when (probe-file link)
                        (sb-posix:unlink (namestring link)))))
                  (let* ((race (merge-pathnames "race.lisp" tree))
                         (backup (merge-pathnames "race-original.lisp" tree))
                         (outside-race
                           (merge-pathnames "race-target.lisp" outside)))
                    (write-text race "()")
                    (with-open-file (stream outside-race
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create
                                            :element-type '(unsigned-byte 8))
                      (write-byte 255 stream))
                    (unwind-protect
                         (let ((*test-readable-roots* (list tree))
                                (*lisp-paren-check-after-first-read-function*
                                 (lambda (opened)
                                   (declare (ignore opened))
                                   (rename-file race backup)
                                   (sb-posix:symlink
                                    (namestring outside-race)
                                    (namestring race)))))
                           (let* ((result (call "path" (namestring race)))
                                  (content (tool-result-content result)))
                             (test-assert
                              (and (not (tool-result-success-p result))
                                   (search "outside the readable workspace and source roots"
                                           content)
                                   (not (search "not valid UTF-8" content)))
                              "descriptor checks reject a path replacement before reading outside authority")))
                      (when (probe-file backup)
                        (ignore-errors (sb-posix:unlink (namestring race)))
                        (rename-file backup race)))))
                (let* ((outside (merge-pathnames "outside/" root))
                       (directory-race
                         (merge-pathnames "directory-race/" root))
                       (directory-link
                         (merge-pathnames "directory-race" root))
                       (directory-backup
                         (merge-pathnames "directory-race-original/" root)))
                    (write-text
                     (merge-pathnames "inside.lisp" directory-race)
                     "()")
                    (unwind-protect
                         (let ((*test-readable-roots*
                                 (list directory-race))
                               (*lisp-paren-check-after-directory-open-function*
                                 (lambda (opened)
                                   (declare (ignore opened))
                                   (rename-file directory-race directory-backup)
                                   (sb-posix:symlink
                                    (namestring outside)
                                    (namestring directory-link)))))
                           (let* ((result
                                    (call "path" (namestring directory-race)))
                                  (content (tool-result-content result)))
                             (test-assert
                              (and (not (tool-result-success-p result))
                                   (search "changed during traversal" content)
                                   (not (search "outside.lisp" content)))
                              "directory descriptor checks reject replacement before outside traversal")))
                      (when (probe-file directory-backup)
                        (ignore-errors
                          (sb-posix:unlink (namestring directory-link)))
                        (rename-file directory-backup directory-race))))
                  (let ((mutable (merge-pathnames "mutable.lisp" tree)))
                    (write-text mutable "()")
                    (let ((*lisp-paren-check-after-first-read-function*
                            (lambda (opened)
                              (declare (ignore opened))
                              (with-open-file (stream mutable
                                                      :direction :output
                                                      :if-exists :overwrite
                                                      :external-format :utf-8)
                                (write-string ")(" stream)
                                (finish-output stream)))))
                      (let* ((result (call "path" (namestring mutable)))
                             (content (tool-result-content result)))
                        (test-assert
                         (and (not (tool-result-success-p result))
                              (search "changed while it was being read" content))
                         "repeated descriptor snapshots reject same-size in-place changes"))))
               (let ((limited (merge-pathnames "file-limit/" root)))
                 (write-text (merge-pathnames "one.lisp" limited) "()")
                 (write-text (merge-pathnames "two.lisp" limited) "()")
                 (let* ((*lisp-paren-check-file-limit* 1)
                        (result (call "path" (namestring limited))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "more than its 1-file limit"
                                 (tool-result-content result)))
                    "lisp.paren-check fails instead of silently truncating discovery")))
               (let ((large (merge-pathnames "large.lisp" root)))
                 (write-text large "(list 1)")
                 (let* ((*lisp-paren-check-file-character-limit* 4)
                        (result (call "path" (namestring large)))
                        (content (tool-result-content result)))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "Checked 0 of 1" content)
                         (search "per-file limit of 4 characters" content))
                    "lisp.paren-check reports files above its per-file input bound")))
               (let ((aggregate (merge-pathnames "aggregate/" root)))
                 (write-text (merge-pathnames "a.lisp" aggregate) "()")
                 (write-text (merge-pathnames "b.lisp" aggregate) "()")
                 (let* ((*lisp-paren-check-total-character-limit* 2)
                        (result (call "path" (namestring aggregate)))
                        (content (tool-result-content result)))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "Checked 1 of 2" content)
                         (search "aggregate source limit of 2 characters" content))
                    "lisp.paren-check reports incomplete aggregate-budget checks")))
               (let ((malformed (merge-pathnames "malformed.lisp" root)))
                 (with-open-file (stream malformed
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :element-type '(unsigned-byte 8))
                   (write-byte 255 stream))
                 (let ((result (call "path" (namestring malformed))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                           (search "not valid UTF-8"
                                   (tool-result-content result)))
                    "lisp.paren-check reports malformed source without crashing")))
                (dolist (limit '(0 1 10 45 46 47 48 49 50))
                  (let* ((*lisp-paren-check-result-character-limit* limit)
                         (content
                           (parenchek::lisp-paren-check--bounded-result
                            (make-string 200 :initial-element #\x))))
                    (test-assert
                     (<= (length content) limit)
                     "lisp.paren-check never exceeds a tiny result bound")))
               (let* ((*lisp-paren-check-result-character-limit* 40)
                      (condition
                        (make-condition
                         'lisp-paren-check-error
                         :message
                         (format nil "unsafe~%~A"
                                 (make-string 100 :initial-element #\x))))
                      (message (lisp-paren-check-error-message condition)))
                 (test-assert
                  (and (<= (length message) 40)
                       (not (find #\Newline message)))
                  "Public checker conditions bound and sanitize their messages"))
               (let ((many (merge-pathnames "many-issues.lisp" root)))
                 (write-text many (make-string 200 :initial-element #\)))
                 (let* ((*lisp-paren-check-issue-limit-per-file* 100)
                        (*lisp-paren-check-result-character-limit* 220)
                        (result (call "path" (namestring many)))
                        (content (tool-result-content result)))
                  (test-assert
                   (and (not (tool-result-success-p result))
                        (<= (length content) 220)
                        (search "diagnostic output truncated" content))
                   "lisp.paren-check bounds its complete model-visible result"))))))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)


;;;; -- Test Entry Point --

(defun run-tests ()
  "Run Parenchek's tests and return true on success."
  (setf *test-count* 0)
  (test-lisp-source-balance)
  (test-lisp-paren-check)
  (format t "~&~D Parenchek tests passed.~%" *test-count*)
  t)
