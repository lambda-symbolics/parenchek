(defpackage #:parenchek
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export
   #:*lisp-source-balance-warning-issue-limit*
   #:*lisp-source-balance-warning-character-limit*
   #:*lisp-source-balance-common-lisp-pathname-types*
   #:*lisp-source-balance-scheme-pathname-types*
   #:*lisp-source-balance-clojure-pathname-types*
   #:*lisp-paren-check-depth-limit*
   #:*lisp-paren-check-directory-limit*
   #:*lisp-paren-check-entry-limit*
   #:*lisp-paren-check-file-limit*
   #:*lisp-paren-check-file-character-limit*
   #:*lisp-paren-check-total-character-limit*
   #:*lisp-paren-check-issue-limit-per-file*
   #:*lisp-paren-check-reported-file-limit*
   #:*lisp-paren-check-result-character-limit*
   #:*lisp-paren-check-after-directory-open-function*
   #:*lisp-paren-check-after-first-read-function*
   #:lisp-source-delimiter
   #:lisp-source-delimiter-create
   #:lisp-source-delimiter-character
   #:lisp-source-delimiter-line
   #:lisp-source-delimiter-column
   #:lisp-source-balance-issue
   #:lisp-source-balance-issue-create
   #:lisp-source-balance-issue-kind
   #:lisp-source-balance-issue-delimiter
   #:lisp-source-balance-issue-opener
   #:lisp-source-balance-issue-expected-character
   #:lisp-source-balance-language
   #:lisp-source-balance-issues
   #:lisp-source-balance-warning
   #:lisp-source-edit-result-content
   #:lisp-paren-check-error
   #:lisp-paren-check-error-message
   #:lisp-paren-check-error-tool-name
   #:lisp-paren-check-diagnostic
   #:lisp-paren-check-diagnostic-create
   #:lisp-paren-check-diagnostic-path
   #:lisp-paren-check-diagnostic-language
   #:lisp-paren-check-diagnostic-issues
   #:lisp-paren-check-diagnostic-issue-count
   #:lisp-paren-check-diagnostic-failure
   #:lisp-paren-check-run
   #:lisp-paren-check-run-create
   #:lisp-paren-check-run-candidate-count
   #:lisp-paren-check-run-checked-count
   #:lisp-paren-check-run-issue-file-count
   #:lisp-paren-check-run-issue-count
   #:lisp-paren-check-run-diagnostics
   #:lisp-paren-check-run-success-p
   #:lisp-paren-check-path))

(in-package #:parenchek)

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))
