(in-package #:parenchek)

;;;; -- Lisp Source Balance Policy --

(defparameter *lisp-source-balance-warning-issue-limit* 6
  "The maximum unmatched delimiter diagnostics appended to one edit result.")

(defparameter *lisp-source-balance-warning-character-limit* 4000
  "The maximum characters returned by one advisory delimiter warning.")

(defparameter *lisp-source-balance-common-lisp-pathname-types*
  '("asd" "cl" "lisp" "lsp" "ros")
  "Pathname types recognized as Common Lisp source for delimiter checking.")

(defparameter *lisp-source-balance-scheme-pathname-types*
  '("sch" "scheme" "scm" "sld" "sls" "ss")
  "Pathname types recognized as Scheme source for delimiter checking.")

(defparameter *lisp-source-balance-clojure-pathname-types*
  '("bb" "clj" "cljc" "cljs" "edn")
  "Pathname types recognized as Clojure source for delimiter checking.")


;;;; -- Source Delimiter Diagnostics --

(defstruct (lisp-source-delimiter
             (:constructor lisp-source-delimiter-create
                 (&key character line column)))
  "One structural delimiter and its one-based source location."
  (character #\( :type character)
  (line 1 :type (integer 1))
  (column 1 :type (integer 1)))

(defstruct (lisp-source-balance-issue
             (:constructor lisp-source-balance-issue-create
                 (&key kind delimiter opener expected-character)))
  "One unmatched or mismatched Lisp-family structural delimiter."
  (kind ':unclosed-open
        :type (member :unexpected-close :mismatched-close :unclosed-open))
  (delimiter
    (lisp-source-delimiter-create
     :character #\(
     :line 1
     :column 1)
    :type lisp-source-delimiter)
  (opener nil :type (or null lisp-source-delimiter))
  (expected-character nil :type (or null character)))


;;;; -- Language Classification --

(-> lisp-source-balance-language (pathname) (option keyword))
(defun lisp-source-balance-language (path)
  "Return PATH's recognized Lisp-family language, or NIL for other files."
  (let ((pathname-type (pathname-type path)))
    (when (stringp pathname-type)
      (let ((normalized (string-downcase pathname-type)))
        (cond
          ((member normalized *lisp-source-balance-common-lisp-pathname-types*
                   :test #'string=)
           ':common-lisp)
          ((member normalized *lisp-source-balance-scheme-pathname-types*
                   :test #'string=)
           ':scheme)
          ((member normalized *lisp-source-balance-clojure-pathname-types*
                   :test #'string=)
           ':clojure)
          (t
           nil))))))

(-> lisp-source-balance--language-name (keyword) string)
(defun lisp-source-balance--language-name (language)
  "Return the user-visible name for recognized LANGUAGE."
  (ecase language
    (:common-lisp "Common Lisp")
    (:scheme "Scheme")
    (:clojure "Clojure")))


;;;; -- Lexical Delimiter Analysis --

(-> lisp-source-balance--whitespace-p (character) boolean)
(defun lisp-source-balance--whitespace-p (character)
  "Return true when CHARACTER is reader whitespace relevant to token scanning."
  (not (null (member character
                     '(#\Space #\Tab #\Newline #\Return #\Page)
                     :test #'char=))))

(-> lisp-source-balance--reader-token-delimiter-p (character) boolean)
(defun lisp-source-balance--reader-token-delimiter-p (character)
  "Return true when CHARACTER terminates a Lisp-family reader token."
  (or (lisp-source-balance--whitespace-p character)
      (not (null (find character "()[]{}\";'`,|" :test #'char=)))))

(-> lisp-source-balance--opening-delimiter-p (keyword character) boolean)
(defun lisp-source-balance--opening-delimiter-p (language character)
  "Return true when CHARACTER opens a structural form in LANGUAGE."
  (or (char= character #\()
      (and (member language '(:scheme :clojure))
           (char= character #\[))
      (and (eq language ':clojure)
           (char= character #\{))))

(-> lisp-source-balance--closing-delimiter-p (keyword character) boolean)
(defun lisp-source-balance--closing-delimiter-p (language character)
  "Return true when CHARACTER closes a structural form in LANGUAGE."
  (or (char= character #\))
      (and (member language '(:scheme :clojure))
           (char= character #\]))
      (and (eq language ':clojure)
           (char= character #\}))))

(-> lisp-source-balance--expected-closer (character) (option character))
(defun lisp-source-balance--expected-closer (opener)
  "Return the closing delimiter paired with OPENER."
  (case opener
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (otherwise
     nil)))

(-> lisp-source-balance--block-comments-p (keyword) boolean)
(defun lisp-source-balance--block-comments-p (language)
  "Return true when LANGUAGE supports nested #| ... |# comments."
  (not (null (member language '(:common-lisp :scheme)))))

(-> lisp-source-balance--escaped-symbols-p (keyword) boolean)
(defun lisp-source-balance--escaped-symbols-p (language)
  "Return true when LANGUAGE supports vertical-bar escaped symbols."
  (not (null (member language '(:common-lisp :scheme)))))

(-> lisp-source-balance--scan
    (keyword string &key (:issue-limit (option (integer 0))))
    (values list (integer 0)))
(defun lisp-source-balance--scan (language content &key issue-limit)
  "Return retained unmatched delimiter issues and the total issue count.

  The scanner ignores delimiters in strings, line comments, nested block comments,
  vertical-bar escaped symbols, Common Lisp single escapes, first-line #!
  directives, and language-specific character literals. Scheme #; and Clojure #_
  datum-discard markers do not suppress delimiter checking of what follows because
  the reader must still parse the discarded datum. ISSUE-LIMIT bounds retained
  diagnostics without stopping analysis."
  (let ((index 0)
        (line 1)
        (column 1)
        (content-length (length content))
        (stack (make-array 32 :adjustable t :fill-pointer 0))
        (issues nil)
        (issue-count 0)
        (retained-issue-count 0)
        (in-string-p nil)
        (string-escaped-p nil)
        (in-escaped-symbol-p nil)
        (symbol-escaped-p nil)
        (block-comment-depth 0))
    (labels ((peek (&optional (offset 0))
               "Return the character OFFSET positions ahead, or NIL at end."
               (let ((position (+ index offset)))
                 (and (< position content-length) (char content position))))

             (advance-one ()
               "Advance one source character while maintaining line and column."
               (let ((character (peek)))
                 (when character
                   (incf index)
                   (if (char= character #\Newline)
                       (progn
                         (incf line)
                         (setf column 1))
                       (incf column)))))

             (advance (count)
               "Advance COUNT source characters."
               (dotimes (ignored count)
                 (advance-one)))

             (record-issue
                 (kind &key delimiter delimiter-character delimiter-line
                            delimiter-column opener expected-character)
               "Count one issue and retain it when ISSUE-LIMIT permits."
               (incf issue-count)
               (when (or (null issue-limit)
                         (< retained-issue-count issue-limit))
                 (push
                  (lisp-source-balance-issue-create
                   :kind kind
                   :delimiter
                   (or delimiter
                       (lisp-source-delimiter-create
                        :character delimiter-character
                        :line delimiter-line
                        :column delimiter-column))
                   :opener opener
                   :expected-character expected-character)
                  issues)
                 (incf retained-issue-count)))

             (scheme-inline-hex-escape-length ()
               "Return the current Scheme inline hex escape length, or NIL."
               (when (and (peek)
                          (char= (peek) #\\)
                          (peek 1)
                          (find (peek 1) "xX" :test #'char=))
                 (loop for offset from 2
                       for character = (peek offset)
                       while (and character (digit-char-p character 16))
                       finally
                          (return
                            (and (> offset 2)
                                 character
                                 (char= character #\;)
                                 (1+ offset))))))

             (delimiter-at-current ()
               "Return a delimiter record for the current source position."
               (lisp-source-delimiter-create
                :character (peek)
                :line line
                :column column))

             (consume-line-comment ()
               "Advance to, but not through, the current line ending."
               (loop while (and (peek) (not (char= (peek) #\Newline)))
                     do (advance-one)))

             (consume-character-token (prefix-length)
               "Consume one reader character literal after PREFIX-LENGTH characters."
               (advance prefix-length)
               (when (peek)
                 (let ((first (peek)))
                   (advance-one)
                   (unless (lisp-source-balance--reader-token-delimiter-p first)
                     (loop while (and
                                  (peek)
                                  (not
                                   (lisp-source-balance--reader-token-delimiter-p
                                    (peek))))
                           do (advance-one)))))))
      (loop while (< index content-length)
            for character = (peek)
            for next = (peek 1)
            do
               (cond
                 ((plusp block-comment-depth)
                  (cond
                    ((and (char= character #\#)
                          next
                          (char= next #\|))
                     (incf block-comment-depth)
                     (advance 2))
                    ((and (char= character #\|)
                          next
                          (char= next #\#))
                     (decf block-comment-depth)
                     (advance 2))
                    (t
                     (advance-one))))
                 (in-string-p
                  (cond
                    (string-escaped-p
                     (setf string-escaped-p nil)
                     (advance-one))
                    ((char= character #\\)
                     (setf string-escaped-p t)
                     (advance-one))
                    ((char= character #\")
                     (setf in-string-p nil)
                     (advance-one))
                    (t
                     (advance-one))))
                 (in-escaped-symbol-p
                  (cond
                    (symbol-escaped-p
                     (setf symbol-escaped-p nil)
                     (advance-one))
                    ((char= character #\\)
                     (setf symbol-escaped-p t)
                     (advance-one))
                    ((char= character #\|)
                     (setf in-escaped-symbol-p nil)
                     (advance-one))
                    (t
                     (advance-one))))
                 ((and (zerop index)
                       (char= character #\#)
                       next
                       (char= next #\!))
                  (consume-line-comment))
                 ((and (lisp-source-balance--block-comments-p language)
                       (char= character #\#)
                       next
                       (char= next #\|))
                  (setf block-comment-depth 1)
                  (advance 2))
                 ((and (eq language ':scheme)
                       (char= character #\#)
                       next
                       (char= next #\;))
                  (advance 2))
                 ((and (member language '(:common-lisp :scheme))
                       (char= character #\#)
                       next
                       (char= next #\\))
                  (consume-character-token 2))
                 ((and (eq language ':common-lisp)
                       (char= character #\\))
                  (advance 2))
                 ((and (eq language ':scheme)
                       (char= character #\\)
                       next
                       (find next "xX" :test #'char=))
                  (let ((escape-length
                          (scheme-inline-hex-escape-length)))
                    (if escape-length
                        (advance escape-length)
                        (advance-one))))
                 ((and (eq language ':clojure)
                       (char= character #\\))
                  (consume-character-token 1))
                 ((char= character #\")
                  (setf in-string-p t)
                  (advance-one))
                 ((and (lisp-source-balance--escaped-symbols-p language)
                       (char= character #\|))
                  (setf in-escaped-symbol-p t)
                  (advance-one))
                 ((char= character #\;)
                  (consume-line-comment))
                 ((lisp-source-balance--opening-delimiter-p language character)
                  (vector-push-extend (delimiter-at-current) stack)
                  (advance-one))
                 ((lisp-source-balance--closing-delimiter-p language character)
                  (if (zerop (fill-pointer stack))
                      (record-issue
                       ':unexpected-close
                       :delimiter-character character
                       :delimiter-line line
                       :delimiter-column column)
                      (let* ((opener (vector-pop stack))
                             (expected
                               (lisp-source-balance--expected-closer
                                (lisp-source-delimiter-character opener))))
                        (unless (char= character expected)
                          (record-issue
                           ':mismatched-close
                           :delimiter-character character
                           :delimiter-line line
                           :delimiter-column column
                           :opener opener
                           :expected-character expected))))
                  (advance-one))
                 (t
                  (advance-one))))
      (loop for opener across stack
            do
               (record-issue
                ':unclosed-open
                :delimiter opener
                :expected-character
                (lisp-source-balance--expected-closer
                 (lisp-source-delimiter-character opener))))
      (values (nreverse issues) issue-count))))

(-> lisp-source-balance-issues (pathname string) list)
(defun lisp-source-balance-issues (path content)
  "Return every unmatched delimiter issue for recognized Lisp-family PATH content."
  (let ((language (lisp-source-balance-language path)))
    (if language
        (nth-value 0 (lisp-source-balance--scan language content))
        nil)))


;;;; -- Model-Facing Edit Warnings --

(-> lisp-source-balance--single-line (t (integer 0)) string)
(defun lisp-source-balance--single-line (value maximum)
  "Return VALUE as one control-free line no longer than MAXIMUM characters."
  (let ((text (princ-to-string value)))
    (with-output-to-string (stream)
      (loop for character across text
            repeat (min (length text) maximum)
            do (write-char
                (if (or (char= character #\Space)
                        (graphic-char-p character))
                    character
                    #\Space)
                stream)))))

(-> lisp-source-balance--bounded-warning (string) string)
(defun lisp-source-balance--bounded-warning (content)
  "Return CONTENT within the exact advisory warning character limit."
  (let* ((limit (max 0 *lisp-source-balance-warning-character-limit*))
         (marker "... delimiter warning truncated.")
         (suffix (format nil "~%~A" marker)))
    (cond
      ((<= (length content) limit)
       content)
      ((zerop limit)
       "")
      ((<= limit (length suffix))
       (subseq marker 0 (min limit (length marker))))
      (t
       (let ((prefix-length (- limit (length suffix))))
         (concatenate 'string
                      (subseq content 0 prefix-length)
                      suffix))))))

(-> lisp-source-balance--issue-description (lisp-source-balance-issue) string)
(defun lisp-source-balance--issue-description (issue)
  "Return one bounded model-facing description of ISSUE."
  (let* ((delimiter (lisp-source-balance-issue-delimiter issue))
         (character (lisp-source-delimiter-character delimiter))
         (line (lisp-source-delimiter-line delimiter))
         (column (lisp-source-delimiter-column delimiter)))
    (ecase (lisp-source-balance-issue-kind issue)
      (:unexpected-close
       (format nil "- line ~D, column ~D: unexpected closing `~C`."
               line column character))
      (:mismatched-close
       (let ((opener (lisp-source-balance-issue-opener issue)))
         (format nil
                 "- line ~D, column ~D: closing `~C` does not match `~C` from line ~D, column ~D; expected `~C`."
                 line
                 column
                 character
                 (lisp-source-delimiter-character opener)
                 (lisp-source-delimiter-line opener)
                 (lisp-source-delimiter-column opener)
                 (lisp-source-balance-issue-expected-character issue))))
      (:unclosed-open
       (format nil
               "- line ~D, column ~D: opening `~C` has no matching `~C`."
               line
               column
               character
               (lisp-source-balance-issue-expected-character issue))))))

(-> lisp-source-balance-warning (pathname string) (option string))
(defun lisp-source-balance-warning (path content)
  "Return a bounded advisory warning when recognized PATH has delimiter issues."
  (let ((language (lisp-source-balance-language path)))
    (when language
      (let ((issue-limit
              (max 0 *lisp-source-balance-warning-issue-limit*)))
        (multiple-value-bind (issues count)
            (lisp-source-balance--scan
             language content :issue-limit issue-limit)
          (when (plusp count)
            (let ((shown-count (length issues)))
              (lisp-source-balance--bounded-warning
               (with-output-to-string (stream)
                 (format stream
                         "WARNING: The edit succeeded, but ~A delimiter checking found ~D unmatched or mismatched delimiter~:P in ~A.~%"
                         (lisp-source-balance--language-name language)
                         count
                         (lisp-source-balance--single-line path 400))
                 (loop for issue in issues
                       do (write-line
                           (lisp-source-balance--issue-description issue)
                           stream))
                 (when (> count shown-count)
                   (format stream "- ... ~D additional issue~:P omitted.~%"
                           (- count shown-count)))
                 (write-string
                  "Review and correct the resulting file before loading or compiling it."
                  stream))))))))))

(-> lisp-source-edit-result-content (string pathname string) string)
(defun lisp-source-edit-result-content (result-content path content)
  "Append PATH's advisory delimiter warning to successful RESULT-CONTENT.

Unknown file types remain unchanged. An unexpected checker failure never changes
edit success and is reported as an advisory validation failure instead."
  (if (null (lisp-source-balance-language path))
      result-content
      (handler-case
          (let ((warning (lisp-source-balance-warning path content)))
            (if warning
                (format nil "~A~%~%~A" result-content warning)
                result-content))
        (error ()
          (let ((warning
                  (lisp-source-balance--bounded-warning
                   (format nil
                           "WARNING: The edit succeeded, but Lisp-family delimiter checking could not complete for ~A. Review the resulting file manually."
                           (lisp-source-balance--single-line path 400)))))
            (if (plusp (length warning))
                (format nil "~A~%~%~A" result-content warning)
                result-content))))))
