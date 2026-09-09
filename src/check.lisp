(in-package #:parenchek)

;;;; -- Check Conditions --

(define-condition lisp-paren-check-error (error)
  ((message
    :initarg :message
    :documentation "The raw failure text exposed through a bounded reader.")
   (tool-name
    :initarg :tool-name
    :initform "lisp.paren-check"
    :reader lisp-paren-check-error-tool-name
    :documentation "The operation name associated with the failure."))
  (:documentation "A safe source traversal or snapshot failure.")
  (:report
   (lambda (condition stream)
     (write-string (lisp-paren-check-error-message condition) stream))))


;;;; -- Check Policy --

(defparameter *lisp-paren-check-depth-limit* 64
  "The maximum directory depth traversed by one lisp.paren-check call.")

(defparameter *lisp-paren-check-directory-limit* 4096
  "The maximum directories traversed by one lisp.paren-check call.")

(defparameter *lisp-paren-check-entry-limit* 100000
  "The maximum directory entries inspected by one lisp.paren-check call.")

(defparameter *lisp-paren-check-file-limit* 10000
  "The maximum recognized source files accepted by one lisp.paren-check call.")

(defparameter *lisp-paren-check-file-character-limit* (* 8 1024 1024)
  "The maximum characters read from one lisp.paren-check source file.")

(defparameter *lisp-paren-check-total-character-limit* (* 64 1024 1024)
  "The maximum source characters scanned by one lisp.paren-check call.")

(defparameter *lisp-paren-check-issue-limit-per-file* 12
  "The maximum exact issue details retained for one checked source file.")

(defparameter *lisp-paren-check-reported-file-limit* 40
  "The maximum source-file diagnostic sections retained in one result.")

(defparameter *lisp-paren-check-result-character-limit* 7600
  "The maximum characters returned by one lisp.paren-check result.")

(defparameter *lisp-paren-check-after-directory-open-function* nil
  "Optional internal callback invoked after opening a checked directory.")

(defparameter *lisp-paren-check-after-first-read-function* nil
  "Optional internal callback invoked after the first checked file snapshot.")


;;;; -- Check Results --

(defstruct (lisp-paren-check-diagnostic
             (:constructor lisp-paren-check-diagnostic-create
                 (&key path language issues issue-count failure)))
  "Delimiter issues or a checking failure for one source file."
  (path #P"" :type pathname)
  (language nil :type (option keyword))
  (issues nil :type list)
  (issue-count 0 :type (integer 0))
  (failure nil :type (option string)))

(defstruct (lisp-paren-check-run
             (:constructor lisp-paren-check-run-create
                 (&key candidate-count checked-count issue-file-count
                       issue-count diagnostics)))
  "The bounded aggregate result of one Lisp-family source check."
  (candidate-count 0 :type (integer 0))
  (checked-count 0 :type (integer 0))
  (issue-file-count 0 :type (integer 0))
  (issue-count 0 :type (integer 0))
  (diagnostics nil :type list))

(-> lisp-paren-check-run-success-p (lisp-paren-check-run) boolean)
(defun lisp-paren-check-run-success-p (run)
  "Return true when RUN checked every candidate and found no delimiter issues."
  (and (= (lisp-paren-check-run-candidate-count run)
          (lisp-paren-check-run-checked-count run))
       (zerop (lisp-paren-check-run-issue-count run))))


;;;; -- Bounded File Discovery --

(-> lisp-paren-check--pathname< (pathname pathname) boolean)
(defun lisp-paren-check--pathname< (left right)
  "Return true when LEFT sorts before RIGHT by namestring."
  (not (null (string< (namestring left) (namestring right)))))

(-> lisp-paren-check--canonical-subpath-p (pathname pathname) boolean)
(defun lisp-paren-check--canonical-subpath-p (path root)
  "Return true when canonical PATH is ROOT or lies beneath it."
  (or (uiop:pathname-equal path root)
      (not (null (uiop:subpathp path root)))))

;;; Observations are LS-COMPAT.POSIX:FILE-INFORMATION objects, whose
;;; identities name a filesystem object on its volume on every host.

(-> lisp-paren-check--same-file-stat-p (t t) boolean)
(defun lisp-paren-check--same-file-stat-p (left right)
  "Return true when observations LEFT and RIGHT identify the same filesystem object."
  (equal (ls-compat.posix:file-information-identity left)
         (ls-compat.posix:file-information-identity right)))

(-> lisp-paren-check--stat-identity (t) t)
(defun lisp-paren-check--stat-identity (stat)
  "Return the EQUAL-comparable identity of observation STAT."
  (ls-compat.posix:file-information-identity stat))

(-> lisp-paren-check--observe (pathname &key (:follow-links-p boolean)) t)
(defun lisp-paren-check--observe (path &key (follow-links-p t))
  "Observe PATH, following a symbolic link unless FOLLOW-LINKS-P is false."
  (ls-compat.posix:file-information path :follow-links-p follow-links-p))

(-> lisp-paren-check--directory-p (t) boolean)
(defun lisp-paren-check--directory-p (stat)
  "Return true when observation STAT is a directory."
  (eq (ls-compat.posix:file-information-kind stat) ':directory))

(-> lisp-paren-check--regular-file-p (t) boolean)
(defun lisp-paren-check--regular-file-p (stat)
  "Return true when observation STAT is a regular file."
  (eq (ls-compat.posix:file-information-kind stat) ':file))

(-> lisp-paren-check--canonical-readable-roots (list) list)
(defun lisp-paren-check--canonical-readable-roots (roots)
  "Return a fixed canonical snapshot of readable ROOTS."
  (mapcar
   (lambda (root)
     (let ((canonical
             (handler-case
                 (truename root)
               (error (condition)
                 (error 'lisp-paren-check-error
                        :message
                        (format nil
                                "Readable root ~A could not be safely resolved: ~A"
                                root
                                condition)
                        :tool-name "lisp.paren-check")))))
       (unless (uiop:directory-pathname-p canonical)
         (error 'lisp-paren-check-error
                :message
                (format nil "Readable root ~A is not a directory." root)
                :tool-name "lisp.paren-check"))
       (uiop:ensure-directory-pathname canonical)))
   roots))

(-> lisp-paren-check--validate-canonical-path
    (pathname pathname list &key (:requested-root (option pathname)))
    null)
(defun lisp-paren-check--validate-canonical-path
    (path canonical readable-roots &key requested-root)
  "Reject canonical PATH outside fixed readable or requested roots."
  (when (and readable-roots
             (not (some (lambda (root)
                          (lisp-paren-check--canonical-subpath-p
                           canonical root))
                        readable-roots)))
    (error 'lisp-paren-check-error
           :message
           (format nil
                   "Path ~A moved outside the readable workspace and source roots while being checked."
                   path)
           :tool-name "lisp.paren-check"))
  (when (and requested-root
             (not (lisp-paren-check--canonical-subpath-p
                   canonical requested-root)))
    (error 'lisp-paren-check-error
           :message
           (format nil
                   "Lisp source path ~A moved outside the requested root during checking."
                   path)
           :tool-name "lisp.paren-check"))
  nil)

(-> lisp-paren-check--observe-path
    (pathname list &key (:requested-root (option pathname)))
    (values pathname t))
(defun lisp-paren-check--observe-path
    (path readable-roots &key requested-root)
  "Return a stable canonical pathname and stat observation for existing PATH."
  (labels ((observe ()
             "Return one internally consistent canonical observation of PATH."
             (let* ((canonical (truename path))
                    (path-stat (lisp-paren-check--observe path))
                    (canonical-stat (lisp-paren-check--observe canonical)))
               (lisp-paren-check--validate-canonical-path
                path canonical readable-roots :requested-root requested-root)
               (unless (lisp-paren-check--same-file-stat-p
                        path-stat canonical-stat)
                 (error 'lisp-paren-check-error
                        :message
                        (format nil
                                "Lisp source path ~A changed while it was being resolved."
                                path)
                        :tool-name "lisp.paren-check"))
               (values canonical path-stat))))
    (multiple-value-bind (canonical-before stat-before)
        (observe)
      (multiple-value-bind (canonical-after stat-after)
          (observe)
        (unless (and (uiop:pathname-equal canonical-before canonical-after)
                     (lisp-paren-check--same-file-stat-p
                      stat-before stat-after))
          (error 'lisp-paren-check-error
                 :message
                 (format nil
                         "Lisp source path ~A changed while its authority boundary was being checked."
                         path)
                 :tool-name "lisp.paren-check"))
        (values canonical-after stat-after)))))

;;; POSIX hosts verify a directory through the descriptor behind its open
;;; handle, which Windows enumeration has no counterpart for; the Windows
;;; traversal below observes the directory by path before and after each
;;; enumeration instead.

#-win32
(-> lisp-paren-check--directory-file-descriptor (t) integer)
#-win32
(defun lisp-paren-check--directory-file-descriptor (directory)
  "Return the native file descriptor backing open DIRECTORY."
  (let ((descriptor
          (sb-alien:alien-funcall
           (sb-alien:extern-alien
            "dirfd" (function sb-alien:int (* t)))
           directory)))
    (when (minusp descriptor)
      (error 'lisp-paren-check-error
             :message "Could not obtain an opened Lisp source directory descriptor."
             :tool-name "lisp.paren-check"))
    descriptor))

(-> lisp-paren-check--validate-directory-path (pathname t) null)
(defun lisp-paren-check--validate-directory-path (path opened-stat)
  "Require PATH to keep naming the directory identified by OPENED-STAT."
  (let ((current-stat
          (handler-case
              (lisp-paren-check--observe path)
            (error ()
              (error 'lisp-paren-check-error
                     :message
                     (format nil
                             "Lisp source directory ~A changed during traversal."
                             path)
                     :tool-name "lisp.paren-check")))))
    (unless (lisp-paren-check--same-file-stat-p opened-stat current-stat)
      (error 'lisp-paren-check-error
             :message
             (format nil
                     "Lisp source directory ~A changed during traversal."
                     path)
             :tool-name "lisp.paren-check")))
  nil)

#-win32
(-> lisp-paren-check--rewind-directory (t) null)
#-win32
(defun lisp-paren-check--rewind-directory (directory)
  "Rewind open DIRECTORY to its first entry."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien
    "rewinddir" (function sb-alien:void (* t)))
   directory)
  nil)

#-win32
(-> lisp-paren-check--directory-entry-names
    (t (integer 0))
    (values list (integer 0) boolean))
#-win32
(defun lisp-paren-check--directory-entry-names (directory entry-limit)
  "Return sorted entry names from open DIRECTORY within ENTRY-LIMIT."
  (let ((names nil)
        (entry-count 0)
        (exceeded-p nil))
    (loop for entry = (sb-posix:readdir directory)
          until (sb-alien:null-alien entry)
          for name = (sb-posix:dirent-name entry)
          unless (member name '("." "..") :test #'string=)
            do
               (incf entry-count)
               (when (> entry-count entry-limit)
                 (setf entry-count entry-limit
                       exceeded-p t)
                 (loop-finish))
               (push name names))
    (values (sort names #'string<) entry-count exceeded-p)))

(-> lisp-paren-check--classify-directory-entries
    (pathname t list)
    (values list list))
(defun lisp-paren-check--classify-directory-entries
    (directory opened-stat names)
  "Return recognized files and subdirectories named by verified DIRECTORY."
  (let ((files nil)
        (subdirectories nil))
    (dolist (name names)
      (lisp-paren-check--validate-directory-path directory opened-stat)
      (let* ((path (merge-pathnames name directory))
             (status (lisp-paren-check--observe path :follow-links-p nil))
             (kind (ls-compat.posix:file-information-kind status)))
        (cond
          ((eq kind ':directory)
           (push (uiop:ensure-directory-pathname path) subdirectories))
          ((eq kind ':symbolic-link)
           (handler-case
               (let ((target-status (lisp-paren-check--observe path)))
                 (cond
                   ((lisp-paren-check--directory-p target-status)
                    (push (uiop:ensure-directory-pathname path)
                          subdirectories))
                   ((and (lisp-paren-check--regular-file-p target-status)
                         (lisp-source-balance-language path))
                    (push path files))))
             (error (condition)
               (error 'lisp-paren-check-error
                      :message
                      (format nil
                              "Could not resolve symbolic link ~A while checking Lisp source: ~A"
                              path
                              condition)
                      :tool-name "lisp.paren-check"))))
          ((and (eq kind ':file)
                (lisp-source-balance-language path))
           (push path files))))
      (lisp-paren-check--validate-directory-path directory opened-stat))
    (values (sort files #'lisp-paren-check--pathname<)
            (sort subdirectories #'lisp-paren-check--pathname<))))

#-win32
(-> lisp-paren-check--descriptor-observation (integer) t)
#-win32
(defun lisp-paren-check--descriptor-observation (descriptor)
  "Observe the object behind open DESCRIPTOR."
  (let* ((stat (sb-posix:fstat descriptor))
         (mode (sb-posix:stat-mode stat)))
    (ls-compat.posix:make-file-information
     (cond
       ((sb-posix:s-isreg mode) ':file)
       ((sb-posix:s-isdir mode) ':directory)
       ((sb-posix:s-islnk mode) ':symbolic-link)
       (t ':other))
     (cons (sb-posix:stat-dev stat) (sb-posix:stat-ino stat))
     (sb-posix:stat-size stat)
     (sb-posix:stat-mtime stat)
     (sb-posix:stat-ctime stat))))

#+win32
(-> lisp-paren-check--directory-entries
    (pathname t (integer 0))
    (values list list (integer 0) boolean))
#+win32
(defun lisp-paren-check--directory-entries
    (directory expected-stat entry-limit)
  "Return repeated stable classifications from verified DIRECTORY.

Enumeration stops at the first entry beyond ENTRY-LIMIT. The third value is the
number of retained entries, and the fourth value reports whether the limit was
exceeded. Windows has no descriptor for an enumerated directory, so the
directory is observed by path before and after each pass instead."
  (flet ((names ()
           "Return the sorted entry names of DIRECTORY and whether more exist."
           (multiple-value-bind (entries exceeded-p)
               (ls-compat.posix:directory-entries directory :limit entry-limit)
             (values (sort (mapcar #'car entries) #'string<) exceeded-p))))
    (lisp-paren-check--validate-directory-path directory expected-stat)
    (when *lisp-paren-check-after-directory-open-function*
      (funcall *lisp-paren-check-after-directory-open-function* directory))
    (multiple-value-bind (names exceeded-p)
        (names)
      (when exceeded-p
        (return-from lisp-paren-check--directory-entries
          (values nil nil entry-limit t)))
      (lisp-paren-check--validate-directory-path directory expected-stat)
      (multiple-value-bind (files subdirectories)
          (lisp-paren-check--classify-directory-entries
           directory expected-stat names)
        (multiple-value-bind (verification-names verification-exceeded-p)
            (names)
          (unless (and (not verification-exceeded-p)
                       (equal names verification-names))
            (error 'lisp-paren-check-error
                   :message
                   (format nil
                           "Lisp source directory ~A changed during traversal."
                           directory)
                   :tool-name "lisp.paren-check"))
          (multiple-value-bind (verification-files verification-subdirectories)
              (lisp-paren-check--classify-directory-entries
               directory expected-stat verification-names)
            (unless (and (equal (mapcar #'namestring files)
                                (mapcar #'namestring verification-files))
                         (equal (mapcar #'namestring subdirectories)
                                (mapcar #'namestring
                                        verification-subdirectories)))
              (error 'lisp-paren-check-error
                     :message
                     (format nil
                             "Lisp source directory ~A changed during traversal."
                             directory)
                     :tool-name "lisp.paren-check"))
            (lisp-paren-check--validate-directory-path directory expected-stat)
            (values verification-files
                    verification-subdirectories
                    (length names)
                    nil)))))))

#-win32
(-> lisp-paren-check--directory-entries
    (pathname t (integer 0))
    (values list list (integer 0) boolean))
#-win32
(defun lisp-paren-check--directory-entries
    (directory expected-stat entry-limit)
  "Return repeated stable classifications from verified DIRECTORY.

Enumeration stops at the first entry beyond ENTRY-LIMIT. The third value is the
number of retained entries, and the fourth value reports whether the limit was
exceeded. Symbolic links are classified by their targets; an unresolved link
makes the check fail rather than silently claiming complete coverage."
  (let ((handle nil))
    (unwind-protect
         (progn
           (setf handle (sb-posix:opendir (namestring directory)))
           (let* ((descriptor
                    (lisp-paren-check--directory-file-descriptor handle))
                  (opened-stat
                    (lisp-paren-check--descriptor-observation descriptor)))
             (unless (and (lisp-paren-check--directory-p opened-stat)
                          (lisp-paren-check--same-file-stat-p
                           expected-stat opened-stat))
               (error 'lisp-paren-check-error
                      :message
                      (format nil
                              "Lisp source directory ~A changed before traversal."
                              directory)
                      :tool-name "lisp.paren-check"))
             (when *lisp-paren-check-after-directory-open-function*
               (funcall *lisp-paren-check-after-directory-open-function*
                        directory))
             (lisp-paren-check--validate-directory-path directory opened-stat)
             (multiple-value-bind (names entry-count exceeded-p)
                 (lisp-paren-check--directory-entry-names handle entry-limit)
               (when exceeded-p
                 (return-from lisp-paren-check--directory-entries
                   (values nil nil entry-count t)))
               (lisp-paren-check--validate-directory-path
                directory opened-stat)
               (multiple-value-bind (files subdirectories)
                   (lisp-paren-check--classify-directory-entries
                    directory opened-stat names)
                 (lisp-paren-check--rewind-directory handle)
                 (multiple-value-bind
                       (verification-names verification-count
                        verification-exceeded-p)
                     (lisp-paren-check--directory-entry-names
                      handle entry-limit)
                   (declare (ignore verification-count))
                   (unless (and (not verification-exceeded-p)
                                (equal names verification-names))
                     (error 'lisp-paren-check-error
                            :message
                            (format nil
                                    "Lisp source directory ~A changed during traversal."
                                    directory)
                            :tool-name "lisp.paren-check"))
                   (multiple-value-bind
                         (verification-files verification-subdirectories)
                       (lisp-paren-check--classify-directory-entries
                        directory opened-stat verification-names)
                     (unless (and
                              (equal (mapcar #'namestring files)
                                     (mapcar #'namestring verification-files))
                              (equal (mapcar #'namestring subdirectories)
                                     (mapcar #'namestring
                                             verification-subdirectories)))
                       (error 'lisp-paren-check-error
                              :message
                              (format nil
                                      "Lisp source directory ~A changed during traversal."
                                      directory)
                              :tool-name "lisp.paren-check"))
                     (lisp-paren-check--validate-directory-path
                      directory opened-stat)
                     (values verification-files
                             verification-subdirectories
                             entry-count
                             nil)))))))
      (when handle
        (sb-posix:closedir handle)))))

(-> lisp-paren-check--directory-files
    (pathname &key (:canonical-root pathname) (:root-stat t)
     (:readable-roots list))
    (values list pathname))
(defun lisp-paren-check--directory-files
    (root &key canonical-root root-stat readable-roots)
  "Return bounded recognized source files beneath verified directory ROOT."
  (let* ((root (uiop:ensure-directory-pathname root))
         (canonical-root (uiop:ensure-directory-pathname canonical-root))
         (visited-directories (make-hash-table :test #'equal))
         (visited-files (make-hash-table :test #'equal))
         (files nil)
         (file-count 0)
         (directory-count 0)
         (entry-count 0))
    (labels ((walk (directory depth &key canonical status)
               "Traverse verified DIRECTORY at DEPTH beneath CANONICAL-ROOT."
               (when (> depth (max 0 *lisp-paren-check-depth-limit*))
                 (error 'lisp-paren-check-error
                        :message
                        (format nil
                                "Lisp source traversal exceeded its depth limit of ~D at ~A."
                                (max 0 *lisp-paren-check-depth-limit*)
                                directory)
                        :tool-name "lisp.paren-check"))
               (unless (and canonical status)
                 (multiple-value-setq (canonical status)
                   (lisp-paren-check--observe-path
                    directory readable-roots
                    :requested-root canonical-root)))
               (unless (lisp-paren-check--directory-p status)
                 (error 'lisp-paren-check-error
                        :message
                        (format nil "~A is not a directory." directory)
                        :tool-name "lisp.paren-check"))
               (let ((identity (lisp-paren-check--stat-identity status)))
                 (when (gethash identity visited-directories)
                   (return-from walk nil))
                 (setf (gethash identity visited-directories) t))
               (incf directory-count)
               (when (> directory-count
                        (max 0 *lisp-paren-check-directory-limit*))
                 (error 'lisp-paren-check-error
                        :message
                        (format nil
                                "Lisp source traversal exceeded its directory limit of ~D."
                                (max 0 *lisp-paren-check-directory-limit*))
                        :tool-name "lisp.paren-check"))
               (let ((remaining
                       (max 0
                            (- (max 0 *lisp-paren-check-entry-limit*)
                               entry-count))))
                 (multiple-value-bind
                       (direct-files subdirectories entries exceeded-p)
                     (lisp-paren-check--directory-entries
                      directory status remaining)
                   (incf entry-count entries)
                   (when exceeded-p
                     (error 'lisp-paren-check-error
                            :message
                            (format nil
                                    "Lisp source traversal exceeded its entry limit of ~D."
                                    (max 0 *lisp-paren-check-entry-limit*))
                            :tool-name "lisp.paren-check"))
                   (dolist (file direct-files)
                     (multiple-value-bind (canonical-file file-stat)
                         (lisp-paren-check--observe-path
                          file readable-roots :requested-root canonical-root)
                       (declare (ignore canonical-file))
                       (let ((identity
                               (lisp-paren-check--stat-identity file-stat)))
                         (unless (gethash identity visited-files)
                           (setf (gethash identity visited-files) t)
                           (incf file-count)
                           (when (> file-count
                                    (max 0 *lisp-paren-check-file-limit*))
                             (error 'lisp-paren-check-error
                                    :message
                                    (format nil
                                            "Lisp source traversal found more than its ~D-file limit."
                                            (max 0 *lisp-paren-check-file-limit*))
                                    :tool-name "lisp.paren-check"))
                           (push file files)))))
                   (dolist (subdirectory subdirectories)
                     (multiple-value-bind (canonical-subdirectory
                                           subdirectory-stat)
                         (lisp-paren-check--observe-path
                          subdirectory readable-roots
                          :requested-root canonical-root)
                       (walk subdirectory
                             (1+ depth)
                             :canonical canonical-subdirectory
                             :status subdirectory-stat)))))))
      (walk root 0 :canonical canonical-root :status root-stat))
    (values (sort files #'lisp-paren-check--pathname<)
            canonical-root)))


;;;; -- Bounded Source Checking --

(-> lisp-paren-check--stable-file-stat-p (t t) boolean)
(defun lisp-paren-check--stable-file-stat-p (before after)
  "Return true when one opened file kept the same identity, size, and timestamps."
  (and (lisp-paren-check--same-file-stat-p before after)
       (= (ls-compat.posix:file-information-size before)
          (ls-compat.posix:file-information-size after))
       (= (ls-compat.posix:file-information-modification-time before)
          (ls-compat.posix:file-information-modification-time after))
       (= (ls-compat.posix:file-information-change-time before)
          (ls-compat.posix:file-information-change-time after))))

(-> lisp-paren-check--validate-opened-file
    (pathname t list &key (:canonical-root (option pathname)))
    null)
(defun lisp-paren-check--validate-opened-file
    (path opened-stat readable-roots &key canonical-root)
  "Recheck PATH authority and identity against OPENED-STAT before reading."
  (multiple-value-bind (canonical current-stat)
      (lisp-paren-check--observe-path
       path readable-roots :requested-root canonical-root)
    (declare (ignore canonical))
    (unless (lisp-paren-check--same-file-stat-p opened-stat current-stat)
      (error 'lisp-paren-check-error
             :message
             (format nil
                     "Lisp source file ~A changed while its authority boundary was being checked."
                     path)
             :tool-name "lisp.paren-check")))
  nil)

(-> lisp-paren-check--read-exact-octets
    (stream (integer 0) pathname)
    (simple-array (unsigned-byte 8) (*)))
(defun lisp-paren-check--read-exact-octets (stream length path)
  "Read exactly LENGTH octets from STREAM for checked source PATH."
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (unless (= (read-sequence octets stream) length)
      (error 'lisp-paren-check-error
             :message
             (format nil
                     "Lisp source file ~A changed while it was being read."
                     path)
             :tool-name "lisp.paren-check"))
    octets))

(-> lisp-paren-check--read-file
    (pathname (integer 0) list
     &key (:canonical-root (option pathname)))
    (values (option string) (integer 0) boolean))
(defun lisp-paren-check--read-file
    (path character-limit readable-roots &key canonical-root)
  "Read a repeated UTF-8 snapshot of PATH within CHARACTER-LIMIT.

Return the content, retained character count, and whether more input existed.
The content is NIL when the limit was exceeded."
  (let* ((limit (max 0 character-limit))
         (byte-limit
           (min (* 4 limit) (1- array-total-size-limit)))
         (open-path (truename path))
         (stream nil))
    (lisp-paren-check--validate-canonical-path
     path open-path readable-roots :requested-root canonical-root)
    (unwind-protect
         (multiple-value-bind (opened-stream stat)
             (handler-case
                 (ls-compat.posix:open-regular-file open-path)
               (ls-compat.posix:not-regular-file ()
                 (error 'lisp-paren-check-error
                        :message (format nil "~A is not a regular file." path)
                        :tool-name "lisp.paren-check")))
           (setf stream opened-stream)
           (progn
             (lisp-paren-check--validate-opened-file
              path stat readable-roots :canonical-root canonical-root)
             (let ((length (ls-compat.posix:file-information-size stat)))
               (when (> length byte-limit)
                 (return-from lisp-paren-check--read-file
                   (values nil (1+ limit) t)))
               (let ((octets
                       (lisp-paren-check--read-exact-octets
                        stream length path)))
                 (when *lisp-paren-check-after-first-read-function*
                   (funcall *lisp-paren-check-after-first-read-function* path))
                 (lisp-paren-check--validate-opened-file
                  path stat readable-roots :canonical-root canonical-root)
                 (unless (file-position stream 0)
                   (error 'lisp-paren-check-error
                          :message
                          (format nil
                                  "Could not repeat the Lisp source snapshot for ~A."
                                  path)
                          :tool-name "lisp.paren-check"))
                 (let ((verification
                         (lisp-paren-check--read-exact-octets
                          stream length path)))
                   (unless (and
                            (lisp-paren-check--stable-file-stat-p
                             stat (ls-compat.posix:stream-file-information stream))
                            (equalp octets verification))
                     (error 'lisp-paren-check-error
                            :message
                            (format nil
                                    "Lisp source file ~A changed while it was being read."
                                    path)
                            :tool-name "lisp.paren-check"))
                   (let ((content
                           (handler-case
                               (sb-ext:octets-to-string
                                octets :external-format :utf-8)
                             (error ()
                               (error 'lisp-paren-check-error
                                      :message
                                      (format nil
                                              "Lisp source file ~A is not valid UTF-8 text."
                                              path)
                                      :tool-name "lisp.paren-check")))))
                     (if (> (length content) limit)
                         (values nil (length content) t)
                         (values content (length content) nil))))))))
      (when stream
        (close stream)))))

(-> lisp-paren-check--single-line (t (integer 0)) string)
(defun lisp-paren-check--single-line (value maximum)
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

(-> lisp-paren-check--check-files
    (list &key (:canonical-root (option pathname)) (:readable-roots list))
    lisp-paren-check-run)
(defun lisp-paren-check--check-files
    (files &key canonical-root readable-roots)
  "Check sorted recognized FILES within bounded source and diagnostic budgets."
  (let ((candidate-count (length files))
        (checked-count 0)
        (issue-file-count 0)
        (issue-count 0)
        (total-characters 0)
        (diagnostics nil)
        (file-limit (max 0 *lisp-paren-check-file-character-limit*))
        (total-limit (max 0 *lisp-paren-check-total-character-limit*))
        (detail-limit (max 0 *lisp-paren-check-issue-limit-per-file*)))
    (loop for file in files
          do
             (let* ((remaining (max 0 (- total-limit total-characters)))
                    (read-limit (min file-limit remaining)))
               (handler-case
                   (multiple-value-bind (content character-count exceeded-p)
                       (lisp-paren-check--read-file
                        file read-limit readable-roots
                        :canonical-root canonical-root)
                     (cond
                       (exceeded-p
                        (if (< read-limit file-limit)
                            (progn
                              (push
                               (lisp-paren-check-diagnostic-create
                                :path file
                                :failure
                                (format nil
                                        "the aggregate source limit of ~:D characters was reached before this file"
                                        total-limit))
                               diagnostics)
                              (loop-finish))
                            (push
                             (lisp-paren-check-diagnostic-create
                              :path file
                              :failure
                              (format nil
                                      "the file exceeds the per-file limit of ~:D characters"
                                      file-limit))
                             diagnostics)))
                       (t
                        (incf total-characters character-count)
                        (let ((language (lisp-source-balance-language file)))
                          (multiple-value-bind (issues count)
                              (lisp-source-balance--scan
                               language content :issue-limit detail-limit)
                            (incf checked-count)
                            (when (plusp count)
                              (incf issue-file-count)
                              (incf issue-count count)
                              (push
                               (lisp-paren-check-diagnostic-create
                                :path file
                                :language language
                                :issues issues
                                :issue-count count)
                               diagnostics)))))))
                 (error (condition)
                   (push
                    (lisp-paren-check-diagnostic-create
                     :path file
                     :failure
                     (format nil
                             "the file could not be safely read and checked: ~A"
                             (lisp-paren-check--single-line condition 300)))
                    diagnostics)))))
    (lisp-paren-check-run-create
     :candidate-count candidate-count
     :checked-count checked-count
     :issue-file-count issue-file-count
     :issue-count issue-count
     :diagnostics (nreverse diagnostics))))


;;;; -- Model-Facing Result --

(-> lisp-paren-check--path-label (pathname (option pathname)) string)
(defun lisp-paren-check--path-label (path root)
  "Return bounded single-line PATH, relative to directory ROOT when supplied."
  (lisp-paren-check--single-line
   (if root
       (enough-namestring path root)
       (namestring path))
   400))

(-> lisp-paren-check--bounded-result (string) string)
(defun lisp-paren-check--bounded-result (content)
  "Return CONTENT within the exact lisp.paren-check result-character limit."
  (let* ((limit (max 0 *lisp-paren-check-result-character-limit*))
         (marker "... lisp.paren-check diagnostic output truncated.")
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

(-> lisp-paren-check--bounded-line (t) string)
(defun lisp-paren-check--bounded-line (value)
  "Return VALUE as one control-free line within the exact result limit."
  (let* ((limit (max 0 *lisp-paren-check-result-character-limit*))
         (marker "... lisp.paren-check result truncated.")
         (text (lisp-paren-check--single-line value (1+ limit))))
    (cond
      ((<= (length text) limit)
       text)
      ((zerop limit)
       "")
      ((<= limit (length marker))
       (subseq marker 0 limit))
      (t
       (concatenate 'string
                    (subseq text 0 (- limit (length marker)))
                    marker)))))

(-> lisp-paren-check-error-message (lisp-paren-check-error) string)
(defun lisp-paren-check-error-message (condition)
  "Return CONDITION's bounded, control-free failure message."
  (lisp-paren-check--bounded-line
   (slot-value condition 'message)))

(-> lisp-paren-check--result-content
    (pathname (option pathname) lisp-paren-check-run)
    string)
(defun lisp-paren-check--result-content (path root run)
  "Return one bounded model-facing summary and exact retained diagnostics for RUN."
  (let* ((candidate-count (lisp-paren-check-run-candidate-count run))
         (checked-count (lisp-paren-check-run-checked-count run))
         (issue-file-count (lisp-paren-check-run-issue-file-count run))
         (issue-count (lisp-paren-check-run-issue-count run))
         (not-checked-count (- candidate-count checked-count))
         (diagnostics (lisp-paren-check-run-diagnostics run))
         (report-limit (max 0 *lisp-paren-check-reported-file-limit*))
         (reported-count (min report-limit (length diagnostics)))
         (reported (subseq diagnostics 0 reported-count)))
    (lisp-paren-check--bounded-result
     (with-output-to-string (stream)
       (format stream "Path: ~A~%" (lisp-paren-check--path-label path nil))
       (format stream
               "Checked ~:D of ~:D recognized Lisp-family file~:P.~%"
               checked-count
               candidate-count)
       (when (plusp issue-count)
         (format stream
                 "Found ~:D unmatched or mismatched delimiter~:P in ~:D file~:P.~%"
                 issue-count
                 issue-file-count))
       (when (plusp not-checked-count)
         (format stream
                 "Could not check ~:D file~:P.~%"
                 not-checked-count))
       (when (lisp-paren-check-run-success-p run)
         (write-string "No unmatched or mismatched delimiters found." stream))
       (dolist (diagnostic reported)
         (let ((failure (lisp-paren-check-diagnostic-failure diagnostic)))
           (format stream
                   "~2%~A~%"
                   (lisp-paren-check--path-label
                    (lisp-paren-check-diagnostic-path diagnostic)
                    root))
           (if failure
               (format stream "- not checked: ~A.~%" failure)
               (let* ((language
                        (lisp-paren-check-diagnostic-language diagnostic))
                      (issues
                        (lisp-paren-check-diagnostic-issues diagnostic))
                      (count
                        (lisp-paren-check-diagnostic-issue-count diagnostic)))
                 (format stream
                         "~A, ~:D issue~:P~%"
                         (lisp-source-balance--language-name language)
                         count)
                 (dolist (issue issues)
                   (write-line
                    (lisp-source-balance--issue-description issue)
                    stream))
                 (when (> count (length issues))
                   (format stream
                           "- ... ~:D additional issue~:P omitted.~%"
                           (- count (length issues))))))))
       (when (> (length diagnostics) reported-count)
         (format stream
                 "~2%... ~:D additional file diagnostic~:P omitted.~%"
                 (- (length diagnostics) reported-count)))))))


;;;; -- Public Path Checking --

(-> lisp-paren-check-path
    (pathname &key (:readable-roots list))
    (values boolean string))
(defun lisp-paren-check-path (path &key readable-roots)
  "Check recognized Lisp-family source at PATH.

Return a success boolean and bounded model-facing report. READABLE-ROOTS fixes
canonical filesystem authority for the complete operation. Failures before
candidate checking, including unsafe root changes and traversal-limit violations,
signal LISP-PAREN-CHECK-ERROR. Individual source read and snapshot failures are
retained as failed file diagnostics in the returned report."
  (let ((readable-roots
          (lisp-paren-check--canonical-readable-roots readable-roots)))
    (if (not (probe-file path))
        (values
         nil
         (lisp-paren-check--bounded-line
          (format nil "~A does not exist." path)))
        (multiple-value-bind (canonical stat)
            (lisp-paren-check--observe-path path readable-roots)
          (cond
            ((lisp-paren-check--directory-p stat)
             (multiple-value-bind (files canonical-root)
                 (lisp-paren-check--directory-files
                  path
                  :canonical-root
                  (uiop:ensure-directory-pathname canonical)
                  :root-stat stat
                  :readable-roots readable-roots)
               (if (null files)
                   (values
                    nil
                    (lisp-paren-check--bounded-line
                     (format nil
                             "No recognized Common Lisp, Scheme, or Clojure files were found under ~A."
                             path)))
                   (let* ((run
                            (lisp-paren-check--check-files
                             files
                             :canonical-root canonical-root
                             :readable-roots readable-roots))
                          (content
                            (lisp-paren-check--result-content
                             path
                             (uiop:ensure-directory-pathname path)
                             run)))
                     (values (lisp-paren-check-run-success-p run) content)))))
            ((not (lisp-paren-check--regular-file-p stat))
             (values
              nil
              (lisp-paren-check--bounded-line
               (format nil "~A is not a regular file." path))))
            ((null (lisp-source-balance-language path))
             (values
              nil
              (lisp-paren-check--bounded-line
               (format nil
                       "~A is not a recognized Common Lisp, Scheme, or Clojure source file."
                       path))))
            (t
             (let* ((run
                      (lisp-paren-check--check-files
                       (list path) :readable-roots readable-roots))
                    (content
                      (lisp-paren-check--result-content path nil run)))
               (values (lisp-paren-check-run-success-p run) content))))))))
