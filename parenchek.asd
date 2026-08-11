(asdf:defsystem #:parenchek
  :description "A library for checking Lisp-family delimiter balance."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:sb-posix
               #:serapeum)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "analysis")
                             (:file "check"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:parenchek/tests))))

(asdf:defsystem #:parenchek/tests
  :description "Tests for Parenchek."
  :depends-on (#:parenchek)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:parenchek/tests '#:run-tests)))
