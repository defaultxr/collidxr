;;;; collidxr.asd

(defsystem #:collidxr
  :name "collidxr"
  :description "modula t.'s extensions for cl-collider."
  :version "0.1"
  :author "modula t."
  :license "MIT"
  :homepage "https://github.com/defaultxr/collidxr"
  :bug-tracker "https://github.com/defaultxr/collidxr/issues"
  :mailto "modula-t at pm dot me"
  :source-control (:git "git@github.com:defaultxr/collidxr.git")
  :depends-on (#:alexandria
               #:mutility
               #:trivial-types
               #:cl-collider
               #:vgplot)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "collidxr"))
  :in-order-to ((test-op (test-op "collidxr/tests")))
  :perform (load-op :after (op c)
                    (mutility:auto-load-systems '(("cl-patterns" "cl-patterns/collidxr")))))

(defsystem #:collidxr/tests
  :name "collidxr/tests"
  :description "FiveAM-based test suite for collidxr."
  :author "modula t."
  :license "MIT"
  :depends-on (#:collidxr
               #:fiveam
               #:mutility/test-helpers)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "collidxr"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:collidxr-tests
                                                         :collidxr/tests))))
