;;;; package.lisp

(uiop:define-package #:collidxr
  (:use)
  (:mix #:cl
        #:alexandria
        #:mutility
        #:trivial-types
        #:cl-collider)
  (:export #:plot

           #:b2
           #:fbn
           #:fbn-read
           #:fbn-write

           #:ds
           #:synth-variant

           #:dn))
