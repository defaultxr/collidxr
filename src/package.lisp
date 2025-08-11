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

           #:ds
           #:synth-variant

           #:dn))
