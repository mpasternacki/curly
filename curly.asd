;;;; -*- lisp -*-

;;;; Copyright (c) 2008, Maciej Pasternacki <maciekp@japhy.fnord.org>
;;;; All rights reserved.  This file is available on the terms
;;;; detailed in COPYING file included with it.

(defpackage #:curly.system
  (:use #:common-lisp #:asdf))

(defsystem #:curly
  :name "Curly"
  :description "Reader macros for easy function currying and composition."
  :version "0.1"
  :author "Maciej Pasternacki <maciekp@japhy.fnord.org>"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :components ((:file "curly")))

(defsystem #:curly.test
  :description "Test suite for Curly"
  :author "Maciej Pasternacki <maciekp@japhy.fnord.org>"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :components ((:file "tests"))
  :depends-on (#:curly #:fiveam))


(defmethod perform ((op asdf:test-op) (system (eql (find-system :curly))))
  (asdf:oos 'asdf:load-op :curly.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           (intern (string :curly) (string :curly))))
