;;;; Copyright (c) 2008, Maciej Pasternacki <maciekp@japhy.fnord.org>
;;;; All rights reserved.  This file is available on the terms
;;;; detailed in COPYING file included with it.

(defpackage #:curly
  (:use #:common-lisp)
  (:export #:*blank-argument* #:curly-reader #:square-reader
           #:make-curly-readtable
           #:enable-curly-syntax #:disable-curly-syntax))
(in-package #:curly)

(defvar *blank-argument* '*
  "Symbol used to indicate argument place in curly syntax.")

(defun curlylist (item rest)
  (cond
    ((symbolp item) (list item rest))
    ((null (rest item)) (list (car item) rest))
    (t (let ((blank (position *blank-argument* (rest item))))
         (if blank
             (replace item (list rest) :start1 (1+ blank))
             (nconc item (list rest)))))))

(defun curly-reader (stream char
                     &aux (funs (read-delimited-list #\} stream t)))
  "Reader macro for #\{ character."
  (declare (ignore char))
  (when (null funs)
    (error "Empty curly braces."))
  (let ((arg (gensym)))
    `(lambda (,arg)
       ,(if (null (rest funs))
            (curlylist (first funs) arg)
            (reduce #'curlylist funs :from-end t :initial-value arg)
           ))))

(defun square-reader (stream char
                      &aux (funs (read-delimited-list #\] stream t)))
  "Reader macro for #\[ character."
  (declare (ignore char))
  (when (null funs)
    (error "Empty square braces."))
  (let ((arg (gensym)))
    `(lambda (,arg)
       ,(curlylist funs arg) )))

(defun make-curly-readtable (&optional (original-readtable *readtable*)
                             &aux (rv (copy-readtable original-readtable)))
  "Return new readtable with curly syntax enabled."
  (set-macro-character #\{ #'curly-reader nil rv)
  (set-macro-character #\[ #'square-reader nil rv)
  (set-syntax-from-char #\} #\) rv)
  (set-syntax-from-char #\] #\) rv)
  rv)

(defvar *original-readtable* nil
  "Original readtable to restore, used by ENABLE/DISABLE-CURLY-SYNTAX.")

(defmacro enable-curly-syntax ()
  "Enable curly syntax for current file."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *original-readtable* *readtable*
          *readtable* (curly:make-curly-readtable))))

(defmacro disable-curly-syntax ()
  "Disable curly syntax for current file.

Warning: Calling DISABLE-CURLY-SYNTAX when curly syntax is not
enabled can give funny results.  Also, reading multiple files
using ENABLE-CURLY-SYNTAX and DISABLE-CURLY-SYNTAX in different
threads can invoke a disaster.  ENABLE-CURLY-SYNTAX itself is
safe."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless *original-readtable*
       (error "Curly syntax not enabled."))
     (setf *readtable* *original-readtable*
           *original-readtable* nil)))
