;;;; Copyright (c) 2008, Maciej Pasternacki <maciekp@japhy.fnord.org>
;;;; All rights reserved.  This file is available on the terms
;;;; detailed in COPYING file included with it.

(in-package #:curly)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '#:fiveam))

(def-suite curly
    :description "test suite for Curly")
(in-suite curly)

(test curlylist
  (is (equalp (copy-tree '(foo bar)) (curlylist 'foo 'bar)))
  (is (equalp (copy-tree '(foo bar)) (curlylist (copy-tree '(foo)) 'bar)))
  (is (equalp (copy-tree '(foo bar baz)) (curlylist (copy-tree '(foo bar)) 'baz)))
  (is (equalp (copy-tree '(foo bar (baz quux))) (curlylist (copy-tree '(foo bar)) (copy-tree '(baz quux)))))
  (is (equalp (copy-tree '(foo bar baz)) (curlylist (copy-tree '(foo bar *)) 'baz)))
  (is (equalp (copy-tree '(foo baz bar)) (curlylist (copy-tree '(foo * bar)) 'baz)))
  (is (equalp (copy-tree '(* foo bar baz)) (curlylist (copy-tree '(* foo bar)) 'baz))))

(test make-curly-readtable
  (let ((rt (make-curly-readtable)))
    (is (eq (get-macro-character #\{ rt) #'curly-reader))
    (is (eq (get-macro-character #\[ rt) #'square-reader))
    (is (eq (get-macro-character #\} rt) (get-macro-character #\) rt)))
    (is (eq (get-macro-character #\] rt) (get-macro-character #\) rt)))))


(defun eqlambda (body form
                 &aux
                 (arg (first (second form)))
                 (had-a-blank nil))
  (flet ((eq-with-blank (a b)
           (or (eq a b)
               (prog1 (and (eq a *blank-argument*)
                           (eq b arg)
                           (not had-a-blank))
                 (setf had-a-blank t)))))

      (and (eq 'lambda (first form))
           (listp (second form))
           (= 1 (length (second form)))
           (tree-equal body (third form) :test #'eq-with-blank))))

(test curly-simple-composition
  (let ((*readtable* (make-curly-readtable)))
    (signals error (read-from-string "{}"))
    (is (eqlambda (copy-tree '(foo *)) (read-from-string "{foo}")))
    (is (eqlambda (copy-tree '(foo (bar *))) (read-from-string "{foo bar}")))
    (is (eqlambda (copy-tree '(foo (bar (baz *)))) (read-from-string "{foo bar baz}")))
    (is (eqlambda (copy-tree '(foo (bar (baz (quux *))))) (read-from-string "{foo bar baz quux}")))))

(test curly-simple-curry
  (let ((*readtable* (make-curly-readtable)))
    (is (eqlambda (copy-tree '(foo *)) (read-from-string "{(foo)}")))
    (is (eqlambda (copy-tree '(foo bar *)) (read-from-string "{(foo bar)}")))
    (is (eqlambda (copy-tree '(foo bar baz *)) (read-from-string "{(foo bar baz)}")))))

(test curly-complex-curry
  (let ((*readtable* (make-curly-readtable)))
    (is (eqlambda (copy-tree '(foo bar baz *)) (read-from-string "{(foo bar baz *)}")))
    (is (eqlambda (copy-tree '(foo bar * baz)) (read-from-string "{(foo bar * baz)}")))
    (is (eqlambda (copy-tree '(foo * bar baz)) (read-from-string "{(foo * bar baz)}")))))

(test curly-curry-with-star-as-function
  (let ((*readtable* (make-curly-readtable)))
        (is (eqlambda (copy-tree '(* foo bar baz *)) (read-from-string "{(* foo bar baz)}")))
    (is (eqlambda (copy-tree '(* foo bar baz *)) (read-from-string "{(* foo bar baz *)}")))
    (is (eqlambda (copy-tree '(* foo bar * baz)) (read-from-string "{(* foo bar * baz)}")))
    (is (eqlambda (copy-tree '(* foo * bar baz)) (read-from-string "{(* foo * bar baz)}")))
    (is (eqlambda (copy-tree '(* * foo bar baz)) (read-from-string "{(* * foo bar baz)}")))))

(test curly-composition-with-curry
  (let ((*readtable* (make-curly-readtable)))
    (is (eqlambda (copy-tree '(foo (bar baz (quux *)))) (read-from-string "{foo (bar baz) quux}")))
    (is (eqlambda (copy-tree '(foo (bar baz xyzzy (quux *)))) (read-from-string "{foo (bar baz xyzzy) quux}")))
    (is (eqlambda (copy-tree '(foo (bar baz xyzzy (quux *)))) (read-from-string "{foo (bar baz xyzzy *) quux}")))
    (is (eqlambda (copy-tree '(foo (bar baz (quux *) xyzzy))) (read-from-string "{foo (bar baz * xyzzy) quux}")))
    (is (eqlambda (copy-tree '(foo (bar (quux *) baz xyzzy))) (read-from-string "{foo (bar * baz xyzzy) quux}")))
    (is (eqlambda (copy-tree '(foo (* bar baz xyzzy (quux *)))) (read-from-string "{foo (* bar baz xyzzy) quux}")))
    (is (eqlambda (copy-tree '(foo (* bar baz (quux *) xyzzy))) (read-from-string "{foo (* bar baz * xyzzy) quux}")))))

(test bracket-simple-curry
  (let ((*readtable* (make-curly-readtable)))
    (signals error (read-from-string "[]"))
    (is (eqlambda (copy-tree '(foo *)) (read-from-string "[foo]")))
    (is (eqlambda (copy-tree '(foo bar *)) (read-from-string "[foo bar]")))
    (is (eqlambda (copy-tree '(foo bar baz *)) (read-from-string "[foo bar baz]")))))

(test bracket-complex-curry
  (let ((*readtable* (make-curly-readtable)))
    (is (eqlambda (copy-tree '(foo bar baz *)) (read-from-string "[foo bar baz *]")))
    (is (eqlambda (copy-tree '(foo bar * baz)) (read-from-string "[foo bar * baz]")))
    (is (eqlambda (copy-tree '(foo * bar baz)) (read-from-string "[foo * bar baz]")))))

(test bracket-curry-with-star-as-function
  (let ((*readtable* (make-curly-readtable)))
        (is (eqlambda (copy-tree '(* foo bar baz *)) (read-from-string "[* foo bar baz]")))
    (is (eqlambda (copy-tree '(* foo bar baz *)) (read-from-string "[* foo bar baz *]")))
    (is (eqlambda (copy-tree '(* foo bar * baz)) (read-from-string "[* foo bar * baz]")))
    (is (eqlambda (copy-tree '(* foo * bar baz)) (read-from-string "[* foo * bar baz]")))
    (is (eqlambda (copy-tree '(* * foo bar baz)) (read-from-string "[* * foo bar baz]")))))

(test curly-syntax-disabled-before
  (is (eq (intern "{}") (read-from-string "{}")))
  (is (eq (intern "[]") (read-from-string "[]")))
  (is (eq (intern (format nil "{~A}" '#:foo)) (read-from-string "{foo}")))
  (is (eq (intern (format nil "[~A]" '#:foo)) (read-from-string "[foo]"))))

(enable-curly-syntax)

(test curly-syntax-enabled
  (is (eqlambda (copy-tree '(foo (bar (baz *)))) '{foo bar baz}))
  (is (eqlambda (copy-tree '(foo (bar baz (quux *) xyzzy))) '{foo (bar baz * xyzzy) quux}))
  (is (eqlambda (copy-tree '(foo bar *)) '[foo bar]))
  (is (eqlambda (copy-tree '(foo * bar)) '[foo * bar])))

(disable-curly-syntax)

(test curly-syntax-disabled-after
  (is (eq (intern "{}") (read-from-string "{}")))
  (is (eq (intern "[]") (read-from-string "[]")))
  (is (eq (intern (format nil "{~A}" '#:foo)) (read-from-string "{foo}")))
  (is (eq (intern (format nil "[~A]" '#:foo)) (read-from-string "[foo]"))))
