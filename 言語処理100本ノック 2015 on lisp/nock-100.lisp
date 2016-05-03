(ql:quickload :cl21)
(defpackage nock-100 (:use :cl21 :cl21.re))
(in-package :nock-100)

(use-package :cl21)
(use-package :cl21.re)

(defun zip (&rest lists)
  (apply #'map #'list lists))

(defun preprocess (sentence)
  (map ^(re-replace #/[\.,?]/ % "") (split #\Space sentence)))
