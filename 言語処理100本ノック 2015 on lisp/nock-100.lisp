(ql:quickload :cl21)
(defpackage nock-100 (:use :cl21 :cl21.re))
(in-package :nock-100)

(use-package :cl21)
(use-package :cl21.re)

(defun zip (&rest lists)
  (apply #'map #'list lists))

(defun preprocess (sentence)
  (map ^(re-replace #/[\.,?]/ % "") (split #\Space sentence)))

(defun seqrnd (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (sort seq #'> :key (lambda (x) (random 1.0))))
