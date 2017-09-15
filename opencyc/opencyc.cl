;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;; Copyright (c) 2017  Chun Tian (University of Bologna, Italy)

(in-package :gx)

;; > (translate-logical-pathname #p"CYC:opencyc-latest.owlz")
;; #P"/Users/binghe/Lisp/SWCLOS/opencyc/opencyc-latest.owl.gz"
;; > (pathname-type *)
;; "owl.gz"

;; > (pathname-type #P"/Users/binghe/Lisp/SWCLOS/opencyc/opencyc-latest.owl.gz")
;; "gz"

(defpackage "cyc"
  (:nicknames "opencyc")
  (:use ) ; supressing using common lisp package
  (:documentation "http://sw.opencyc.org/concept/#"))

(eval-when (:execute :load-toplevel)
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package "cyc") t)))
    (find-package "cyc"))
  ;; maybe unnecessary
  (setf (uri-namedspace-package (set-uri-namedspace "http://sw.opencyc.org/concept/"))
	(find-package "cyc")))

(defun load-cyc ()
  (read-rdf-file #'add-rdf/xml #p"CYC:opencyc-latest.owlz"))
