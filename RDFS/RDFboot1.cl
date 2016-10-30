;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdf Boot module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007-2010 Seiji Koide
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfboot0))

(in-package :gx)

;; redefined rdfsClass (TODO: SBCL crashed here)
(defclass rdfsClass (rdfs:|Class|) ()
  (:metaclass rdf-node)
  (:documentation "This is the proxy of rdfs:|Class| in order to make the membership loop."))

;; Now we got a twisted relation between rdfs:|Class| and rdfsClass.

;;;
;;;; rdfs:|Resource|
;;; At initial stage of booting, kernel classes are defined without slots to let class-changing easy.

(defclass rdfs:|Resource| (gnode) ()
  (:metaclass rdfs:|Class|)
  (:documentation "Every resource in RDF(S) universe including classes is an instance of 
rdfs:|Resource|."))

(defclass rdfs:|Class| (rdfs:|Resource| rdf-node) ()
  (:metaclass rdfsClass)
  (:documentation "This is rdfs:|Class|, and it is a class of all classes in RDF(S) universe."))

(defparameter rdfs:|Class|
  (find-class 'rdfs:|Class|)
  "This is rdfs:|Class| and it is a class of all classes in RDF(S) universe.")

(defparameter rdfs:|Resource|
  (find-class 'rdfs:|Resource|)
  "rdfs:|Resource| is the top class in the RDF universe, but subclass of gnode actually.")

(defmethod print-object ((obj rdfs:|Resource|) stream)
  (cond ((not (slot-exists-p obj 'name))
         (call-next-method))
        ((and (slot-boundp obj 'name)
              (slot-value obj 'name))
         (print-unreadable-object (obj stream :type t)
           (prin1 (slot-value obj 'name) stream)))
        (t (print-unreadable-object (obj stream :type t)
             (prin1 :anonymous stream)))))

(cl:provide :rdfboot1)
