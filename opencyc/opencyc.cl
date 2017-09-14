(in-package :gx)

;; > (translate-logical-pathname #p"CYC:opencyc-latest.owlz")
;; #P"/Users/binghe/Lisp/SWCLOS/opencyc/opencyc-latest.owl.gz"
;; > (pathname-type *)
;; "owl.gz"

;; > (pathname-type #P"/Users/binghe/Lisp/SWCLOS/opencyc/opencyc-latest.owl.gz")
;; "gz"

(defpackage cyc
  (:nicknames opencyc "opencyc" "cyc" cycAnnot "cycAnnot")
  (:use ))

(eval-when (:load-toplevel)
  (setf (uri-namedspace-package (set-uri-namedspace "http://sw.opencyc.org/concept/"))
	(find-package :cyc))
  (setf (uri-namedspace-package (set-uri-namedspace "http://sw.opencyc.org/concept/#"))
	(find-package :cyc)))

(defun load-opencyc ()
  (read-rdf-file #'add-rdf/xml #p"CYC:opencyc-latest.owlz"))
