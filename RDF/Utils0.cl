(in-package :gx)

;;;"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; A specific facility for temporary strings.  This could be replaced by
;;; a general STACK-LET facility if we write something like that.

(defresource temporary-string
             (&key (length 100) (adjustable t))
  :constructor (make-array length
                           :element-type 'character
                           :fill-pointer 0
                           :adjustable adjustable)
  :matcher (and (eq adjustable (adjustable-array-p temporary-string))
                (or (and (not adjustable)
                         (= length (array-dimension temporary-string 0)))
                    (<= length (array-dimension temporary-string 0))))
  :initializer (setf (fill-pointer temporary-string) 0))

(defun temporary-string-p (string)
  (with-resource-rd ('temporary-string RD)
    (dovector (ts (rd-objects RD))
      (when (eq (os-object ts) string)
        (return-from temporary-string-p t)))))

(defmacro with-temporary-string ((var &key (length 100) (adjustable t)) &body body)
  `(using-resource (,var temporary-string :length ,length :adjustable ,adjustable)
     ,@body))

(defmacro evacuate-temporary-string (string-var)
  `(if (temporary-string-p ,string-var)
       (make-array (length ,string-var)
                   :element-type 'character
                   :fill-pointer (length ,string-var)
                   :initial-contents ,string-var)
       ,string-var))

;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;; Activation gestures terminate the entire input line.  They are usually
;; non-printing characters such as #\Newline or #\End.
(defvar *activation-gestures* nil)

(defvar *standard-activation-gestures* '(:newline :return :end))

(defun activation-gesture-p (gesture)
  (and (not (typep gesture '(or pointer-event noise-string end-of-file-marker)))
       (dolist (set *activation-gestures*)
         (when (if (listp set)
                   (member gesture set
                           :test #'keyboard-event-matches-gesture-name-p)
                   (funcall set gesture))
           (return-from activation-gesture-p t)))))

;; Delimiter gestures terminate a field in an input line.  They are usually
;; printing characters such as #\Space or #\Tab
(defvar *delimiter-gestures* nil)

(defun delimiter-gesture-p (gesture)
  (and (not (typep gesture '(or pointer-event noise-string end-of-file-marker)))
       (dolist (set *delimiter-gestures*)
         (when (if (listp set)
                   (member gesture set
                           :test #'keyboard-event-matches-gesture-name-p)
                   (funcall set gesture))
           (return-from delimiter-gesture-p t)))))

;;; Reading and writing of tokens

(defparameter *quotation-character* #\")

;; READ-TOKEN reads characters until it encounters an activation gesture,
;; a delimiter character, or something else (like a mouse click).
(defun read-token (stream &key input-wait-handler pointer-button-press-handler
                               click-only timeout)
  (with-temporary-string (string :length 50 :adjustable t)
    (let* ((gesture nil)
           (gesture-type nil)
           (quote-seen nil)
           (old-delimiters *delimiter-gestures*)
           (*delimiter-gestures* *delimiter-gestures*))
      (flet ((return-token (&optional unread)
               (when unread
                 (unread-gesture unread :stream stream))
               (when (and (activation-gesture-p unread)
                          (input-editing-stream-p stream))
                 (rescan-if-necessary stream))
               (return-from read-token
                 (values (evacuate-temporary-string string)))))
        (loop
          (multiple-value-setq (gesture gesture-type)
            (stream-read-gesture stream
				 :input-wait-handler
				 (or input-wait-handler
				     *input-wait-handler*)
				 :pointer-button-press-handler
				 (or pointer-button-press-handler
				     *pointer-button-press-handler*)
				 :timeout (and click-only timeout)))
          (cond ((eq gesture-type :timeout)
                 (return-from read-token :timeout))
                ((and click-only (eq gesture :eof))
                 (error "Got an EOF when waiting for mouse input"))
                ((eq gesture :eof)
                 (return-token gesture))
                ((and click-only
                      (not (typep gesture 'pointer-button-event)))
                 (beep stream))
                ((typep gesture 'pointer-button-event)
                 ;; No need to do anything, since this should have been handled
                 ;; in the presentation type system already
                 )
                ((characterp gesture)
                 (cond ((and (zerop (fill-pointer string))
                             (eql gesture *quotation-character*))
                        (setq quote-seen t)
                        (setq *delimiter-gestures* nil))
                       ((and quote-seen
                             (eql gesture *quotation-character*))
                        (setq quote-seen nil)
                        (setq *delimiter-gestures* old-delimiters))
                       ((activation-gesture-p gesture)
                        (return-token gesture))
                       ((delimiter-gesture-p gesture)
                        ;; ditto?
                        (return-token gesture))
                       ((or (ordinary-char-p gesture)
                            (diacritic-char-p gesture))
                        (vector-push-extend gesture string)
                        ;;--- haven't updated WRITE-CHAR yet
                        #+++ignore (write-char gesture stream))
                       (t (beep stream))))
                ((activation-gesture-p gesture)
                 (return-token gesture))
                ((delimiter-gesture-p gesture)
                 (return-token gesture))
                (t (beep stream))))))))


;;; from CCL (l1-reader.lisp)

(defun %return-token-string (token)
  (let* ((str (token.string token))
         (pool %token-strings%))
    (setf (token.string token) nil)
    (without-interrupts
     (setf (pool.data pool)
           (cheap-cons str (pool.data pool))))))

;;;Look for an exact match, else create a simple-string.

(defun %get-token-string (len)
  (declare (fixnum len))
  (without-interrupts
   (do* ((pool %token-strings%)
         (head (cons nil (pool.data pool)))
         (prev head next)
         (next (cdr prev) (cdr next)))
        ((null next)
         (make-string len :element-type 'base-char))
     (declare (dynamic-extent head)
              (list head prev next))
     (let* ((s (car next)))
       (when (= len (length s))
         (rplacd prev (cdr next))
         (setf (pool.data pool) (cdr head))
         (free-cons next)
         (return s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-token-buffer ((name) &body body &environment env)
    (multiple-value-bind (body decls) (parse-body body env nil)
      `(let* ((,name (vector (%get-token-string 16) 0 0 16 nil)))
        (declare (dynamic-extent ,name))
        (unwind-protect
             (locally ,@decls ,@body)
          (%return-token-string ,name))))))

(defun %string-from-token (token)
  (let* ((opos (token.opos token))
         (ipos (token.ipos token))
         (tstr (token.string token))
         (len (the fixnum (- opos ipos)))
         (string (make-string len :element-type 'base-char)))
    (do* ((k 0 (1+ k))
          (i ipos (1+ i)))
         ((= i opos) string)
      (declare (fixnum i k))
      (setf (%schar string k) (%schar tstr i)))))

(defun %string-from-token (token)
  (let* ((opos (token.opos token))
         (ipos (token.ipos token))
         (tstr (token.string token))
         (len (the fixnum (- opos ipos)))
         (string (make-string len :element-type 'base-char)))
    (do* ((k 0 (1+ k))
          (i ipos (1+ i)))
         ((= i opos) string)
      (declare (fixnum i k))
      (setf (%schar string k) (%schar tstr i)))))

(defun %read-char-no-eof (stream)
  (read-char stream))

(defun read-string (stream termch)
  (with-token-buffer (tb)
    (do* ((attrs (rdtab.ttab *readtable*))
          (ch (%read-char-no-eof stream)
              (%read-char-no-eof stream)))
         ((eq ch termch)
          (%string-from-token tb))
      (if (= (the fixnum (%character-attribute ch attrs)) $CHT_SESC)
          (setq ch (%read-char-no-eof stream)))
      (%add-char-to-token ch tb))))
