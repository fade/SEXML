;;; polymers.lisp --- 
;; 
;; Filename: polymers.lisp
;; Description: SEXML support for HTML5 html imports as per polymer-project.org
;; Author: Brian O'Reilly <fade@deepsky.com>
;; Maintainer: Brian O'Reilly <fade@deepsky.com>
;; Created: Mon Jun 17 14:38:02 2013 (-0400)
;; Version: 0.0.0
;; Last-Updated: Fri Jul  5 18:39:43 2013 (-0400)
;;           By: Brian O'Reilly
;;     Update #: 38
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(in-package :sexml)

;; file utilities

(defun find-html-targets (path)
  "given a base path, finds all the html files at or below path."
  (let ((path (cl-fad:canonical-pathname path)))
    (walk-directory path
                    #'(lambda (path)
                        (let ((outcome (cl-ppcre:all-matches-as-strings ".+html$" (file-namestring path))))
                          (if outcome
                              (format t  "~&~A" path)))))))

;; HTML utilities

(defun clean-html (path)
  (cxml:parse-file path (cxml-dom:make-dom-builder))
  ;; (chtml:parse string (cxml:make-string-sink))
  )

(defun get-node-data-as-list (path)
  "given the path of an file of html code, return a list representing
  the elements and attributes contained in the file."
  (cxml:parse-file path (cxml-xmls:make-xmls-builder)))

(defun klack-it (path)
  (klacks:with-open-source
    (s (cxml:make-source path))
    (loop
          :for key = (klacks:peek s)
          :while key
          :do
             ;; (format t "~&BOSTAGE:: ~A" key)
             (case key
                 (:start-element
                  (format t "~&~A {" (klacks:current-qname s)))
                 (:end-element
                  (format t "}")))
             (klacks:consume s))))

(defun read-html-file (path)
  (with-open-file (input path :direction :input)
    (let* ((len (file-length input))
           (data (make-string len)))
      (values data (read-sequence data input)))))

(defun parse-html-include (page)
  (chtml:parse (clean-html page) (cxml-stp:make-builder)))

(defun elements-and-attributes (page)
  (let ((data (parse-html-include page)))
    ()))

;; (stp:do-recursively (a *kk*)
;;          (cond ((typep a 'stp:element)
;;                 (format t "~&Element: ~A" (stp:local-name a)))
;;                ((typep a 'stp:attribute)
;;                 (format t "~&Has Attribute Named: ~A ~& \t With Value: ~A" (stp:attribute) (stp:attribute-value)))
;;                (t
;;                 (format t "Done Document."))))

(defun bung (doc)
  (stp:do-recursively (a doc)
         (cond
           ;; ((typep a 'stp:comment)
           ;;  (format t "~&Document Comment: ~A || ~A" (stp:local-name a) (stp:string-value a)))
           ((typep a 'stp:element)
            (format t "~&Element: ~A~%Value: ~A~%" (stp:local-name a) (stp:string-value a)))
           ;; ((typep a 'stp:element)
           ;;  (format t "~&Element: ~A~%~&Has Attributes Named: ~{ ~A~^ ~}" 
           ;;          (stp:local-name a) (stp:list-attributes a))) 
           ;; ((typep a 'stp:attribute)
           ;;  (format t "" 
           ;;          (stp:attribute a) (stp:attribute-value a)))
           )))

(defclass html-include (dtd)
  ((uri :initarg :uri :reader include-uri :initform nil))
  (:documentation "Datastructure which contains the tag and attribute
  information for an HTML include."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; polymers.lisp ends here
