;;; signature.el --- Signature survey

;; Copyright (C) 2014  Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 0.1
;; Package-Requires ((cl-lib "0.5") (s "1.9.0") (f "0.16.2"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate a signature survery from your code

;;; Code:

(require 'eieio)
(require 'cl)
(require 's)
(require 'f)

(require 'signature-ruby)

;; General language machinery

(defvar signature-languages nil)

(defclass signature-language ()
 ((name :initarg :name)
  (extension :initarg :extension)
  (source-line-matchers :initarg :source-line-matchers)))

(defclass signature-source-line-matcher ()
 ((regexp :initarg :regexp)))

(defclass signature-source-line-push-scope (signature-source-line-matcher) ())
(defclass signature-source-line-pop-scope (signature-source-line-matcher) ())
(defclass signature-source-line-no-scope (signature-source-line-matcher) ())

(defclass signature-match-class (signature-source-line-push-scope) ())
(defclass signature-match-conditional (signature-source-line-push-scope) ())
(defclass signature-match-loop (signature-source-line-push-scope) ())
(defclass signature-match-method (signature-source-line-push-scope) ())
(defclass signature-match-comment (signature-source-line-no-scope) ())
(defclass signature-match-any (signature-source-line-no-scope)
 ((regexp :initform "[a-z]+")))

(defmethod signature-marker ((m signature-source-line-matcher))
 "How to denote a comment."
 "")

(defmethod signature-marker ((m signature-match-comment))
 "How to denote a comment."
 "'")

(defmethod signature-marker ((m signature-match-any))
 "How to denote anything."
 ".")

(defmethod signature-marker-enter ((m signature-match-conditional))
 "Signature character representing entering a conditional."
 "(")

(defmethod signature-marker-exit ((m signature-match-conditional))
 "Signature character representing exiting a conditional."
 ")")

(defmethod signature-marker-enter ((m signature-match-class))
 "Signature character representing entering a class."
 "<")

(defmethod signature-marker-exit ((m signature-match-class))
 "Signature character representing exiting a class."
 ">")

(defmethod signature-marker-enter ((m signature-match-method))
 "Signature character representing entering a method."
 "{")

(defmethod signature-marker-exit ((m signature-match-method))
 "Signature character representing exiting a method."
 "}")

(defmethod signature-marker-enter ((m signature-match-loop))
 "Signature character representing entering a loop."
 "[")

(defmethod signature-marker-exit ((m signature-match-loop))
 "Signature character representing exiting a loop."
 "]")

(defgeneric signature-push-state-p (matcher &optional stack)
 "Generic function to decide wether the line pushes a new scope to the stack.")

(defgeneric signature-pop-state-p (matcher &optional stack)
 "Generic function to decide wether the line pops a scope off the stack.")

(defmethod signature-push-state-p ((m signature-source-line-matcher) &optional stack)
 "Default is not to push onto the stack."
 nil)

(defmethod signature-push-state-p ((m signature-source-line-push-scope) &optional stack)
 "Classes inheriting from signature-source-line-push-scope push onto the stack."
 t)

(defmethod signature-pop-state-p ((m signature-source-line-matcher) &optional stack)
 "Default is not to pop the stack."
 nil)

(defmethod signature-pop-state-p ((m signature-source-line-pop-scope) &optional stack)
 "Classes inheriting from signature-soure-line-pop-scope pop the stack."
 t)

(cl-defmacro signature-with-source-file ((file) &body body)
 `(with-temp-buffer
   (insert-file-contents ,file)
   (let ((lines (split-string (buffer-string) "\n" t)))
    ,@body)))

(cl-defmacro signature-with-source-lines ((line) &body body)
 `(dolist (,line lines)
   ,@body))

(defun signature-parser-for-file (file)
 (cl-find-if
  (lambda (language)
   (with-slots (extension) language
    (s-suffix-p extension file)))
  signature-languages))

(defun signature-match (parser line)
 (cl-find-if
  (lambda (matcher)
   (with-slots (regexp) matcher
    (string-match-p regexp line)))
  (slot-value parser 'source-line-matchers)))

(defun signature--parse-file (file)
 (let* ((class-count 0)
        (method-count 0)
        (line-count 0)
        (signature-string
         (with-output-to-string
          (let ((parser (signature-parser-for-file file)))
           (signature-with-source-file (file)
            (let (stack)
             (signature-with-source-lines (line)
              (let ((matcher (signature-match parser line)))
               (when matcher

                ;; Statistics
                (incf line-count)
                (when (child-of-class-p (class-of matcher) 'signature-match-class)
                 (incf class-count))
                (when (child-of-class-p (class-of matcher) 'signature-match-method)
                 (incf method-count))

                ;; Stack handling and signature production
                (cond

                 ((signature-push-state-p matcher stack)
                  (princ (signature-marker-enter matcher))
                  (push matcher stack))

                 ((signature-pop-state-p matcher stack)
                  (when stack
                   (princ (signature-marker-exit (pop stack)))))

                 (t
                  (princ (signature-marker matcher)))))))))))))

  (list class-count method-count line-count signature-string)))

;;;###autoload
(defun signature-report ()
 "Report on signatures for files matching a glob."
 (interactive)
 (with-current-buffer (get-buffer-create "*signature*")
  (erase-buffer)
  (let* ((glob (read-string "Glob: "))
         (boring-prefix (expand-file-name (car (split-string glob "*")))))
   (dolist (file (f-glob glob))
    (insert (s-chop-prefix boring-prefix file))
    (newline)
    (insert (apply 'format "%dc\t%dm\t%dL\n%s" (signature--parse-file file)))
    (newline)
    (newline))
   (switch-to-buffer "*signature*"))))

(provide 'signature)
;;; signature.el ends here
