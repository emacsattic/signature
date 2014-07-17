;;; signature-backend.el --- Basic wireing

;;; Commentary:

;; Reading of files, determining which parser to use, picking matcher
;; for current source line and building signature using the available
;; machinery. this is where it comes to gether.

;;; Code:

(cl-defmacro signature-with-source-file ((file) &body body)
 "Executes BODY as lines from FILE with the variable LINES bound to the lines of the file."
 `(with-temp-buffer
   (insert-file-contents ,file)
   (let ((lines (split-string (buffer-string) "\n" t)))
    ,@body)))

(cl-defmacro signature-with-source-lines ((line) &body body)
 "Iterate over LINES binding each one to LINE and executing BODY."
 `(dolist (,line lines)
   ,@body))

(defun signature-parser-for-file (file)
 "Determine parser (language implementation) by matching extension of FILE with the languages defined extension."
 (cl-find-if
  (lambda (language)
   (with-slots (extension) language
    (s-suffix-p extension file)))
  signature-languages))

(defun signature-match (parser line)
 "Given a language PARSER and source code LINE, return a matcher."
 (cl-find-if
  (lambda (matcher)
   (with-slots (regexp) matcher
    (string-match-p regexp line)))
  (slot-value parser 'source-line-matchers)))

(defun signature-class-p (instance)
 "Determine if INSTANCE is a subclass of class symbol `signature-match-class'."
 (child-of-class-p (class-of instance) 'signature-match-class))

(defun signature-method-p (instance)
 "Determine if INSTANCE is a subclass of class symbol `signature-match-method'."
 (child-of-class-p (class-of instance) 'signature-match-method))

(defun signature--parse-file (file)
 "Parse a FILE, returning a list of statistics and an ascii signature."
 (let ((parser (signature-parser-for-file file))
       (class-count 0)
       (method-count 0)
       (line-count 0)
       (stack nil)
       (signature-string

        (with-output-to-string
         (signature-with-source-file (file)
          (signature-with-source-lines (line)
            
           (let ((matcher (signature-match parser line)))
            (when matcher

             ;; Statistics:
             (incf line-count)
             (when (signature-class-p matcher) (incf class-count))
             (when (signature-method-p matcher) (incf method-count))

             ;; Signature production:
             (cond
              ((signature-push-state-p matcher stack)
               (princ (signature-marker-enter matcher))
               (push matcher stack))
              ((signature-pop-state-p matcher stack)
               (when stack
                (princ (signature-marker-exit (pop stack)))))
              (t (princ (signature-marker matcher)))))))))))

  (list class-count method-count line-count signature-string)))

(provide 'signature-backend)

;;; signature-backend.el ends here
