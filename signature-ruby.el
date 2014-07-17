;;; signature-ruby.el --- A signature language definition for Ruby

;;; Commentary:

;; A rather happy case implementation of a ruby language parser
;; missing many bits and pieces but providing the basics:

;;; Code:

(defclass signature-ruby-class (signature-match-class)
 ((regexp :initform "^\s?+class\s+[a-z]+")))

(defclass signature-ruby-method (signature-match-method)
 ((regexp :initform "^\s?+def\s+[a-z]+")))

(defclass signature-ruby-comment (signature-match-comment)
 ((regexp :initform "^\s?+#")))

(defclass signature-ruby-switch (signature-match-conditional)
 ((regexp :initform "^\s?+case\s?+")))

(defclass signature-ruby-conditional (signature-match-conditional)
 ((regexp :initform "^\s?+\\(if\\|when\\|unless\\)")))

(defclass signature-ruby-loop (signature-match-loop)
 ((regexp :initform "\\(for .* in\\|\.each\\| do$\\)")))

(defclass signature-ruby-end (signature-source-line-pop-scope)
 ((regexp :initform "^\s?+end\s?+$")))

(defclass signature-ruby (signature-language)
 ((name :initform "Ruby")
  (extension :initform "rb")
  (source-line-matchers
   :initform
   (list
    (make-instance 'signature-ruby-class)
    (make-instance 'signature-ruby-method)
    (make-instance 'signature-ruby-switch)
    (make-instance 'signature-ruby-conditional)
    (make-instance 'signature-ruby-loop)
    (make-instance 'signature-ruby-end)
    (make-instance 'signature-ruby-comment)
    (make-instance 'signature-match-any)))))

(defmethod signature-push-state-p ((m signature-ruby-conditional) stack)
 "Do not push to stack for conditionals when STACK head contains
a signature-ruby-switch. We loose a bit precision put gain the
ability to match the case statements corresponding end."
 (or (null stack) (not (eql (class-of (car stack)) signature-ruby-switch))))

;; Push definition onto the signature-languages list:

(push (make-instance 'signature-ruby) signature-languages)

(provide 'signature-ruby)

;;; signature-ruby.el ends here
