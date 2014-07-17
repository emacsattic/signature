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

(provide 'signature-api)
