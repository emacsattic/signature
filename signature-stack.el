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

(provide 'signature-stack)
