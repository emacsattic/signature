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

(provide 'signature-interface)
