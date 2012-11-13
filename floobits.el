
(defun change-func (var begin end old_length)
  "does stuff"
  (setq contents (buffer-substring-no-properties begin end))
  (set var
	(mapcar 'symbol-value '(begin end contents))
	)
  (print (append (symbol-value var) old_length))
  )

(add-hook 'after-change-functions
	  (apply-partially 'change-func 'after-change)
	  nil
	  t)
