
(defun change-func (var begin end &rest)
  "does stuff"
  (setq contents (buffer-substring-no-properties begin end))
  (set var
	(mapcar 'symbol-value '(begin end contents))
	)
  (print before-change)
  (print after-change)
  )

(add-hook 'before-change-functions
	  (apply-partially 'change-func 'before-change)
	  nil
	  t)
(add-hook 'after-change-functions
	  (apply-partially 'change-func 'after-change)
	  nil
	  t)
