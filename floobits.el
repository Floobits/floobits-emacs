(defun change-func (begin end)
  "does stuff"
  (setq val (buffer-substring-no-properties begin end))
  (setq previous-state 
	(mapcar 
	   'symbol-value
	 '(begin end val)
	 )
	)
)

(add-hook 'before-change-functions
	  'change-func
	  nil
	  t)

(add-hook 'after-change-functions
	  'change-func
	  nil
	  t)
