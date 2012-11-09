(defun test-func (begin end)
  "does stuff"
  (print (buffer-substring-no-properties begin end))
)

(add-hook 'before-change-functions
	  'test-func
	  nil
	  t)









