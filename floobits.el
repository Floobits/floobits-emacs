(defun test-func (begin end)
  "does stuff"
  (message "%s:%s" begin end)
)


(add-hook 'before-change-functions
	  'test-func
	  nil
	  t)




