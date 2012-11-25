(require 'json)

(setq floobits-change-set '((before . '(-1 -1 "")) (after . '(-1 -1 ""))))


(defun floobits-listener(process response)
  (print response))

(defun create-connection()
  (setq floo (open-network-stream "floobits" nil "localhost" 4567))
  (set-process-coding-system pserv 'utf-8 'utf-8)
  (set-process-filter floo 'floobits-listener))

(defun change-func (var begin end &optional &rest old_length)
  "does stuff"
  (setq contents (buffer-substring-no-properties begin end))
  (set var
	(mapcar 'symbol-value '(begin end contents old_length))))

(add-hook 'before-change-functions 
	   (lambda (begin end) (put 'floobits-change-set 'before '(begin end (buffer-substring-no-properties begin end))) (message (symbol-plist 'floobits-change-set )))
	   nil
	   t)
	  
(add-hook 'after-change-functions
	  (lambda (begin end old_length) (setq floobits-after-change (buffer-substring-no-properties begin end))
	    (process-send-string floo (json-encode floobits-change-set)))
	  nil
	  t)

(create-connection)