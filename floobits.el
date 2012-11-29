(require 'json)

(setq floobits-change-set ())

(defun floobits-listener(process response)
  (print response))

(defun create-connection()
  (setq floo (open-network-stream "floobits" nil "localhost" 4567))
  (set-process-coding-system floo 'utf-8 'utf-8)
  (set-process-filter floo 'floobits-listener))

(defun change-func (var begin end &optional &rest old_length)
  "does stuff"
  (setq contents (buffer-substring-no-properties begin end))
  (set var
       (mapcar 'symbol-value '(begin end contents old_length))))

(defun get-text (begin end)
  (buffer-substring-no-properties begin end))

(defun before-change (begin end)
  (let ((text (get-text begin end)))
    (add-to-list 'floobits-change-set (cons 'before `(,begin ,end ,text)))))

(defun after-change (begin end old_length)
  (let ((text (get-text begin end)))
    (add-to-list 'floobits-change-set (cons 'after `(,begin ,end ,text)))
    (process-send-string floo (json-encode 'floobits-change-set)))
  (print floobits-change-set)
  (setq floobits-change-set nil))

(add-hook 'before-change-functions 
	  'before-change
	   nil
	   t)
	  
(add-hook 'after-change-functions
	  'after-change
	  nil
	  t)

(create-connection)