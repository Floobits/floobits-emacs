(require 'tcp-client)

(defun floobits-error-report(buffer server port error)
  (save-excursion
    (set-buffer buffer)
    (insert (format "[error] %s:%i -- %s\n" server port error)))
  )
     
(defun floobits-connection-report(buffer server port)
  (save-excursion
    (set-buffer buffer)
    (insert (format "[connect] %s:%i -- Connection established\n" server port))))

(defun floobits-abort-report(buffer server port)
  (save-excursion
    (set-buffer buffer)
    (insert (format "[error] %s:%i -- Abort connection\n" server port))))

(defun floobits-sentinel-report(process event)
  (save-excursion
    (set-buffer (process-buffer process))
    (insert (format "[event] Process: %s had the event -- %s" process event))))

(defun floobits-filter-report(process message)
  (save-excursion
    (set-buffer (process-buffer process))
    (insert (format "[got] %s" message))))
     
(tcp-connect "*floobits*" 
	     (make-new-tcp-connection :server "127.0.0.1" :port 4567)
	     (make-new-tcp-hooks 
	      :connection-failed-handler 'floobits-error-report
	      :connection-established-handler 'floobits-connection-report
	      :connection-abort-handler 'floobits-abort-report
	      :sentinel-handler 'floobits-sentinel-report
	      :filter-handler 'floobits-filter-report)
	     )

(defun change-func (var begin end &optional &rest old_length)
  "does stuff"
  (setq contents (buffer-substring-no-properties begin end))
  (set var
	(mapcar 'symbol-value '(begin end contents))
	)
  (print (append (symbol-value var) old_length))
  )

(add-hook 'before-change-functions
	  (apply-partially 'change-func 'before-change)
	  nil
	  t)

(add-hook 'after-change-functions
	  (apply-partially 'change-func 'after-change)
	  nil
	  t)
