(defvar floobits-agent-port 9999
    "port of the floobits agent")

(defvar floobits-agent-host "127.0.0.1"
    "host of the floobits agent")

(defun connect-to-agent nil
    "connects to the agent"
    (interactive)
    (make-network-process :name "floobits" :buffer "*floobits*" :family 'ipv4 :host connect-to-agent :service floobits-agent-port :sentinel 'floobits-sentinel :filter 'floobits-filter ) 
    )

(defun floobits-sentinel (proc msg)
  (message msg proc)
  )

(defun floobits-filter (proc string)   
  (message string)
  )

(defun floobits-stop nil
  "stop an emacs tcp listener"
  (interactive)
  (delete-process "listen")
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
