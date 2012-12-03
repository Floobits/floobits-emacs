(require 'json)

(setq floobits-agent-version "0.01")
(setq floobits-agent-host "localhost")
(setq floobits-agent-port 4567)
(setq floobits-change-set ())
(setq floobits-agent-buffer "")

(defcustom floobits-username "ggreer"
  "Username for floobits"
  :type 'string
  )

(defcustom floobits-secret "1234"
  "Secret for floobits"
  :type 'string
  )

(defcustom floobits-room "test"
  "Room for floobits"
  :type 'stringq
  )

(defcustom floobits-share-dir "~/share"
  "Room for floobits"
  :type 'string
  )

(defun floobits-switch (text)
  (print text))
;  (let* ((json-key-type 'string) 
;	 (req (json-read-from-string text)))
;;    (message "%s" req)))

(defun floobits-listener(process response)
  (setq floobits-agent-buffer (concat floobits-agent-buffer response))
  (let ((position (search "\n" floobits-agent-buffer)))
       (if (not (eq nil position))
	   (progn (print position)
		  (floobits-switch (substring floobits-agent-buffer 0 position))
		  (setq floobits-agent-buffer 
			(substring floobits-agent-buffer 
				   (if (> (length floobits-agent-buffer) position) (+ 1 position) position)))
		  (floobits-listener process "")))))

(defun floobits-auth()
  (let ((req (list `(name . auth)
	     `(username . ,floobits-username)
	     `(room . ,floobits-room)
	     `(secret . ,floobits-secret))))
    (send-to-agent req)))

(defun create-connection()
  (setq floo (open-network-stream "floobits" nil floobits-agent-host floobits-agent-port))
  (set-process-coding-system floo 'utf-8 'utf-8)
  (set-process-filter floo 'floobits-listener)
  (floobits-auth))
  
(defun change-func (var begin end &optional &rest old_length)
  "does stuff"
  (setq contents (buffer-substring-no-properties begin end))
  (set var
       (mapcar 'symbol-value '(begin end contents old_length))))

(defun send-to-agent (req)
  (add-to-list 'req (cons 'version floobits-agent-version))
  (process-send-string floo (concat (json-encode req) "\n")))

(defun get-text (begin end)
  (buffer-substring-no-properties begin end))

(defun before-change (begin end)
  (let ((text (get-text begin end)))
    (add-to-list 'floobits-change-set (cons 'before `(,begin ,end ,text)))))

(defun after-change (begin end old_length)
  (let ((text (get-text begin end)))
    (add-to-list 'floobits-change-set (cons 'after `(,begin ,end ,text)))
    (send-to-agent floobits-change-set)
  (setq floobits-change-set)))

(defun after-new-buffer ()
  (let ((req '("event" "new-buffer")))
    (add-to-list 'req (cons 'text (get-text point-min point-max)))
    (add-to-list 'req (cons 'path (file-name-directory load-file-name)))
    (send-to-agent req)))

(add-hook 'before-change-functions 'before-change nil t)
(add-hook 'after-change-functions 'after-change nil t)
(add-hook 'find-file-hook 'after-new-buffer nil t)

(create-connection)