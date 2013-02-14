(require 'json)
(require 'url)

(setq floobits-agent-version "0.01")
(setq floobits-agent-host "localhost")
(setq floobits-agent-port 4567)
(setq floobits-change-set ())
(setq floobits-agent-buffer "")

(setq floobits-conn nil)

(defcustom floobits-username "kans"
  "Username for floobits"
  :type 'string)

(defcustom floobits-secret "1aiwkewqzwauexwnmqk9u9q3c"
  "Secret for floobits"
  :type 'string)

(defcustom floobits-share-dir "~/share"
  "Room for floobits"
  :type 'string)

(defmacro floo-get-item (alist key)
  "just grab an element from an alist"
  (list 'cdr (list 'assoc key alist)))

(defun floobits-filter-func (condp lst)
  (delq nil
  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun floobits-join-room (floourl)
"Join a floobits room"
  (interactive (list (read-from-minibuffer "Floobits room URL (owner/room): " "https://floobits.com/r/")))
  (let* ((url-struct (url-generic-parse-url floourl))
    (domain (url-host url-struct))
    (port (url-port url-struct))
    (path (url-filename url-struct))
    (_ (string-match "^/r/\\(.*\\)/\\(.*\\)$" path))
    (owner (match-string 1 path))
    (room (match-string 2 path)))
    (print (list path room owner))
    (if (and room owner)
      (progn
        (setq floobits-room room)
        (setq floobits-room-owner owner)
        (floobits-destroy-connection)
        (floobits-create-connection))
    (message "Invalid url! I should look like: https://floobits.com/r/owner/room"))))

(defun _floobits-is-buffer-public(buf)
  (let ((name (buffer-name buf)))
    (cond
     ((string="*" (substring name 0 1)) nil)
     ((string=" " (substring name 0 1)) nil)
     ((< (length name) 11) t)
     ((string= "floobits.el" (substring name 0 11)) nil)
     (t t))))

(defun _floobits-is-buffer-shared(buf)
  (let ((name (buffer-name buf))
  (length (length floobits-share-dir)))
    (cond
     ((not (boundp floobits-share-dir)) nil)
     ((< (length name) length) nil)
     (string= floobits-share-dir (substring name 0 length) t)
     (t nil))))

(defun floobits-get-public-buffers ()
  "returns buffers that aren't internal to emacs"
  (floobits-filter-func '_floobits-is-buffer-public (buffer-list)))

(defun floobits-get-buffer-text (buffer)
  "returns properties free text of buffer with name (name)"
  (with-current-buffer buffer)
  ; (with-current-buffer (set-buffer (get-buffer-create name))
    (buffer-substring-no-properties (point-min) (point-max)))

(defun floobits-event-disconnect (req)
  (message "Disconnected: %s" (floo-get-item req 'reason)))

(defun floobits-event-room_info (req)
   (message "Successfully joined room %s" floobits-room))

(defun floobits-event-join (req)
  (message "%s joined the room"  (floo-get-item req 'username)))

(defun floobits-event-part (req)
  (message "%s left the room" (floo-get-item req 'username)))

(defun floobits-event-edit (req)
  (let* ((filename (cdr (assoc "full_path" req)))
    (buf (get-file-buffer filename))
    (edits (cdr (assoc "edits" req)))
    (apply-edit (lambda (edit)
      (let* ((inhibit-modification-hooks t)
        (edit-start (+ 1 (elt edit 0)))
        (edit-length (elt edit 1))
        (edit-end (min (+ 1 (buffer-size)) (+ edit-start edit-length))))
        (delete-region edit-start edit-end)
        (when (eq 3 (length edit))
          (goto-char edit-start)
          (insert (elt edit 2)))))))
    (if buf
      (with-current-buffer buf
        (save-excursion
          (atomic-change-group
            (mapcar apply-edit edits)))))))

(defun floobits-event-get_buf (req)
  (let ((filename (cdr (assoc "full_path" req))))
    (if (not (eq filename nil))
      (find-file filename)
    (message "filename does not exist for buffer %s" (floo-get-item req 'id)))))

(defun floobits-switch (text)
  (let* ((json-key-type 'string)
   (req (json-read-from-string text))
   (event (cdr (assoc "name" req)))
   (func (concat "floobits-event-" event)))
    (funcall (read func) req)))

(defun floobits-listener (process response)
  (setq floobits-agent-buffer (concat floobits-agent-buffer response))
  (let ((position (search "\n" floobits-agent-buffer)))
    (when position
      (floobits-switch (substring floobits-agent-buffer 0 position))
      (setq floobits-agent-buffer
      (substring floobits-agent-buffer
        (if (> (length floobits-agent-buffer) position) (+ 1 position) position)))
      (floobits-listener process ""))))

(defun floobits-auth ()
  (let ((req (list
    (cons 'username floobits-username)
    (cons 'room floobits-room)
    (cons 'secret floobits-secret)
    (cons 'room_owner floobits-room-owner))))
    (floobits-send-to-agent req 'auth)))

(defun floobits-create-connection ()
  (setq floobits-conn (open-network-stream "floobits" nil floobits-agent-host floobits-agent-port))
  (set-process-coding-system floobits-conn 'utf-8 'utf-8)
  (set-process-filter floobits-conn 'floobits-listener)
  (floobits-auth))

(defun floobits-destroy-connection ()
  (when (and (boundp 'floobits-conn) floobits-conn)
    (message "deleting floobits conn")
    (setq floobits-conn nil)))

; (defun floobits-change-func (var begin end &optional &rest old_length)
;   "does stuff"
;   (setq contents (buffer-substring-no-properties begin end))
;   (set var
;     (mapcar 'symbol-value '(begin end contents old_length))))

(defun floobits-send-to-agent (req event)
  (add-to-list 'req (cons 'version floobits-agent-version))
  (add-to-list 'req (cons 'name event))
  (process-send-string floobits-conn (concat (json-encode req) "\n")))

(defun floobits-get-text (begin end)
  (buffer-substring-no-properties begin end))

(defun floobits-before-change (begin end)
  (if (eq (_floobits-is-buffer-public (current-buffer)) t)
    (let ((text (floobits-get-buffer-text (current-buffer))))
      (add-to-list 'floobits-change-set (cons 'before text)))))

(defun floobits-after-change (begin end old_length)
  (if (eq (_floobits-is-buffer-public (current-buffer)) t)
     (let ((text (floobits-get-buffer-text (current-buffer))))
      (add-to-list 'floobits-change-set (cons 'after text))
      (add-to-list 'floobits-change-set (cons 'full_path (buffer-file-name (current-buffer))))
      (floobits-send-to-agent floobits-change-set 'change)
    (setq floobits-change-set))))

(defun floobits-after-new-buffer ()
  (let ((req '("event" "new-buffer")))
    (add-to-list 'req (cons 'text (floobits-get-text point-min point-max)))
    (add-to-list 'req (cons 'path (file-name-directory load-file-name)))
    (floobits-send-to-agent req 'new-buffer)))

;;(add-hook 'before-change-functions 'before-change nil nil)
(add-hook 'after-change-functions 'floobits-after-change nil nil)
(add-hook 'find-file-hook 'floobits-after-new-buffer nil t)
