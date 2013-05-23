(require 'cl)
(require 'json)
(require 'url)

(setq floobits-agent-version "0.01")
(setq floobits-agent-host "localhost")
(setq floobits-agent-port 4567)
(setq floobits-change-set ())
(setq floobits-agent-buffer "")
(setq floobits-conn nil)
(setq max-specpdl-size 1500)
(setq floobits-current-position '((mark . 1) (point . 1) (name . "")))
(setq floobits-open-buffers nil)
(setq floobits-follow-mode nil)
; ; To set this: M-x customize-variable RET floobits-username
; (defcustom floobits-username ""
;   "Username for floobits"
;   :type 'string)

; ; To set this: M-x customize-variable RET floobits-secret
; (defcustom floobits-secret ""
;   "Secret for floobits"
;   :type 'string)

; (defcustom floobits-share-dir "~/share"
;   "Room for floobits"
;   :type 'string)

(defmacro floo-get-item (alist key)
  "just grab an element from an alist"
  (list 'cdr (list 'assoc-string key alist)))

(defmacro floo-set-item (alist key value)
  "set an element in an alist"
  (list 'add-to-list alist (list 'cons key value)))

(defun floobits-load-floorc ()
  "loads floorc file vars"
  (condition-case nil
    (progn
      (with-temp-buffer
        (insert-file-contents "~/.floorc")
        (goto-char 1)
        (let ((strings (split-string (buffer-string) "\n" t)))
          (loop for s in strings do
            (let ((substrings (split-string s " " t)))
              (set (intern (concat "floobits-" (car substrings))) (cadr substrings)))))))
  (error nil)))

(defun floobits-post-command-func ()
  "used for grabbing changes in point for highlighting"
  (let* ((name (buffer-name (current-buffer)))
    (current (list
      (cons 'point (or (point) -1))
      (cons 'mark (or (mark) -1))
      (cons 'name (or name "")))))
    (unless (equal current floobits-current-position)
      (setq floobits-current-position current)
      (let* ((mark (floo-get-item current 'mark))
        (req (list
          (cons 'ranges (list (list mark mark)))
          (cons 'full-path (buffer-file-name (current-buffer))))))
        (floobits-send-to-agent req 'highlight)))))

(defun floobits-filter-func (condp lst)
  (delq nil
  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun floobits-launch-agent ()
  (when (boundp 'floobits-python-agent)
    (kill-process floobits-python-agent)
    (delete-process floobits-python-agent))
  ; Assumes floobits.el is in the same dir as floobits.py
  (start-process "floobits-python-agent" "*Messages*" (concat (file-name-directory load-file-name) "floobits.py")))

(defun floobits-follow-mode-toggle ()
  "Toggles following of recent changes in a room"
  (interactive)
  (setq floobits-follow-mode (not floobits-follow-mode))
  (message "Follow mode %s." (if (eq floobits-follow-mode nil) "disabled" "enabled")))

(defun floobits-leave-room ()
  "leaves the current rooom"
  (interactive)
  (floobits-destroy-connection))

(defun floobits-join-room (floourl)
  "Join a floobits room"
  (interactive (list (read-from-minibuffer "Floobits room URL (owner/room): " "https://floobits.com/r/")))
  (floobits-load-floorc)
  (if (or (not (boundp 'floobits-username)) (string= "" floobits-username))
    (error "Floobits username not found. Please define a username and secret in ~/.floorc"))
  (if (or (not (boundp 'floobits-secret)) (string= "" floobits-secret))
    (error "Floobits secret not found. Please define a username and secret in ~/.floorc"))
  (let* ((url-struct (url-generic-parse-url floourl))
    (domain (url-host url-struct))
    (port (url-port url-struct))
    (path (url-filename url-struct))
    (path
      (if (string= "/" (substring path -1))
        (concat path "")
        (concat path "/")))
    (_ (string-match "^/r/\\(.*\\)/\\(.*\\)/" path))
    (owner (match-string 1 path))
    (room (match-string 2 path)))
    (if (and path room owner)
      (progn
        (floobits-destroy-connection)
        (setq floobits-room room)
        (setq floobits-room-owner owner)
        (floobits-create-connection))
      (message "Invalid url! I should look like: https://floobits.com/r/owner/room/"))))

(defun _floobits-is-buffer-public(buf)
  (let ((name (buffer-name buf)))
    (cond
      ((string="*" (substring name 0 1)) nil)
      ((string=" " (substring name 0 1)) nil)
      ((buffer-file-name buf) t)
      (t nil))))

; TODO: make this function work, then actually use it
(defun _floobits-is-buffer-shared(buf)
  (let ((name (buffer-file-name buf))
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
  (message "Successfully joined room %s" floobits-room)
  (message "project path is %s" (floo-get-item req 'project_path))
  (dired (floo-get-item req "project_path")))

(defun floobits-event-join (req)
  (message "%s" req)
  (message "%s joined the room"  (floo-get-item req 'username)))

(defun floobits-event-part (req)
  (message "%s" req)
  (message "%s left the room" (floo-get-item req 'username)))

(defun floobits-event-create_view (req)
  (message "opening file %s" (floo-get-item req 'full_path))
  (find-file (floo-get-item req 'full_path)))

(defun floobits-event-highlight (req)
  (when 'floobits-follow-mode
    (message "opening file %s" (floo-get-item req 'full_path))
    (find-file (floo-get-item req 'full_path))
    (let*
      ((ranges (floo-get-item req 'ranges))
      (ranges-length (- (length ranges) 1)))
        (goto-char (elt (elt ranges ranges-length) 0)))))

(defun floobits-apply-edit (edit)
  (let* ((inhibit-modification-hooks t)
    (edit-start (+ 1 (elt edit 0)))
    (edit-length (elt edit 1))
    (edit-end (min (+ 1 (buffer-size)) (+ edit-start edit-length))))
    (delete-region edit-start edit-end)
    (when (eq 3 (length edit))
      (goto-char edit-start)
      (insert (elt edit 2)))))

(defun floobits-event-edit (req)
  (let* ((filename (floo-get-item req "full_path"))
    (buf (get-file-buffer filename))
    (edits (floo-get-item req "edits")))
    (if buf
      (with-current-buffer buf
        (save-excursion
          (atomic-change-group
            (mapcar 'floobits-apply-edit edits)))))))

(defun floobits-event-create_buf (req)
  (let ((filename (floo-get-item req "path" ))
        (username (floo-get-item req "username")))
    (message "User %s created buffer %s" username filename)))

(defun floobits-event-delete_buf (req)
  (let ((filename (floo-get-item req "path" ))
        (username (floo-get-item req "username")))
    (message "User %s deleted buffer %s" username filename)))

(defun floobits-event-get_buf (req)
  (let ((filename (floo-get-item req "full_path" )))
    (if (not (eq filename nil))
      (when floobits-follow-mode
        (find-file filename))
    (message "filename does not exist for buffer %s" (floo-get-item req 'id)))))

(defun floobits-switch (text)
  (message "%s" text)
  (let* ((json-key-type 'string)
    (req (json-read-from-string text))
    (event (floo-get-item req "name"))
    (func (concat "floobits-event-" event)))
    (if (fboundp (intern-soft func))
      (funcall (read func) req)
      (message "func %s doesn't exist" func))))

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
  (when floobits-conn
    (message "deleting floobits conn")
    (delete-process floobits-conn)))

(defun floobits-send-to-agent (req event)
  (floo-set-item 'req 'name event)
  (floo-set-item 'req 'version floobits-agent-version)
  (process-send-string floobits-conn (concat (json-encode req) "\n")))

(defun floobits-get-text (begin end)
  (buffer-substring-no-properties begin end))

(defun floobits-before-change (begin end)
  (if (eq (_floobits-is-buffer-public (current-buffer)) t)
    (let ((text (floobits-get-buffer-text (current-buffer))))
      (floo-set-item 'floobits-change-set 'before text))))

(defun floobits-after-change (begin end old_length)
  (if (eq (_floobits-is-buffer-public (current-buffer)) t)
     (let ((text (floobits-get-buffer-text (current-buffer))))
      (floo-set-item 'floobits-change-set 'after text)
      (floo-set-item 'floobits-change-set 'full_path (buffer-file-name (current-buffer)))
      (floobits-send-to-agent floobits-change-set 'change)
    (setq floobits-change-set))))

(defun floobits-after-new-buffer ()
  (let ((req '("event" "new-buffer")))
    (floo-set-item 'req 'text (floobits-get-text point-min point-max))
    (floo-set-item 'req 'path (file-name-directory load-file-name))
    (floobits-send-to-agent req 'new-buffer)))

(defun floobits-buffer-list-change ()
  (let* ((current-buffers (mapcar 'buffer-file-name (floobits-get-public-buffers)))
      (added (set-difference current-buffers floobits-open-buffers))
      (deleted (set-difference floobits-open-buffers current-buffers)))
    (when (or added deleted)
      (setq floobits-open-buffers current-buffers)
      (let* (
          (added-text
            (mapcar
              (lambda (buf-path)
                (cons (intern buf-path)
                  (floobits-get-buffer-text (find-buffer-visiting buf-path))))
            added))
        (req (list
        (cons 'current current-buffers)
        (cons 'added added-text)
        (cons 'deleted deleted))))
        (floobits-send-to-agent req 'buffer_list_change)))))

;;(add-hook 'before-change-functions 'before-change nil nil)
(add-hook 'after-change-functions 'floobits-after-change nil nil)
(add-hook 'post-command-hook 'floobits-buffer-list-change nil nil)
;(add-hook 'kill-buffer-hook 'floobits-buffer-list-change nil nil)
;;(add-hook 'post-command-hook 'floobits-post-command-func nil nil)
;(floobits-launch-agent)
;deleted (#<killed buffer>)
