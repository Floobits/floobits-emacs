(require 'cl)
(require 'json)
(require 'url)

(setq floobits-plugin-dir (file-name-directory load-file-name))
(add-to-list 'load-path floobits-plugin-dir)
(require 'highlight)

(setq max-specpdl-size 1500)

(setq floobits-agent-version "0.01")
(setq floobits-agent-host "localhost")
(setq floobits-agent-port 4567)
(setq floobits-python-path (concat floobits-plugin-dir "floobits.py"))

(defun floobits-initialize ()
  (setq floobits-change-set ())
  (setq floobits-agent-buffer "")
  (setq floobits-conn nil)
  (setq floobits-current-position '((mark . 1) (point . 1) (name . "")))
  (setq floobits-open-buffers nil)
  (setq floobits-follow-mode nil)
  (setq floobits-perms nil)
  (setq floobits-share-dir "")
  (setq floobits-user-highlights (make-hash-table :test 'equal)))

(floobits-initialize)

(defun floobits-add-hooks ()
  (add-hook 'after-change-functions 'floobits-after-change nil nil)
  (add-hook 'post-command-hook 'floobits-post-command-func nil nil)
  (ad-enable-advice 'delete-file 'before 'floobits-delete-file)
  (ad-enable-advice 'rename-file 'before 'floobits-rename-file)
  (ad-activate 'delete-file)
  (ad-activate 'rename-file))

(defun floobits-remove-hooks ()
  (remove-hook 'after-change-functions 'floobits-after-change)
  (remove-hook 'post-command-hook 'floobits-post-command-func)
  (ad-disable-advice 'delete-file 'before 'floobits-delete-file)
  (ad-disable-advice 'rename-file 'before 'floobits-rename-file))

(defadvice delete-file (before floobits-delete-file (name))
  (when (_floobits-is-path-shared name)
    (if (member "delete_buf" floobits-perms)
      (let ((req (list
            (cons 'path name))))
        (floobits-send-to-agent req 'delete_buf))
      (message "You don't have permission to delete buffers in this room."))))

(defadvice rename-file (before floobits-rename-file
    (old-name new-name &optional OK-IF-ALREADY-EXISTS))
  (when (_floobits-is-path-shared old-name)
    (if (member "rename_buf" floobits-perms)
      (let ((req (list
            (cons 'path new-name)
            (cons 'old_path old-name))))
        (floobits-send-to-agent req 'rename_buf))
      (message "You don't have permission to rename buffers in this room."))))

(defun floobits-process-live-p (process)
  "Returns non-nil if PROCESS is alive.
  A process is considered alive if its status is `run', `open',
  `listen', `connect' or `stop'."
  (memq (process-status process)
    '(run open listen connect stop)))

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
  (error nil))
  (if (or (not (boundp 'floobits-username)) (string= "" floobits-username))
    (error "Floobits username not found. Please define a username and secret in ~/.floorc"))
  (if (or (not (boundp 'floobits-secret)) (string= "" floobits-secret))
    (error "Floobits secret not found. Please define a username and secret in ~/.floorc")))

(defun floobits-listener (process response)
  (setq floobits-agent-buffer (concat floobits-agent-buffer response))
  (let ((position (search "\n" floobits-agent-buffer)))
    (when position
      (floobits-switch (substring floobits-agent-buffer 0 position))
      (setq floobits-agent-buffer
      (substring floobits-agent-buffer
        (if (> (length floobits-agent-buffer) position) (+ 1 position) position)))
      (floobits-listener process ""))))

(defun floobits-create-connection ()
  (floobits-launch-agent)
  (setq floobits-conn (open-network-stream "floobits" nil floobits-agent-host floobits-agent-port))
  (set-process-coding-system floobits-conn 'utf-8 'utf-8)
  (process-kill-without-query floobits-conn)
  (set-process-filter floobits-conn 'floobits-listener))

(defun floobits-destroy-connection ()
  (when floobits-conn
    (message "Destroying Floobits conn")
    (floobits-remove-hooks)
    (delete-process floobits-conn)
    (delete-process floobits-python-agent)
    (floobits-initialize)
    (setq floobits-python-agent nil)
    (message "")))

(defun floobits-filter-func (condp lst)
  (delq nil
  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun floobits-agent-listener (proc string)
  ; todo: check for 'started'
  ; (when (buffer-live-p (process-buffer proc))
  (with-current-buffer "*Messages*"
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
       ;; Insert the text, advancing the process marker.
       (goto-char (process-mark proc))
       (insert (concat "floobits agent says: " string))
       (set-marker (process-mark proc) (point)))
     (if moving (goto-char (process-mark proc))))))

(defun floobits-launch-agent ()
  (condition-case nil
    (progn
      (delete-process floobits-python-agent))
    (error nil))
  (message "Launching Floobits python agent...")
  (setq floobits-python-agent (start-process "" "*Messages*" floobits-python-path))
  ; (set-process-filter floobits-python-agent 'floobits-agent-listener)
  (accept-process-output floobits-python-agent 2)
  (process-kill-without-query floobits-python-agent))

(defun floobits-send-to-agent (req event)
  (if (floobits-process-live-p floobits-conn)
    (progn
      (floo-set-item 'req 'name event)
      (floo-set-item 'req 'version floobits-agent-version)
      (process-send-string floobits-conn (concat (json-encode req) "\n")))
    (progn
      (message "connection to floobits died :(")
      (floobits-destroy-connection))))

(defun floobits-get-text (begin end)
  (buffer-substring-no-properties begin end))

(defun floobits-post-command-func ()
  "used for grabbing changes in point for highlighting"
  (floobits-buffer-list-change)
  (floobits-send-highlight))

(defun floobits-event-user_input (req)
  (floo-set-item 'req 'response (read-from-minibuffer (floo-get-item req 'prompt) (floo-get-item req 'initial)))
  (floobits-send-to-agent req 'user_input))

(defun floobits-event-rename_buf (req)
  (let* ((old-path (floo-get-item req 'old_path))
         (new-path (floo-get-item req 'path))
         (buf (get-file-buffer old-path)))
    (message "renaming %s to %s" old-path new-path)
    (rename-file old-path new-path 1)
    (when buf
      (with-current-buffer buf
        (rename-buffer new-path)
        (set-visited-file-name new-path)
        (set-buffer-modified-p nil)))))

(defun floobits-send-highlight (&optional ping)
  (when (eq (_floobits-is-buffer-public (current-buffer)) t)
    (let* ((name (buffer-file-name (current-buffer)))
           (mark (or (mark) -1))
           (current (list
             (cons 'point (or (point) -1))
             (cons 'mark mark)
             (cons 'name (or name "")))))
      (unless (or ping (equal current floobits-current-position))
        (setq floobits-current-position current)
        (let ((req (list
            (cons 'ranges (vector (vector mark mark)))
            (cons 'full_path name)
            (cons 'ping ping))))
          (floobits-send-to-agent req 'highlight))))))

(defun floobits-summon ()
  "Summons all users to your cursor position."
  (interactive)
  (floobits-send-highlight t))

(defun floobits-follow-mode-toggle ()
  "Toggles following of recent changes in a room"
  (interactive)
  (when floobits-conn
    (setq floobits-follow-mode (not floobits-follow-mode))
    (floobits-send-to-agent (list (cons 'follow_mode floobits-follow-mode)) 'set_follow_mode)
    (message "Follow mode %s." (if (eq floobits-follow-mode nil) "disabled" "enabled"))))

(defun floobits-leave-room ()
  "leaves the current rooom"
  (interactive)
  (floobits-destroy-connection))

(defun floobits-share-dir (dir-to-share)
  "Create a room and populate it with a directory"
  (interactive "GGive me a directory: ")
  (floobits-load-floorc)
  (floobits-destroy-connection)
  (floobits-create-connection)
  (let ((req (list
    (cons 'username floobits-username)
    (cons 'secret floobits-secret)
    (cons 'dir_to_share dir-to-share))))
    (floobits-send-to-agent req 'share_dir)))

(defun floobits-event-error (req)
  (message-box (floo-get-item req 'msg)))

(defun floobits-join-room (floourl)
  "Join a floobits room"
  (interactive (list (read-from-minibuffer "Floobits room URL (owner/room): " "https://floobits.com/r/")))
  (floobits-load-floorc)
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
        (floobits-create-connection)
        (let ((req (list
          (cons 'username floobits-username)
          (cons 'room floobits-room)
          (cons 'secret floobits-secret)
          (cons 'room_owner owner))))
          (floobits-send-to-agent req 'join_room)))
      (message "Invalid url! I should look like: https://floobits.com/r/owner/room/"))))

(defun _floobits-is-buffer-public (buf)
  (let ((name (buffer-name buf)))
    (cond
      ((string="*" (substring name 0 1)) nil)
      ((string=" " (substring name 0 1)) nil)
      ((_floobits-is-buffer-shared buf) t)
      (t nil))))

(defun _floobits-is-path-shared (path)
  (let ((length (length floobits-share-dir)))
    (cond
     ((eq 0 length) nil)
     ((< (length path) length) nil)
     ((string= floobits-share-dir (substring path 0 length)) t)
     (t nil))))

(defun _floobits-is-buffer-shared (buf)
  (_floobits-is-path-shared (buffer-file-name buf)))

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
  (setq floobits-room (floo-get-item req 'room_name))
  (message "Successfully joined room %s" floobits-room)
  (setq floobits-share-dir (floo-get-item req 'project_path))
  (message "project path is %s" floobits-share-dir)
  (setq floobits-perms (append (floo-get-item req 'perms) nil))
  (floobits-add-hooks)
  (dired floobits-share-dir))

(defun floobits-event-join (req)
  (message "%s" req)
  (message "%s joined the room"  (floo-get-item req 'username)))

(defun floobits-event-part (req)
  (message "%s" req)
  (message "%s left the room" (floo-get-item req 'username)))

(defun floobits-event-create_view (req)
  (message "opening file %s" (floo-get-item req 'full_path))
  (find-file (floo-get-item req 'full_path))
  (floobits-buffer-list-change))

(defun floobits-event-focus (req)
  (find-file (floo-get-item req 'full_path))
  (goto-char (+ 1 (floo-get-item req 'offset))))

(defun floobits-clear-highlights ()
  "Clears all highlights"
  (interactive)
  (maphash
    (lambda (key highlight)
      (with-current-buffer (get-file-buffer (cadr key))
        (save-excursion
          (goto-char 0)
          (push-mark (buffer-size))
          (hlt-unhighlight-region))))
    floobits-user-highlights))

(defun floobits-apply-highlight (user_id buffer ranges)
  (let* ((key (list user_id (buffer-file-name buffer)))
         (previous-ranges (gethash key floobits-user-highlights)))
    (message "%s key %s" key previous-ranges)
    (with-current-buffer buffer
      (save-excursion
        (when previous-ranges
          ; convert to list :(
          (mapcar
            (lambda(x)
              (let ((start (min (buffer-size buffer) (+ (elt x 0) 1)))
                    (end (+ (elt x 1) 2)))
                (goto-char start)
                (push-mark end t t))
                (hlt-unhighlight-region))
            previous-ranges))
        (mapcar
          (lambda(x)
            (let ((start (min (buffer-size buffer) (+ (elt x 0) 1)))
                  (end (+ (elt x 1) 2)))
              (goto-char start)
              (push-mark end t t))
              (hlt-highlight-region))
          ranges)
        (puthash key ranges floobits-user-highlights)))))

(defun floobits-event-highlight (req)
  (let* ((ranges (floo-get-item req 'ranges))
         (ranges-length (- (length ranges) 1))
         (buffer (get-file-buffer (floo-get-item req 'full_path)))
         (user_id (floo-get-item req 'user_id)))
  (floobits-apply-highlight user_id buffer ranges)
  (when floobits-follow-mode
    (switch-to-buffer buffer)
    (goto-char (+ 1 (elt (elt ranges ranges-length) 0))))))

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
        (username (floo-get-item req "username")))))

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

(defun floobits-after-change (begin end old_length)
  (if (eq (_floobits-is-buffer-public (current-buffer)) t)
     (let ((text (floobits-get-buffer-text (current-buffer))))
      (floo-set-item 'floobits-change-set 'after text)
      (floo-set-item 'floobits-change-set 'full_path (buffer-file-name (current-buffer)))
      (floobits-send-to-agent floobits-change-set 'change)
    (setq floobits-change-set))))

(defun floobits-buffer-list-change ()
  (let* ((current-buffers (mapcar 'buffer-file-name (floobits-get-public-buffers)))
      (added (set-difference current-buffers floobits-open-buffers))
      (deleted (set-difference floobits-open-buffers current-buffers)))
    (when (or added deleted)
      (when (and added (not (member "patch" floobits-perms)))
        (mapcar
          (lambda (buf-path)
            (with-current-buffer (find-buffer-visiting buf-path)
              (setq buffer-read-only t)))
          added))
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
