;; GLOBALS

(defvar notes-directory-path "~/Documents/notes/" "The directory note-manager will look when performing note actions")

;; KEYBINDING

(defvar notes-mode-map (make-sparse-keymap)
  "Keymap for 'notes-mode'.
\\{notes-mode-map}")

(keymap-set notes-mode-map "C-c n c" 'create-note)
(keymap-set notes-mode-map "C-c n f" 'find-note-by-title)

;; SETUP MINOR MODE

(define-minor-mode notes-mode
  "Minor mode for managing notes"
  :lighter " Notes"
  :keymap notes-mode-map)

(string-search "title:" "title: dsd.org")
(split-string "title: dsd.org" ":")
(string-clean-whitespace "  fdsfd")

;; HELPERS

(defun parse-yaml-list (list-string)
  (let* ((unparsed-elements (split-string list-string ","))
         (build-list (lambda (list pos)
                       (if (nth pos unparsed-elements)
                           (let ((target-string (nth pos unparsed-elements)))
                             (when (string-match "\\\"\\([^\"]+\\)\\\"" target-string)
                               (setq list (cons (match-string-no-properties 1 target-string) list)))
                             (funcall build-list list (+ pos 1)))
                         (nreverse list)))))
    (funcall build-list '() 0)))

(defun note-has-tags-p (full-note-name)
  (let* ((tags (split-string (read-string "Tag(s)")))
         (full-note-path (concat note-directory full-note-name))
         (note-buffer (find-file full-note-path))
         (get-tag-list (lambda ()
                         (let ((current-line (thing-at-point 'line 1)))
                           (if (string-search "tags:" current-line)
                               (parse-yaml-list (string-clean-whitespace (nth 1 (split-string current-line ":"))))
                             (forward-line)
                             (funcall get-tag-list))))))
    (with-current-buffer note-buffer
      (save-excursion
        (goto-char (point-min))
        (funcall get-tag-list)
        ))))

(defun insert-yaml-into-buffer (buffer)
  (let ((name (file-name-sans-extension (buffer-name buffer))) (current-date (format-time-string "%Y-%m-%d")))
    (with-current-buffer buffer
      (insert (format "---\nname: %s\ndate: %s\ntags: []\n---" name current-date))
      (save-buffer))))

(defun find-note (&optional predicate)
  (let* ((note-files (directory-files notes-directory-path))
         (full-note-name (ido-completing-read+ "Note name: " note-files predicate))
         (full-note-path (concat notes-directory-path full-note-name)))
    (if (file-exists-p full-note-path)
        (find-file (concat full-note-path))
      (create-note (file-name-sans-extension note-name)))))

;; INTERACTIVES

(defun create-note (note-name)
  "Creates a new note in note-directory and opens its buffer"
  (interactive "sNote name: ")
  
  (let* ((full-note-name (concat note-name ".org"))
         (full-note-path (concat notes-directory-path full-note-name))
         (note-buffer (find-file full-note-path)))
    (insert-yaml-into-buffer note-buffer)))

(defun find-note-by-title ()
  (interactive)
  (find-note))

(defun find-note-by-tags-and-title ()
  "Find a note by tag(s). Seperate each tag by a space"
  (interactive)
  (find-note 'note-has-tags-p))
