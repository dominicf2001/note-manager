;; GLOBALS

(defvar notes-directory-path "~/Documents/notes/" "The directory note-manager will look when performing note actions")

;; KEYBINDING

(defvar notes-mode-map (make-sparse-keymap)
  "Keymap for 'notes-mode'.
\\{notes-mode-map}")

(keymap-set notes-mode-map "C-c n" 'create-note)

;; SETUP MINOR MODE

(define-minor-mode notes-mode
  "Minor mode for managing notes"
  :lighter " Notes"
  :keymap notes-mode-map)

;; HELPERS

(defun insert-yaml-into-buffer (buffer)
  (let ((name (file-name-sans-extension (buffer-name buffer))) (current-date (format-time-string "%Y-%m-%d")))
    (with-current-buffer buffer
      (insert (format "---\nname: %s\ndate: %s\ntags: []\n---" name current-date))
      (save-buffer))))

(defun find-note (&optional predicate)
  (let* ((note-files (directory-files notes-directory-path predicate))
         (note-name (ido-completing-read+ "Note name: " note-files))
         (full-note-path (concat notes-directory-path note-name)))
    (if (file-exists-p full-note-path)
        (find-file (concat full-note-path))
      (create-note note-name))))

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

(defun find-note-by-title-and-tag ()
  (interactive)
  (find-note )) 
