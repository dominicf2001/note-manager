;; GLOBALS
(defvar notes-directory-path "~/documents/notes/" "The directory note-manager will look when performing note actions")
(defvar defined-tags '("philosophy" "psychology" "c1" "c2"))

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

(defun note-has-tags-p (full-note-name input-tags)
  (let* ((full-note-path (concat notes-directory-path full-note-name))
         (note-buffer (find-file-noselect full-note-path))
         (get-tag-list (lambda ()
                         (if (eobp)
                             nil
                           (let ((current-line (thing-at-point 'line 1))
                                 (has-tags t))
                             (if (string-search "tags:" current-line)
                                 (progn
                                   (let ((note-tags (parse-yaml-list (string-clean-whitespace (nth 1 (split-string current-line ":"))))))
                                     (dolist (input-tag input-tags has-tags)
                                       (unless (member input-tag note-tags)
                                         (setq has-tags nil)))))
                               (progn
                                 (forward-line)
                                 (funcall get-tag-list))))))))
    (with-current-buffer note-buffer
      (save-excursion
        (goto-char (point-min))
        (funcall get-tag-list)))))

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

(cons 1 '(1 2))

(defun find-note-by-tags-and-title ()
  "Find a note by tag(s). Seperate each tag by a space"
  (interactive)
  (let ((continue t)
        (input-tags '()))
    
    (while continue
      (let ((input))
        (setq input (ido-completing-read+ (concat "Tags " (concat (prin1-to-string input-tags t) ": ")) defined-tags))
        (if (equal input "y")
            (setq continue nil)
          (setq input-tags (cons input input-tags)))))
    (find-note (lambda (full-note-name) (note-has-tags-p full-note-name input-tags)))))
