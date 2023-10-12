;;; nodes-mode.el --- A minor mode for note management

;; Author: Dominic Ferrando <dominicf2001@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/dominicf2001/notes-mode

;; LOAD/CREATE TAGS STORAGE FILE

(with-temp-buffer
  (unless (file-exists-p "tags")
    (write-region "()" nil "tags"))
  
  (insert-file-contents "tags")
  (setq defined-tags (read (current-buffer))))

;; GLOBALS
(defvar notes-directory-path "~/documents/notes/" "What note-manager will look to as the notes directory when performing note actions")

;; KEYBINDING

(defvar notes-mode-map (make-sparse-keymap)
  "Keymap for 'notes-mode'.
\\{notes-mode-map}")

(keymap-set notes-mode-map "C-c n c" 'notes-create)
(keymap-set notes-mode-map "C-c n f" 'notes-find-by-title)

;; SETUP MINOR MODE

(define-minor-mode notes-mode
  "Minor mode for managing notes"
  :lighter " Notes"
  :keymap notes-mode-map)

;; HELPERS

(defun notes-parse-yaml-list (list-string)
  (let* ((unparsed-elements (split-string list-string ","))
         (build-list (lambda (list pos)
                       (if (nth pos unparsed-elements)
                           (let ((target-string (nth pos unparsed-elements)))
                             (when (string-match "\\\"\\([^\"]+\\)\\\"" target-string)
                               (setq list (cons (match-string-no-properties 1 target-string) list)))
                             (funcall build-list list (+ pos 1)))
                         (nreverse list)))))
    (funcall build-list '() 0)))

(defun notes-has-tags-p (full-note-name input-tags)
  (let* ((full-note-path (concat notes-directory-path full-note-name))
         (note-buffer (find-file-noselect full-note-path))
         (get-tag-list (lambda ()
                         (if (eobp)
                             nil
                           (let ((current-line (thing-at-point 'line t)))
                             (if (and current-line (string-search "tags:" current-line))
                                 (notes-parse-yaml-list (string-clean-whitespace (nth 1 (split-string current-line ":"))))
                               (progn
                                 (forward-line)
                                 (funcall get-tag-list))))))))
    (save-excursion
      (with-current-buffer note-buffer
        (goto-char (point-min))
        (let ((tag-list (funcall get-tag-list))
              (has-tags t))
          (dolist (input-tag input-tags has-tags)
            (unless (member input-tag tag-list)
              (setq has-tags nil))))))))

(defun notes-insert-yaml-into-buffer (buffer)
  (let ((name (file-name-sans-extension (buffer-name buffer))) (current-date (format-time-string "%Y-%m-%d")))
    (with-current-buffer buffer
      (insert (format "---\nname: %s\ndate: %s\ntags: []\n---" name current-date))
      (save-buffer))))

(defun notes-filter-list (list predicate)
  (let ((filtered-list '()))
    (dolist (list-item list filtered-list)
      (when (funcall predicate list-item)
        (setq filtered-list (cons list-item filtered-list))))))

(defun notes-find (&optional predicate)
  (let* ((full-note-names (directory-files notes-directory-path))
         (filtered-full-note-names (if predicate
                                       (notes-filter-list full-note-names predicate)
                                     full-note-names)))
    (if filtered-full-note-names
        (let* ((full-note-name (ido-completing-read+ "Note name: " filtered-full-note-names))
               (full-note-path (concat notes-directory-path full-note-name)))
          
          (unless (file-directory-p full-note-path)
            (if (file-exists-p full-note-path)
                (find-file (concat full-note-path))
              (notes-create (file-name-sans-extension full-note-name)))))
      (message "No results"))))

;; INTERACTIVES

(defun notes-create (note-name)
  "Creates a new note in note-directory and opens its buffer"
  (interactive "sNote name: ")
  
  (let* ((full-note-name (concat note-name ".org"))
         (full-note-path (concat notes-directory-path full-note-name))
         (note-buffer (find-file full-note-path)))
    (notes-insert-yaml-into-buffer note-buffer)))

(defun notes-find-by-title ()
  (interactive)
  (notes-find))

(defun notes-add-tag ()
  "Adds a tag to current file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((current-line (thing-at-point 'line t))
          (found-tags-line nil)
          (input-tag (ido-completing-read+ "Tag: " defined-tags)))
      
      (while (and (not (eobp)) (not found-tags-line))
        (if (and current-line (string-search "tags:" current-line))
            (setq found-tags-line t)
          (progn
            (forward-line)
            (setq current-line (thing-at-point 'line t)))))
      
      (when found-tags-line
        (message "Test")
        (search-forward "]")
        (backward-char)
        (if (string-search "[]" current-line)
            (insert "\"" input-tag "\"")
          (insert ", \"" input-tag "\""))))))

(defun notes-find-by-tags-and-title ()
  "Find a note by tag(s)"
  (interactive)
  (let ((continue t)
        (defined-tags (cons "[done]" defined-tags))
        (input-tags '())
        (input)
        (unselected-tags '()))
    
    (while continue
      (setq unselected-tags '())
      (dolist (defined-tag defined-tags unselected-tags)
        (unless (member defined-tag input-tags)
          (setq unselected-tags (cons defined-tag unselected-tags))))
      
      (setq input (ido-completing-read+ (concat "Tags " (concat (prin1-to-string input-tags t) ": ")) unselected-tags))
      
      (if (equal input "[done]")
          (setq continue nil)
        (setq input-tags (cons input input-tags))))
    
    (notes-find (lambda (full-note-name) (notes-has-tags-p full-note-name input-tags)))))

(provide 'notes-mode)
