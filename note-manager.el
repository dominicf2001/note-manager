;; GLOBALS

(defvar notes-directory "~/Documents/notes/" "The directory note-manager will look when performing note actions")

(defvar notes-mode-map (make-sparse-keymap)
  "Keymap for 'notes-mode'.
\\{notes-mode-map}")

;; SETUP MINOR MODE

(keymap-set notes-mode-map "C-c n" 'create-note)

(define-minor-mode notes-mode
  "Minor mode for managing notes"
  :lighter " Notes"
  :keymap notes-mode-map)

;; FUNCTIONS

(defun create-note (note-name)
  "Creates a new note in note-directory and opens its buffer"
  (interactive "sEnter the note name: ")

  (let ((full-note-name (concat note-name ".org")))
    (find-file (concat notes-directory full-note-name))))

