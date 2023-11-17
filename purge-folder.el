;;; purge-folder.el --- `purge-folder' command definition
;; author: Victor Bartush (c) 2023

(defgroup purge-folder nil
  "Purge folder functions settings"
  :group 'files)

(defcustom purge-folder-file-types '("bak" "pyc" "pyo" "dwl" "dwl2")
  "Registered file extensions "
  :group 'purge-folder
  :type '(repeat string))

(defcustom purge-folder-menu-highligth-face 'highlight
  "File types selection menu window height"
  :group 'purge-folder
  :type 'symbol)

(defcustom purge-folder-menu-window-height 6
  "File types selection menu window height"
  :group 'purge-folder
  :type 'integer)

(defun purge-folder-body (starting-path extensions-list)
  "Function deletes files with extensions in `extensions-list'
   recursively begining from folder `starting-path'"
  (let* ((counter 0)
	 (purge (lambda (starting-path extensions-list)
		  (let ((files-list (directory-files starting-path nil directory-files-no-dot-files-regexp)))
		    (dolist (item files-list)
		      (let ((full-path (file-name-concat starting-path item)))
			(if (file-directory-p full-path)
			    (when (not (file-symlink-p full-path))
			      (funcall purge full-path extensions-list))
			  (when (member  (file-name-extension item) extensions-list)
			    (princ (format "Deleting file: %s\n" full-path))
			    (condition-case ex
				(progn
				  (delete-file full-path)
				  (setq counter (1+ counter)))
			      (error (princ (format "%s !Error deleting file: %s" ex full-path))))))))))))
    (princ (format "Purge folder %s...\n" starting-path))
    (funcall purge starting-path extensions-list)
    (princ (format "total %d files deleted...\n" counter))
    (> counter 0)))

(defun purge-folder-perform-menu-actions (menu-buffer)
  "Function performs menu actions. Returns selected file types"
  (let* ((action :idle) ;; could be `:idle' `:update' `:apply' `:quit' `:select' `:prev' `:next'
	 (result)
	 (selection)
	 (menu-action)
	 (index 0)
	 (index-max (1- (length (default-value 'purge-folder-file-types))))
	 (file-types (mapcar 'intern (default-value 'purge-folder-file-types)))
	 (menu-window (get-buffer-window))
	 (menu-action (lambda ()
			"Interactions with menu"
			(let ((key (read-key
				    "Select file types [space-Select q-Quit enter-Apply]: ")))
			  (cond ((or (eq key 27)   ;; esc
				     (eq key 113)) ;; q - key
				 (setq action :quit))
				((eq key 13) ;; enter
				 (setq action :apply))
				((eq key 32) ;; space
				 (setq action :select))
				((or (eq key 'up)
				     (eq key 'left))
				 (setq action :prev)
				 (if (> index 0)
				     (setq index (1- index))))
				((or (eq key 'down)
				     (eq key 'right))
				 (setq action :next)
				 (if (< index index-max)
				     (setq index (1+ index))))
				(t (setq action :idle))))))
	 (menu-highlight-item (lambda (idx &optional idx-prev)
				"Highlight `idx' menu item and set face of `idx-prev' elment to `default'"
				(let ((line)
				      (start)
				      (end))
				  ;; change highlighted item
				  (save-excursion
				    (when idx-prev
				      (goto-char 0)
				      (setq line (1+ idx-prev))
				      (setq start (line-beginning-position line))
				      (setq end (line-end-position line))
				      (put-text-property start end 'face 'default))
				    (goto-char 0)
				    (setq line (1+ idx))
				    (setq start (line-beginning-position line))
				    (setq end (line-end-position line))
				    (put-text-property start end 'face purge-folder-menu-highligth-face))
				  ;; center menu items
				  (save-excursion
				    (with-selected-window menu-window
				      (goto-line line)
				      (recenter))))))
	 (menu-update-selected (lambda (idx-highlight)
				 "Update selected menu items according to `selection' list"
				 (let ((line)
				       (check-pos)
				       (curr-type)
				       (curr-type-selectedp))
				   (dotimes (idx (length file-types))
				     (save-excursion
				       (goto-char 0)
				       (setq line (1+ idx))
				       (setq check-pos (1+ (line-beginning-position line)))
				       (goto-char check-pos)
				       (delete-char 1)
				       (setq curr-type (nth idx file-types))
				       (setq curr-type-selectedp (memq curr-type selection))
				       (if curr-type-selectedp
					   (insert "x")
					 (insert " "))
				       (when (= idx idx-highlight)
					 (put-text-property (1- (point)) (point) 'face purge-folder-menu-highligth-face)))))))
	 (action-update (lambda ()
			  "Perform update menu action"
			  (erase-buffer)
			  (let ((idx 0))
			    (dolist (ext file-types)
			      (princ (concat "[ ] ." (symbol-name ext) "\n"))
			      (when (= idx index)
				(funcall menu-highlight-item idx))
			      (setq idx (1+ idx)))))))
    ;; function body
    (funcall action-update)
    (while (not (eq action :quit))
      (funcall menu-action)
      (cond ((eq action :update)
	     (funcall action-update))
	    ((eq action :apply)
	     (setq result (mapcar 'symbol-name selection))
	     (setq action :quit))
	    ((eq action :select)
	     (let ((curr-type (nth index file-types)))
	       (if (memq curr-type selection)
		   (setq selection (remove curr-type selection))
		 (push curr-type selection)))
	     (funcall menu-update-selected index))
	    ((eq action :prev)
	     (funcall menu-highlight-item index (1+ index)))
	    ((eq action :next)
	     (funcall menu-highlight-item index (1- index)))))
    result))

;; (custom-set-variables
;;  '(purge-folder-file-types '("bak" "dwl" "dwl2" "old")))
;; (custom-variable-p 'purge-folder-file-types)
;; (setq tmp (default-value 'purge-folder-file-types))
;;(customize-save-variable 'purge-folder-file-types (butlast (default-value 'purge-folder-file-types)))

(defun purge-folder ()
  "Iteractiveley call `purge-folder-body' function"
  (interactive)
  (let ((menu-buffer (get-buffer-create "Pick file types to delete:"))
	(starting-folder (read-directory-name "Select starting directory: " default-directory))
	(selection))
    ;; file types menu selection
    (with-current-buffer menu-buffer
      (with-current-buffer-window
	  menu-buffer
	  ;; action
	  (display-buffer-below-selected menu-buffer
					 (list
					  ;;(window-height . fit-window-to-buffer)
					  (cons 'window-height purge-folder-menu-window-height)
					  '(preserve-size . (nil . t))))
	  ;; quit function
	  (lambda (window _value)
	    (with-selected-window window
              (unwind-protect
		  (when (window-live-p window)
		    (quit-restore-window window 'kill)))))
	;; buffer body
	(setq cursor-type nil)
	(when starting-folder
	  (setq selection (purge-folder-perform-menu-actions menu-buffer)))))
    ;; perform purge foder for selected types
    (when selection
      (let ((file-types-string ""))
	(dolist (ext selection file-types-string)
	  (setq file-types-string (concat file-types-string (format ".%s, " ext))))
	(setq file-types-string (format "[%s]" (substring file-types-string 0 -2)))
	(when (yes-or-no-p (format  "Are you sure you want to delete all %s files in %s directory? "
				    file-types-string
				    starting-folder))
	  (purge-folder-body starting-folder selection))))))


(provide 'purge-folder)
