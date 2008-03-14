;; (string-matches "h.*t" '("hot" "post" "heat"))
(defun string-matches (re list)
  (message "Searching for %s" re)
  (let ((result nil))
    (dolist (item list)
      (if (string-match re item)
          (setq result (cons item result))))
    result))

;; find phoenix matches in tags
(defun string-matches-in-tag-files (string)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (string-matches (phoenix-search-expression string) (tags-table-files))))

; build an expression like ".*1.*3.*3" from "123"
(defun phoenix-search-expression (string)
  (let ((result "[^/]*"))
    (dolist (item (split-string string "" t))
      (setq result (concat result item "[^/]*")))
    result))
    
(defun find-file-in-tag-files-dired (str)
  (switch-to-buffer (get-buffer-create "*Choose A File*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (setq default-directory "~/relevance/surety/trunk/")
  (let ((files (apply 'call-process (append '("ls" nil t nil "-al") (string-matches-in-tag-files str)))))
    (insert files)
    (dired-mode "~/relevance/surety/trunk/")
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons default-directory (point-min-marker))))))

 
(find-file-in-tag-files-dired "a/m/bonds")


