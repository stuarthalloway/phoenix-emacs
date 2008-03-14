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
    (string-matches (phoenix-search-expression string) (tags-ituntable-files))))

;; build a path-component match expression
;; (phoenix-path-component-expression "12")
(defun phoenix-path-component-expression (string)
  (apply 'concat (phoenix-splice (split-string string "" t) "[^/]*")))

;; build an expression like "1[^/*]2[.*/.*]/3[^/]*4" from "12/34"
;; (phoenix-search-expression "12/34")
;; (string-match (phoenix-search-expression "12/34") "01/123/789/345")
(defun phoenix-search-expression (string)
  (let ((names (split-string string "/" t)))
    (apply 'concat
           (phoenix-splice (mapcar 'phoenix-path-component-expression names)
                           ".*/.*"))))

;; (phoenix-relevance-expression "app/foo")
(defun phoenix-relevance-expression (string)
  (apply 'concat (phoenix-splice (split-string string "/" t) "[^/]*/[^/]*")))

;; (phoenix-splice '(1 2 3) :foo)
(defun phoenix-splice (list splice)
  (let ((result nil))
    (dolist (item list)
      (setf result (cons item result))
      (setf result (cons splice result)))
    (reverse (cdr result))))

(defun find-file-in-tag-files-dired (str)
  "Find files matching string, a la TextMate's Command-T"
  (interactive "s")
  (let ((matches (string-matches-in-tag-files str)))
    (message "Matches are %s" matches)
    (if (not matches) (error "No matches"))    
    (switch-to-buffer (get-buffer-create "*Choose A File*"))
    (erase-buffer)
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (setq default-directory "~/relevance/surety/trunk/")
    (let* ((files (apply 'call-process (append '("ls" nil t nil "-al") matches)))
           (ls-list (split-string (buffer-substring (point-min) (point-max)) "\n")))
      (erase-buffer)
     (dolist (item (phoenix-relevance-sort str ls-list))
       (insert-string (concat item "\n")))
      (dired-mode "~/relevance/surety/trunk/")
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker))))
      (beginning-of-buffer))))

;; sort list by relevance to string
;; early match beats late match beats no match
(defun phoenix-relevance-sort (str list)
  (let ((rel (phoenix-relevance-expression str)))
    (sort list
          (lambda (x y)
            (let ((rel-x (string-match rel x))
                  (rel-y (string-match rel y)))
              (cond
               ((not rel-y) t)
               ((not rel-x) nil)
               (t (< rel-x rel-y))))))))
        

;; (global-set-key "\M-t" 'find-file-in-tag-files-dired) 





