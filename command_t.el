;;;; This is a set of experiments on possible Emacs variants of TextMates' Command-T

;; (string-matches "h.*t" '("hot" "post" "heat"))
(defun string-matches (re list)
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
    (string-matches (phoenix-search-re string) (tags-table-files))))

(defun phoenix-last-path-component-re (string)
  (concat (phoenix-path-component-re string) "$"))

;; build a path-component match re
;; (phoenix-path-component-re "12")
(defun phoenix-path-component-re (string)
  (apply 'concat (phoenix-splice (split-string string "" t) "[^/]*")))

;; build an re like "1[^/*]2[.*/.*]/3[^/]*4" from "12/34"
;; (phoenix-search-re "12/34")
;; (string-match (phoenix-search-re "12/34") "01/123/789/345")
(defun phoenix-search-re (string)
  (let ((names (split-string string "/" t)))
    (apply 'concat
           (phoenix-splice (mapcar 'phoenix-path-component-re names)
                           ".*/.*"))))

;; (phoenix-relevance-re "app/foo")
(defun phoenix-relevance-re (string)
  (apply 'concat (phoenix-splice (split-string string "/" t) "[^/]*/[^/]*")))

;; (phoenix-splice '(1 2 3) :foo)
(defun phoenix-splice (list splice)
  (let ((result nil))
    (dolist (item list)
      (setf result (cons item result))
      (setf result (cons splice result)))
    (reverse (cdr result))))

;; infer the top directory of a project from the location of the 
;; current tags file. Is this a reasonable assumption?
(defun phoenix-find-file-top-dir ()
  (file-name-directory tags-file-name))

(defun find-file-in-tag-files-dired (str)
  "Find files matching string, a la TextMate's Command-T"
  (interactive "s")
  (let ((matches (string-matches-in-tag-files str)))
    (if (not matches) (error "No matches"))    
    (switch-to-buffer (get-buffer-create "*Choose A File*"))
    (setq buffer-read-only nil)
    (widen)
    (erase-buffer)
    (kill-all-local-variables)
    (setq default-directory (phoenix-find-file-top-dir))
    (let* ((files (apply 'call-process (append '("ls" nil t nil "-al") matches)))
           (ls-list (split-string (buffer-substring (point-min) (point-max)) "\n")))
      (erase-buffer)
     (dolist (item (phoenix-relevance-sort str ls-list))
       (insert-string (concat item "\n")))
      (dired-mode (phoenix-find-file-top-dir))
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker))))
      (beginning-of-buffer))))

;; sort list by relevance to string
;; early match beats late match beats no match
(defun phoenix-relevance-sort (str list)
  (let ((rel (phoenix-relevance-re str)))
    (sort list
          (lambda (x y)
            (let ((rel-x (string-match rel x))
                  (rel-y (string-match rel y)))
              (cond
               ((not rel-y) t)
               ((not rel-x) nil)
               (t (< rel-x rel-y))))))))
        

;; (global-set-key "\M-t" 'find-tag-file)

;; this does not work as well as just searching by tags
(defun find-tag-file ()
  (interactive)
  (visit-tags-table-buffer)
  (find-file (completing-read
   "Find a file: "
   'phoenix-tabs-completion
   nil t nil)))

(defun phoenix-tabs-completion (string pred flag)
  (let ((matches (string-matches-in-tag-files string)))
    (cond
     (flag matches)
     ; does not yet handle the lambda case correctly
     (t
      (visit-tags-table-buffer)
      (member string (tags-table-files))))))

     
          


