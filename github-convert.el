(require 'vc-git)
(defun github-convert-link (&optional filename)
  (interactive)
  (let* ((filename (or filename (read-file-name "Select a file")))
         (root-path (vc-git-root filename))
         ;; (asset-abs-path (expand-file-name default-directory filename))
         (relative-path (file-relative-name filename root-path))
         (remote-url
          (with-temp-buffer
            (insert (vc-git-dir-extra-headers root-path))
            (when (re-search-backward "^Remote[[:blank:]]+:[[:blank:]]+\\([^[:blank:]\r\n]+\\)" nil t)
              (match-string 1))))
         (branch
          (with-temp-buffer
            (insert (vc-git-dir-extra-headers root-path))
            (when (re-search-backward "^Branch[[:blank:]]+:[[:blank:]]+\\([^[:blank:]\r\n]+\\)" nil t)
              (match-string 1))))
         (converted-path (when (string-match "^git@github.com:\\([^/]+\\)/\\(.+\\).git" remote-url)
                           (let ((site "raw.githubusercontent.com")
                                 (user (match-string-no-properties 1 remote-url))
                                 (repo (match-string-no-properties 2 remote-url)))
                             (format "https://%s/%s/%s/%s/%s" site user repo branch relative-path)))))
    (when (interactive-p)
      (message "%s" converted-path)
      (kill-new converted-path))
    converted-path))

(defcustom github-convert-rules
  '(("<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^\"]+\\)\"[^>]*>" . 2) ;HTML link
    ("\[\\([^]]+\\)\]\[\\([^]]+\\)\]" . 2))                         ;Markdown link
  "Alist of filename REGEXP vs NUM.
Each element looks like (REGEXP . NUM).
NUM specifies which parenthesized expression in the regexp should be replaced.")

(defun github-convert-body (body &optional filename)
  (let ((filename (or filename (buffer-file-name))))
    (with-temp-buffer
      (insert body)
      (save-excursion
        (dolist (pair github-convert-rules)
          (goto-char (point-min))
          (let ((regex (car pair))
                (num (cdr pair)))
            (while (re-search-forward regex nil t)
              (let* ((asset-path (match-string num))
                     (asset-path-begin (match-beginning num))
                     (asset-path-end (match-end num))
                     (asset-abs-path (expand-file-name asset-path (file-name-directory filename))))
                (unless (url-type (url-generic-parse-url asset-path)) ;; 判断是否为绝对路径的URI
                  (if (not (file-exists-p asset-abs-path))
                      (message "[WARN] File %s in hyper link does not exist, org file: %s." asset-abs-path filename)
                    (let ((converted-path (github-convert-link asset-abs-path)))
                      (when converted-path
                        (setf (buffer-substring asset-path-begin asset-path-end) converted-path))))))))))
      (buffer-string))))

(provide 'github-convert)
