(require 'vc-git)
(defun github-imager (&optional filename)
  (let* ((filename (or filename (buffer-file-name)))
         (root-path (vc-git-root filename))
         ;; (asset-abs-path (expand-file-name default-directory filename))
         (relative-path (file-relative-name filename root-path))
         (remote-url
          (with-temp-buffer
            (insert (vc-git-dir-extra-headers root-path))
            (when (re-search-backward "^Remote[[:blank:]]+:[[:blank:]]+\\([^[:blank:]\r\n]+\\)" nil t)
              (match-string 1))))
         (converted-path (when (string-match "^git@github.com:\\([^/]+\\)/\\(.+\\).git" remote-url)
                           (let ((site "raw.githubusercontent.com")
                                 (user (match-string-no-properties 1 remote-url))
                                 (repo (match-string-no-properties 2 remote-url)))
                             (format "https://%s/%s/%s/source/%s" site user repo relative-path)))))
    converted-path))

(defun github-imager-convert (body &optional filename)
  (let ((filename (or filename (buffer-file-name))))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
;;; TODO: not only links need to convert, but also inline
;;; images, may add others later
              ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
              "<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^\"]+\\)\"[^>]*>" nil t)
        (let* ((asset-path (match-string 2))
               (asset-path-begin (match-beginning 2))
               (asset-path-end (match-end 2))
               (asset-abs-path (expand-file-name asset-path (file-name-directory filename))))
          (unless (url-type (url-generic-parse-url asset-path)) ;; 判断是否为绝对路径的URI
            (if (not (file-exists-p asset-abs-path))
                (message "ORG2ISSUE: [WARN] File %s in hyper link does not exist, org file: %s." asset-abs-path filename)
              (let ((converted-path (github-imager asset-abs-path)))
                (when converted-path
                  (setf (buffer-substring asset-path-begin asset-path-end) converted-path)))))))
      (buffer-string))))

(provide 'github-imager)
