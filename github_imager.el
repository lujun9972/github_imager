(require 'vc-git)
(defun org2issue--convert-src (body &optional filename)
  (let ((filename (or filename (buffer-file-name))))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
;;; TODO: not only links need to convert, but also inline
;;; images, may add others later
              ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
              "<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^\"]+\\)\"[^>]*>" nil t)
        (let* ((root-path (vc-git-root filename))
               (asset-path (match-string 2))
               (asset-path-begin (match-beginning 2))
               (asset-path-end (match-end 2))
               (asset-abs-path (expand-file-name asset-path (file-name-directory filename)))
               (asset-relative-path (file-relative-name asset-abs-path root-path)))
          (unless (url-type (url-generic-parse-url asset-path)) ;; 判断是否为绝对路径的URI
            (if (not (file-exists-p asset-abs-path))
                (message "ORG2ISSUE: [WARN] File %s in hyper link does not exist, org file: %s." asset-abs-path filename)
              (let ((converted-path
                     (with-temp-buffer
                       (insert (vc-git-dir-extra-headers default-directory))
                       (when (re-search-backward "^Remote[[:blank:]]+:[[:blank:]]+\\([^[:blank:]\r\n]+\\)" nil t)
                         (match-string 1)))
                     (concat "/" (file-relative-name pub-abs-path pub-root-dir)))) ;TODO 转换后的地址
                (setf (buffer-substring asset-path-begin asset-path-end) converted-path))))))
      (buffer-string))))
