;;; orgpress.el --- .
;;;
;;; Commentary:
;;; Code:

(require 'org-element)

(defvar orgpress-root "~/Dropbox/Writings")

(defvar orgpress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'orgpress-status)
    (define-key map (kbd "<return>") 'orgpress--open-at-point)
    map))

(defun orgpress--open-at-point ()
  "커서가 위치한 곳의 포스트를 오픈한다."
  (interactive)
  (let ((filename (plist-get (text-properties-at (point)) 'filename)))
    (when (not (null filename))
      (pop-to-buffer (find-file filename)))))

(define-derived-mode orgpress-mode nil "Orgpress")

(defun orgpress--org-parse (filename)
  "주어진 파일 FILENAME 에서 제목, 날짜, 출판여부등의 정보를 확인하여 반환한다."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((tree (org-element-parse-buffer)))
      (cons (cons 'filename filename)
            (org-element-map tree '(keyword)
              (lambda (element)
                (let ((key (org-element-property :key element))
                      (value (org-element-property :value element)))
                  (cond ((string= key "PUBLISH")
                         (cons 'publish value))
                        ((string= key "TITLE")
                         (cons 'title value))
                        ((string= key "DATE")
                         (cons 'date value))))))))))

(defun orgpress--sort-posts (posts)
  "주어진 포스트 POSTS 들을 날짜별로 정렬한다."
  (sort posts (lambda (x y) (string> (alist-get 'date x) (alist-get 'date y)))))

(defun orgpress--filter-posts (posts)
  "주어진 포스트 POSTS 들에서 제목이 없는 것은 제외한다."
  (cl-remove-if-not (lambda (post) (alist-get 'title post)) posts))

(defun orgpress--get-posts ()
  "주어진 경로 orgpress-root 내에 포스트 목록을 찾아서 반환한다."
  (let ((posts (directory-files-recursively orgpress-root "\\.org$")))
    (orgpress--sort-posts
     (orgpress--filter-posts
      (mapcar #'orgpress--org-parse posts)))))

(defun orgpress--draw-status (buffer)
  "주어진 버퍼 BUFFER 에 상태를 출력한다."
  (let ((posts (orgpress--get-posts)))
    (with-current-buffer buffer
      (orgpress-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize "Orgpress Status:" 'face 'bold)
         "\n\n"
         (orgpress--draw-posts posts)
         )))))

(defun orgpress--draw-post (post)
  "하나의 포스트 POST 를 출력한다."
  (propertize (format " %s [%s] %s\n"
                      (if (string= (alist-get 'publish post) "true")
                          "[X]"
                        "[ ]")
                      (substring (alist-get 'date post) 0 10)
                      (alist-get 'title post))
              'face 'font-lock-constant-face
              'filename (alist-get 'filename post)))

;; (orgpress--draw-post (car (orgpress--get-posts)))

(defun orgpress--draw-posts (posts)
  "여러개의 포스트 POSTS 들을 출력한다."
  (cl-reduce #'concat (mapcar #'orgpress--draw-post posts)))

;;;###autoload
(defun orgpress-status ()
  "."
  (interactive)
  (let ((buffer (get-buffer-create "*orgpress-status*")))
    (progn
      (orgpress--draw-status buffer)
      (pop-to-buffer buffer))))

(provide 'orgpress)
;;; orgpress.el ends here.
