;;; pelpa-mode.el -- popkit elpa monitor mode-line

;; Copyright 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 1.0

;;; Commentary:

;; for popkit elpa monitor
;; 监控 elpa.popkit.org的后台运行情况

(require 'cl-lib)
(require 'package)
(require 'lisp-mnt)
(require 'json)

(define-derived-mode pelpa-mode text-mode "pelpa-mode"
  "popkit elpa mode for building status monitor"
  :group 'pelpa-mode
  )

(defvar pelpa-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-i" 'pm/ajax-build-status)
    map)
  "major key map for pelpa-mode")

(defcustom pm/build-status-url "http://pelpa.popkit.org/elpa/build/ajaxBuildStatus.json"
  "build status url for ajax"
  :group 'pelpa-mode
  :type 'string)

(defun pm/decode-region (arg)
  "decode current buffer"
  (interactive "P")
  (decode-coding-region (point-min) (point-max) 'utf-8))

(defun pm/ajax-build-status (arg)
  "ajax pelpa building status"
  (interactive "P")
  (let* ((pelpa-build-status-url pm/build-status-url)
         (pelpa-buffer-name "*pelpa*")
         (pelpa-buffer (get-buffer-create pelpa-buffer-name))
         (buffer (url-retrieve-synchronously pelpa-build-status-url))
         (headers nil)
         (handle nil))
    (if (not buffer)
        (error "请求%s失败，请重试!" pelpa-build-status-url))
    (with-current-buffer buffer
      (unless (= 200 url-http-response-status)
        (error "Http error %s fetching %s" url-http-response-status pelpa-build-status-url))
      (message "buffer name%s" (buffer-name))
      (setq handle (mm-dissect-buffer t))
      (setq headers (decode-coding-string (buffer-string) 'utf-8))
      (with-current-buffer pelpa-buffer
        (setq-default major-mode 'pelpa-mode)  ;; 设置local mojor-mode为'text-mode
        (set-buffer-major-mode pelpa-buffer)
        (erase-buffer)     ;; 先清空原有的内容
        (insert headers)))
    (switch-to-buffer pelpa-buffer-name)))

(provide 'pelpa-mode)
