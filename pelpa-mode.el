;;; pelpa-mode.el -- popkit elpa monitor mode-line

;; Copyright (C) 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 1.0
;; Keywords: popkit, elpa, monitor
;; Homepage: https://github.com/popkit/pelpa-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; for popkit elpa monitor
;; 监控 elpa.popkit.org的后台运行情况

(require 'cl-lib)
(require 'package)
(require 'lisp-mnt)
(require 'json)
;; 引入widget库
(require 'widget)
(require 'cus-edit)
(eval-when-compile
  (require 'wid-edit))

;; 注意：这个pelpa-mode-map的定义一定要放在 define-derived-mode 之前
(defvar pelpa-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "r" 'pm/monitor)  ;; refresh status
    map)
  "major key map for pelpa-mode")

(define-derived-mode pelpa-mode nil "pelpa-mode"
  "Major for popkit elpa site (https://elpa.popkit.org/#/) running monitor."
  (widen)
  (setq buffer-read-only t)
  :group 'pelpa-mode)

(defface pm/chart-bar-face
  '((t . (:background "orange" :foreground "black")))
  "face for chart bar" :group 'pelpa-mode)

(defface pm/default-button-green
  '((t . (:background "green" :foreground "black")))
  "default button" :group 'pelpa-mode)

(defcustom pm/build-status-url "http://pelpa.popkit.org/elpa/build/ajaxBuildStatus.json"
  "build status url for ajax"
  :group 'pelpa-mode
  :type 'string)

(defcustom pm/pelpa-buffer-name "*pelpa*"
  "the display buffer name"
  :group 'pelpa-mode
  :type 'string)

(defun pm/decode-region (arg)
  "decode current buffer"
  (interactive "P")
  (decode-coding-region (point-min) (point-max) 'utf-8))

;; 读取http结果中的json值
(defun pm/read-http-data-as-json (http-data)
  (with-temp-buffer
    (insert http-data)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read)))

;; 将时间转成北京时间
(defun pm/convert-to-ut+8 (origin)
  (if (stringp origin)
      (let* ((time (date-to-time origin)))
        (format-time-string "%Y-%m-%d %H:%M:%S %a. Week %W" time))
    origin))

;; 解析出http返回的header中的timestamp信息
;; Date: Sat, 18 Jun 2016 14:43:59 GMT
(defun pm/read-http-timestamp-string (http-data)
  (with-temp-buffer
    (insert http-data)
    (goto-char (point-min))
    (let* ((start-point (+ 1 (search-forward "Date:")))
           (end-point (search-forward "\n"))
           (timestamp-string (buffer-substring start-point end-point)))
      (pm/convert-to-ut+8 timestamp-string))
    ))

(defun pm/to-string (origin)
  (cond ((numberp origin) (number-to-string origin))
        (t origin)))

(defun pm/pelpa-mode-kill ()
  (interactive)
  (with-current-buffer pm/pelpa-buffer-name
    (let ((buffer (current-buffer)))
      (unless (one-window-p)
        (when (get-buffer-window buffer)
          (delete-window)))
      (kill-buffer buffer)))
  (message "%s buffer was killed!" pm/pelpa-buffer-name))

;; 获得构建状态信息
(defun pm/ajax-build-status ()
  "get build status info"
  (let* ((buffer (url-retrieve-synchronously pm/build-status-url))
         (http-content nil)
         (json-data nil)
         (result-data nil))
    (if (not buffer)
        (error "请求%服务失败，请重试！" pm/build-status-url))
    (with-current-buffer buffer
      (unless (= 200 (url-http-parse-response)))
      (setq http-content (decode-coding-string (buffer-string) 'utf-8))
      (setq json-data (pm/read-http-data-as-json http-content))
      (with-temp-buffer
        (insert (format "%s" (pm/read-http-timestamp-string http-content)))
        ;; 插入json的key value值
        (dolist (item (list 'currentRun 'percent 'percentDesc))
          (insert
           (format "\n%s:%s"
                   (symbol-name item)
                   (pm/to-string (assoc-default item json-data)))))
        (setq result-data (buffer-string))))
    result-data))

(defun pm/go-github-pelpa-repo (button)
  "go elpa site"
  (browse-url "https://github.com/popkit/pelpa"))

;; 显示所有的监控信息
(defun pm/monitor (arg)
  "ajax pelpa building status"
  (interactive "P")
  (let* ((pelpa-buffer (get-buffer-create pm/pelpa-buffer-name))
         (ajax-status-content nil))
    (with-current-buffer pelpa-buffer
      (pelpa-mode)
      (setq buffer-read-only nil)
      (erase-buffer)     ;; 先清空原有的内容
      (setq ajax-status-content (pm/ajax-build-status))
      (put-text-property 0 9 'font-lock-faces 'pm/chart-bar-face ajax-status-content)
      (insert ajax-status-content)
      (insert "\n")
      (insert-button "pelpa@github"
                     ;;'face 'pm/default-button-green
                     'action 'pm/go-github-pelpa-repo)
      (insert "\n")
      (widget-create 'push-button
                     :format "查看历史访问的统计数据 %[ %t %]\n"
                     :button-face 'custom-button
                     :help-echo "访问统计链接"
                     :action (lambda (wid &rest ignore)
                               (browse-url "http://pelpa.popkit.org/elpa/data/index.html"))
                     :tag "点击")
      (setq buffer-read-only t)
      ))
  (unless (get-buffer-window pm/pelpa-buffer-name)
    (if (one-window-p)    ;; 只有一个window里，在当前window里显示buffer
        (switch-to-buffer pm/pelpa-buffer-name)
      (switch-to-buffer-other-window pm/pelpa-buffer-name))))

(provide 'pelpa-mode)
