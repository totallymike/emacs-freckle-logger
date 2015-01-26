;;; -*- lexical-binding: t -*-
;;; emacs-freckle-logger.el --- freckle-logger

;;; Commentary:
;;

;;; Code:

(use-package request
  :ensure request)
(require 'request)

(defun get-projects ()
  (let* ((url "https://apitest.letsfreckle.com/api/projects.json")
         (token "lx3gi6pxdjtjn57afp8c2bv1me7g89j"))
    (request
     url
     :headers '(("X-FreckleToken" . "lx3gi6pxdjtjn57afp8c2bv1me7g89j"))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                (message "%S" data)))
     :error (function*
             (lambda (&key data &allow-other-keys)
               (message data)
               (message err)
               (message status))))))

(get-projects)

(request "https://apitest.letsfreckle.com/api/projects.json"
         :type "GET"
         :headers '(("X-FreckleToken" . "lx3gi6pxdjtjn57afp8c2bv1me7g89j"))
         :parser 'json-read
         :success (function*
                   (lambda (&key data &allow-other-keys)
                     (message "Here's a thing: %S" (assoc-default 'json data))))
         :error (function*
                 (lambda (data err status)
                   (message (request-response-data))))

(provide 'emacs-freckle-logger)

;;; emacs-freckle-logger.el ends here
