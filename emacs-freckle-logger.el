;;; -*- lexical-binding: t -*-
;;; emacs-freckle-logger.el --- freckle-logger

;;; Commentary:
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'json)
(require 'request)

(defgroup freckle nil
  "Log your Freckle hours in Emacs"
  :group 'comm
  :prefix "freckle-logger-")

(defcustom freckle-personal-access-token nil
  "Your access token"
  :link '(url-link "http://help.letsfreckle.com/import-export-api/api")
  :group 'freckle)

(defvar freckle-projects-cache ()
  "The cache of projects, in the form of alists

This way we don't have to request them each time, which is slow.")

(defun freckle-logger--cache-freckle-project (project)
  "Add project id and name to cache"
  (let ((id (cdr (assoc 'id project)))
        (name (cdr (assoc 'name project))))
    (setq freckle-projects-cache
          (plist-put freckle-projects-cache name id))))

(defun freckle-logger--get-projects ()
  (message "%S" freckle-personal-access-token)
  (let* ((url "https://api.letsfreckle.com/v2/projects"))
    (request
     url
     :headers `(("X-FreckleToken" . ,freckle-personal-access-token))
     :parser (lambda ()
               (let ((json-array-type 'list))
                 (json-read)))
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (dolist (project data)
                   (cache-freckle-project project))))
     :error (function*
             (lambda (&key data error-thrown &allow-other-keys)
               (message "%S" error-thrown))))))


(provide 'emacs-freckle-logger)

;;; emacs-freckle-logger.el ends here
