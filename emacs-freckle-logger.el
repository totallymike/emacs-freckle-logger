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

(defvar freckle-projects-cache
  (make-hash-table :test 'equal )
  "The cache of projects, in the form of alists

This way we don't have to request them each time, which is slow.")

(defun freckle-logger--cache-freckle-project (project)
  "Add project id and name to cache"
  (let ((id (cdr (assoc 'id project)))
        (name (cdr (assoc 'name project))))
    (message "%S" id name)
    (puthash name id freckle-projects-cache)))

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
                   (freckle-logger--cache-freckle-project project))))
     :error (function*
             (lambda (&key data error-thrown &allow-other-keys)
               (message "OH NO ERROR")
               (message "%S" error-thrown))))))

(defun freckle-logger--fetch-project-id (project-id)
  "Take project id from cache, prompting user if nil"
  (or project-id
      (let (project-name)
        (setq project-name
              (completing-read "Project: " freckle-projects-cache nil t))
        (setq project-id (gethash project-name freckle-projects-cache)))))
(message "%s" freckle-personal-access-token)

(defun freckle-logger--error-message (&key error-thrown &allow-other-keys)
  (message "OH NO ERROR" error-thrown))

(defun freckle-logger-start-timer (project-id)
  "Sends a request to Freckle to start the timer for a given project"
  (interactive "PProject Id: ")
  (let ((project-id (freckle-logger--fetch-project-id project-id)))
    (let* ((url-template "https://api.letsfreckle.com/v2/projects/%s/timer/start")
           (url (format url-template project-id))
           (success-fn (function*
                        (lambda (&key data &allow-other-keys)
                          (message "%s started" (assoc-default 'name (assoc 'project data)))))))
      (freckle-logger--make-request url success-fn #'freckle-logger--error-message "PUT"))))

(defun freckle-logger-pause-timer (project-id)
  "Sends a request to Freckle to start the timer for a given project"
  (interactive "PProject Id: ")
  (let ((project-id (freckle-logger--fetch-project-id project-id)))
    (let* ((url-template "https://api.letsfreckle.com/v2/projects/%s/timer/pause")
           (url (format url-template project-id))
           (success-fn (function*
                        (lambda (&key data &allow-other-keys)
                          (message "%s paused" (assoc-default 'name (assoc 'project data)))))))
      (freckle-logger--make-request url
                                    success-fn #'freckle-logger--error-message
                                    "PUT"))))

(defun freckle-logger--make-request (url success-fn error-fn &optional verb)
  (let ((verb (or verb "GET")))
    (request url
             :type verb
             :headers `(("X-FreckleToken" . ,freckle-personal-access-token))
             :parser (lambda ()
                       (let ((json-array-type 'list))
                         (json-read)))
             :success success-fn
             :error error-fn)))

(provide 'emacs-freckle-logger)

;;; emacs-freckle-logger.el ends here
