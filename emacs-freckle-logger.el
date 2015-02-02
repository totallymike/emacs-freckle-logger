;;; -*- lexical-binding: t -*-
;;; emacs-freckle-logger.el --- freckle-logger

;; Copyright (C) 2015 Michael Westbom

;; Author: Michael Westbom <michael@agilionapps.com>
;; Keywords: communication

;;; Commentary:

;; Log freckle hours from emacs.  That's, so far, the long and short of it.

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

(defun freckle-logger--success-message (&key data &allow-other-keys)
  (message "Success"))

(defconst freckle-logger--freckle-api-url
  "https://api.letsfreckle.com/v2/")

(defconst freckle-logger--template--pause-timer
  (concat freckle-logger--freckle-api-url "projects/%s/timer/pause"))

(defconst freckle-logger--template--start-timer
  (concat freckle-logger--freckle-api-url "projects/%s/timer/start"))

(defun freckle-logger-start-timer (project-id)
  "Sends a request to Freckle to start the timer for a given project"
  (interactive "PProject Id: ")
  (let ((project-id (freckle-logger--fetch-project-id project-id)))
    (let* ((url (format freckle-logger--template--start-timer project-id)))
      (freckle-logger--make-request url
                                    #'freckle-logger--success-message
                                    #'freckle-logger--error-message
                                    "PUT"))))

(defun freckle-logger-pause-timer (project-id)
  "Sends a request to Freckle to start the timer for a given project"
  (interactive "PProject Id: ")
  (let ((project-id (freckle-logger--fetch-project-id project-id)))
    (let* ((url (format freckle-logger--template--pause-timer project-id)))
      (freckle-logger--make-request url
                                    #'freckle-logger--success-message
                                    #'freckle-logger--error-message
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
