(load-file "./emacs-freckle-logger.el")

(require 'ert)

(ert-deftest freckle-projects-caching ()
  "Tests that the cache-freckle-project function works"
  (let ((current-cache freckle-projects-cache)
        (project '((id . 1) (name . hello))))
    (setq freckle-projects-cache ())
    (freckle-logger--cache-freckle-project project)
    (should (equal freckle-projects-cache '(hello 1)))
    (setq freckle-projects-cache current-cache)))

(message "%S" freckle-projects-cache)
