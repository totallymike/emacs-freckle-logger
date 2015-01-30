(load-file "./emacs-freckle-logger.el")

(require 'ert)

(ert-deftest freckle-projects-caching ()
  "Tests that the cache-freckle-project function works"
  (let ((project '((id . 1) (name . hello))))
    (freckle-logger--cache-freckle-project project)
    (should (equal (gethash 'hello freckle-projects-cache) 1))
    (remhash 'hello freckle-projects-cache)))

(message "%S" freckle-projects-cache)
