

(defun prepare-pr (&optional arg)
  ;; Run the check and the extracheck functions prior to pushing a PR
  (interactive "p")
  (require 'projectile)
  (let ((makefile (helm--make-makefile-exists
                   (projectile-project-root)
                   (if (and (stringp helm-make-build-dir)
                            (not (string-match-p "\\`[ \t\n\r]*\\'" helm-make-build-dir)))
                       `(,helm-make-build-dir "" "build")
                     `(,@helm-make-build-dir "" "build")))))
    (if (not makefile)
        (error "No build file found for project %s" (projectile-project-root))
      (setq helm-make-command (helm--make-construct-command '"check" makefile))
      (helm--make makefile))))
