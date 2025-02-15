(when (configuration-layer/package-used-p 'my-private-conf)

  ;; Declare the dependency on the git layer (required by the conventional commit
  ;; local package)
  (configuration-layer/declare-layer 'git)

  ;; We are redefining sqlfmt, which is a dependency of sql
  (configuration-layer/declare-layer 'sql)

  )

;; Allow creating missing test-files using projectile
(when (configuration-layer/package-used-p 'spacemacs-project)
  (customize-set-variable 'projectile-create-missing-test-files t))
