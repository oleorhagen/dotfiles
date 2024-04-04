


(when (configuration-layer/package-used-p 'my-private-conf)

  ;; Declare the dependency on the git layer (required by the conventional commit
  ;; local package)
  (configuration-layer/declare-layer 'git)

  )

;; Allow creating missing test-files using projectil
(when (configuration-layer/package-used-p 'spacemacs-project)
  (customize-set-variable 'projectile-create-missing-test-files t)
)

