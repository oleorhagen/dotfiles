


(when (configuration-layer/package-used-p 'my-private-conf)

  ;; Declare the dependency on the git layer (required by the conventional commit
  ;; local package)
  (configuration-layer/declare-layer 'git)

  )
