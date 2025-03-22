;;; packages.el --- my-private-conf layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  <olepor@olepor>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-private-conf-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-private-conf/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-private-conf/pre-init-PACKAGE' and/or
;;   `my-private-conf/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-private-conf-packages
  '(
    ;; Personal packages
    ;; (github-review :location local)
    ;; (md-to-jira-mode :location local)
    (git-conventional-commit-mode :location local)

    ;; (go-tagalign-fmt :location local)

    ;; Treesitter-mode
    devicetree-ts-mode

    react-snippets

    ;; coverage ;; add in-buffer test coverage for golang
    ;; Install logview mode
    ;; (logview :location
    ;;          (recipe
    ;;           :fetcher github
    ;;           :repo "doublep/logview"))
    ;; (bitbake :location
    ;;          (recipe
    ;;               :fetcher github
    ;;               :repo "canatella/bitbake-el"))
    ;; ox-jira
    ;;
    ;; K8s mode
    (k8s-mode
     :toggle
     :location
     (recipe
      :fetcher github
      :repo "TxGVNN/emacs-k8s-mode"))

    ;; OPA policy language mode - https://www.openpolicyagent.org/docs/latest/policy-language/
    (rego-mode
     :toggle
     :location
     (recipe
      :fetcher github
      :repo "psibi/rego-mode"))

    ;; We do not want this mode - Use sqlformat instead
    (sqlfmt-mode
     :excluded t)

    (sqlformat
     :init
     ;; (setq sqlformat-command 'pgformatter)
     ;; (setq sqlformat-command 'sqlfmt)
     :location
     (recipe
      :fetcher github
      :repo github.com/purcell/sqlformat))

    ;; TODO - CPP-insights mode

    (flycheck-google-cpplint
     :toggle
     :location
     (recipe
      :fetcher github
      :repo "flycheck/flycheck-google-cpplint"))
    )

  "The list of Lisp packages required by the my-private-conf layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun my-private-conf/init-git-conventional-commit-mode ()
  (use-package git-conventional-commit-mode))


;;
;;; INPROGRESS - Add flyspell to the commit buffer
;;

(defun my-private-conf/init-devicetree-ts-mode ())

(defun my-private-conf/init-flyspell ()
  (spell-checking/add-flyspell-hook 'git-conventional-commit-mode-hook))

(defun my-private-conf/init-k8s-mode ())

(defun my-private-conf/init-react-snippets())

(defun my-private-conf/init-flycheck-google-cpplint ())

(defun my-private-conf/init-rego-mode ())

(defun my-private-conf/init-sqlformat ()
  (use-package sqlformat
    :commands sqlformat-buffer
    :init
    (setq sqlformat-command 'sqlfmt)
    (setq sqlfmt-options '("-")) ;; Read from stdin by default
    (spacemacs/declare-prefix-for-mode 'sql-mode "m=" "formatting")
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "=r" 'sqlformat-region
      "==" 'sqlformat-buffer)))

;;; TODO - Tagalign does not accept stdin input atm, only takes package
;; (defun my-private-conf/init-go-tagalign-fmt ()
;;   (use-package go-tagalign-fmt
;;     :init
;;     )
;;   )


;;; TODO - Add pgformatter to post-init-sqlfmt setup
;; -- (setq sqlformat-command 'pgformatter)
;; -- (setq sqlformat-args '("-s2" "-g"))

;;; packages.el ends here
