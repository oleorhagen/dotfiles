;; Treesitter language source list
(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
        (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
        (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
        (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
        (make       "https://github.com/alemuller/tree-sitter-make")
        (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
        (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
        (perl       "https://github.com/tree-sitter-perl/tree-sitter-perl" "master" "src")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (treesitter "https://github.com/joelspadin/tree-sitter-devicetree" "main" "src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (php        "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; Emacs defaults to using the non-suffixed *-ts-mode modes, let's remap the
;; ones we have to use treesitter

;; Do not use along with the tree-sitter layer
;; (setq major-mode-remap-alist
;;       '(
;;         (bash-mode . bash-ts-mode)
;;         (c-mode . c-ts-mode)
;;         (c++-mode . c++-ts-mode)
;;         (css-mode . css-ts-mode)
;;         ;; (elisp-mode . elisp)
;;         ;; (go-mode . go-ts-mode)
;;         (html-mode . html-ts-mode)
;;         ;; (javascript-mode . javascript-ts)
;;         (dockerfile-mode . dockerfile-ts-mode)
;;         (make-mode . makefile-ts-mode)
;;         (markdown-mode . markdown-ts-mode)
;;         ;; (treesitter-mode . treesitter-ts-mode)
;;         (toml-mode . toml-ts-mode)
;;         (js2-mode . js-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         (yaml-mode . yaml-ts-mode)

;;       ))

;; Font lock all the things
;; (defvar my-treesit-font-lock-level-max 4)
;; (customize-set-variable 'treesit-font-lock-level my-treesit-font-lock-level-max)

(defun my-install-treesitter-grammars ()
  "Install the given tree-sitter grammars from treesit-language-source-alist"
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; CFEngine3 mode indentation
(customize-set-variable 'cfengine-indent 0) ;; 0 - column indent is required according to their spec

;; Enable indent guides in YAML (sort of required on big files)
(add-hook 'yaml-mode-hook
          (lambda ()
            (indent-guide-mode)))

;;; Add the ruff linter as next after lsp
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'lsp 'python-ruff)))

;;; Make return follow link in info-mode
(add-hook 'Info-mode-hook
          (lambda ()
            (define-key evil-motion-state-local-map
                        (kbd "<return>") 'Info-follow-nearest-node)
            (define-key evil-motion-state-local-map (kbd "n") 'Info-next)
            (define-key evil-motion-state-local-map (kbd "H") 'Info-prev)
            (define-key evil-motion-state-local-map (kbd "L") 'Info-next)))
