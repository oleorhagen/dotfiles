
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
        (go-mod      "https://github.com/camdencheek/tree-sitter-go-mod" "main" "src")
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
(defvar my-treesit-font-lock-level-max 4)
(customize-set-variable 'treesit-font-lock-level my-treesit-font-lock-level-max)

(defun my-install-treesitter-grammars ()
  "Install the given tree-sitter grammars from treesit-language-source-alist"
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; CFEngine3 mode indentation
(customize-set-variable 'cfengine-indent 0) ;; 0 - column indent is required according to their spec


;; (defun yas-try-expanding-auto-snippets ()
;;   "Expand snippets with the `auto' condition.
;; This is intended to be added to `post-command-hook'."
;;   (when (bound-and-true-p yas-minor-mode)
;;     (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;       (yas-expand))))
;;; Inspired by the above
(defun yas-try-expanding-auto-snippets ()
  "Expand snippets automatically if starting a command with (,) comma.
This is intended to be added to `post-command-hook'."
  (when (bound-and-true-p yas-minor-mode)
    (yas-expand)))



;; save-restriction
;; narrow-to-region
;; (looking-back "\\b,\\w+" (point))

;; (let (
;;       (mw (current-word))
;;       )
;;   (message current-word)
;;   (when (s-starts-with-p "," mw)
;;     (yas-expand)))

(defun my-test-expand ()
  (interactive)
  (message (current-word))
  (let ( (mbyword (current-word)) )
    (message mbyword)
    (when (s-starts-with-p "," mbyword)
      (message "yayy"))
    )
  )

(defun my-test-looking-back ()
  "docstring"
  (interactive "P")
  (looking-back )
  )


;; Add the hook to post-insert hook
;; (add-hook 'post-command-hook #'yas-try-expanding-auto-snippets )

(add-hook 'yaml-mode-hook
          (lambda ()
            (indent-guide-mode)))
