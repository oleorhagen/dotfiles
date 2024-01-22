
;; Treesitter language source list
(setq treesit-language-source-alist
   '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
     (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
     (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
     (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
     (cmake      "https://github.com/uyha/tree-sitter-cmake")
     (css        "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "file:///opt/src/github/tree-sitter-dockerfile" "main" "src")
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
     (perl       "file:///opt/src/github/tree-sitter-perl" "master" "src")
     (python     "https://github.com/tree-sitter/tree-sitter-python")
     (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
     (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
     (toml       "https://github.com/tree-sitter/tree-sitter-toml")
     (treesitter "https://github.com/joelspadin/tree-sitter-devicetree" "master" "src")
     (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; Emacs defaults to using the non-suffixed *-ts-mode modes, let's remap the
;; ones we have to use treesitter

(setq major-mode-remap-alist
      '(
        (bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)
      ))

;; Font lock all the things
(defvar my-treesit-font-lock-level-max 4)
(customize-set-variable 'treesit-font-lock-level my-treesit-font-lock-level-max)


;; CFEngine3 mode indentation
(customize-set-variable 'cfengine-indent 0) ;; 0 - column indent is required according to their spec
