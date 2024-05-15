;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.layers/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(markdown
     markdown
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; My private display package from ~/.layers
     ;; display
     my-private-conf

     ;; React baby
     (react)

     log

     ;; kubernetes

     (sql
      :variables
      sql-backend 'lsp
      sql-lsp-sqls-workspace-config-path 'workspace
      sql-capitalize-keywords t
      sql-auto-indent nil
      )

     ;; For changing the buffer format (hopefully)
     (pandoc :variables
             pandoc-data-dir "~/.emacs.d/.cache/pandoc/")

     (cmake :variables
            cmake-backend 'lsp
            cmake-enable-cmake-ide-support nil ;; TODO - Hangs on opening C-files..
            )

     ;; Jupyter notebook setup
     (ipython-notebook :variables ein-backend 'jupyter)

     import-js

     (javascript :variables
                 javascript-backend 'lsp
                 javascript-lsp-linter nil
                 javascript-import-tool 'import-js
                 javascript-fmt-tool 'prettier
                 javascript-fmt-on-save t)
     react
     prettier

     (html :variables
           web-fmt-tool 'prettier
           scss-enable-lsp t
           css-enable-lsp t
           less-enable-lsp t
           html-enable-lsp t)

     ;; TODO - setup DAP for c++
     dap
     (c-c++ :variables
            c-c++-backend 'rtags
            c-c++-enable-clang-support t
            c-c++-enable-clang-format-on-save t
            c-c++-enable-rtags-support t
            c-c++-enable-organize-includes-on-save nil
            c-c++-lsp-enable-semantic-highlight 'rainbow
            c-c++-default-mode-for-headers 'c++-mode ;; Ideally, should be project local
            c-c++-adopt-subprojects t
            :init
            ;; Set up DAP for C++
            (require 'dap-cpptools)
            )

     ;; Enable ligatures <3 and using unicode fonts
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t)

     debug
     (latex :variables
      latex-build-command "LaTeX")
     ;; ess ;; Layer for R statistical computing
     ;; erlang
     helm
     docker
     ;; dash
     ;; (asm :variables
     ;;      asm-comment-char "#")
     ;; julia
     ;; ;; better-defaults
     ;; (shell :variables shell-default-shell 'ansi-term
     ;;        shell-default-term-shell "/bin/zsh")
     (shell-scripts :variables shell-scripts-backend 'lsp)
     emacs-lisp
     (git
      :variables
      git-magit-status-fullscreen t
      )
     ;; (git
     ;;  :variables
     ;;  git-magit-status-fullscreen t
     ;;  git-commit-major-mode 'git-conventional-commit-mode)
     (lsp :variables
          lsp-pyright-multi-root nil
          lsp-ui-remap-xref-keybindings nil
          lsp-ui-doc-enable             nil
          lsp-ui-doc-include-signature  nil
          lsp-ui-sideline-enable        nil
          lsp-ui-sideline-show-symbol   nil
          )
     cfengine

     ;; Emacs speaks statistics
     ess

     (tree-sitter :variables
                  ;; spacemacs-tree-sitter-hl-black-list nil
                  tree-sitter-syntax-highlight-enable t
                  tree-sitter-fold-enable t
                  tree-sitter-fold-indicators-enable t)
     ;; ;; web-beautify
     ;; ;; The go-layer needs the auto-completion and syntax-checking layers
     ;; ;; TODO - add gotests function wrappers to the go-mode https://github.com/cweill/gotests
     (go :variables
            go-format-before-save t
            gofmt-command "goimports"
            gofmt-args '("-local=github.com/mimiro-io")
            go-use-golangci-lint t
            gofmt-show-errors nil ;; errors are already shown by flycheck
            godoc-at-point-function 'godoc-gogetdoc)
     ;; Debugging
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'nil
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-idle-delay 0.0 ;; set to 0.0 for optimal results with lsp
                      auto-completion-minimum-prefix-length 1 ;; set to 1 for optimal results with lsp
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      ;; auto-completion-enable-help-tooltip nil
                      auto-completion-use-company-box nil
                      auto-completion-enable-sort-by-usage t
                      )
     ;; ;; semantic
     ;; systemd
     syntax-checking
     ;; (markdown :variables
     ;;           markdown-live-preview-engine 'vmd
     ;;           markdown-mmm-auto-modes '("c" "c++" "python" "scala" "bash" ("elisp" "emacs-lisp")))
     (yaml
      :variables
      yaml-enable-lsp t
      yaml-indent-offset 2) ;; Standard in the mender yaml files
     (python :variables
             python-backend 'lsp
             python-lsp-server 'pyright
             python-pytest-runner 'pytest
             python-formatter 'black
             python-enable-yapf-format-on-save nil
             pyton-sort-imports-on-save t)
     ;; ;; (python :variables
     ;; ;;         python-backend 'anaconda
     ;; ;;         python-test-runner 'pytest
     ;; ;;         python-formatter 'black
     ;; ;;         python-enable-yapf-format-on-save nil
     ;; ;;         python-sort-imports-on-save nil)
     ;; ;; gtags
     ;; (scheme
     ;;  :variables
     ;;  geiser-active-implementation '(mit)
     ;;  geiser-default-implementation 'mit)
     (org
      :variables
      org-enable-github-support t
      org-enable-hugo-support t
      org-enable-modern-support t
      org-enable-org-journal-support t)
     spacemacs-org
     ;; neotree
     ;; ;; (shell :variables
     ;; ;;        shell-default-height 300
     ;; ;;        shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t)
     ;; ;; version-control
     ;; (rust :variables
     ;;       rust-format-on-save t)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ;; org-jira maybe? TODO
                                      ;; Add in-buffer code coverage visualisation
                                      ;; ;; (coverage :location (recipe :fetcher github :repo "google/coverage"))
                                      ;;; Snippets for react when developing UI's
                                      (react-snippets)
                                      ;; (go-impl :location
                                      ;;          (recipe
                                      ;;           :fetcher github
                                      ;;           :repo "syohex/emacs-go-impl"))
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(misterioso
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom :separator nil :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '(
                               ("FiraCode Nerd Font"
                                :size 20
                                :weight normal
                                :width normal)
                               ;; ("Cascadia Code"
                               ;;  :size 16
                               ;;  :weight normal
                               ;;  :width normal)
                               ;; ("Source Code Pro"
                               ;; :size 16
                               ;; :weight normal
                               ;; :width normal)
                               )


   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                                         :disabled-for-modes
                                         org-mode
                                         dired-mode
                                         pdf-view-mode)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
)


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
)


(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;;
  ;;; Helm-projectil Make
  ;;
  ;; Default to two processes when calling Make from Helm
  (setq-default helm-make-nproc 2)
  ;; Make the process nice
  (setq-default helm-make-niceness 20)

  ;; C++ (Mender) State-machine alignment hack
  (defun align-statemachine-states (start end)
    (interactive "r\n")
    (align-regexp start end ",\\(\\s-*\\)[a-z_.]+" 1 2 t))

  ;; Set a random theme on startup
  (defun random-list-element (arg-list)
	  (nth (random (length arg-list)) arg-list))
  (spacemacs/load-theme (random-list-element  dotspacemacs-themes))

  ;; No smartparens plz
  (spacemacs/toggle-smartparens-globally-off)

  ;; modify the misterioso background-color for line-highlight
  (custom-theme-set-faces
   'misterioso
   '(hl-line ((t (:background "#292b2e")))))

  ;; evaluate sexpressions with ee
  (spacemacs/set-leader-keys
    "ee" 'eval-last-sexp
    "eb" 'eval-buffer)

  ;; Comment box binding on '<leader> o c b'
  (spacemacs/declare-prefix "oc" "comment")
  (spacemacs/set-leader-keys
    "ocb" 'comment-box)

  (spacemacs/declare-prefix "os" "snippets")
  (spacemacs/set-leader-keys
    "os" 'yas-describe-tables)

  ;; ------ Go-Coverage ---------- TODO; Not working atm
  ;; (add-to-list 'load-path "~/.emacs.d/private/local/coverage/")
  ;; (require 'coverage)
  ;; TODO - why is this not set with package macro in (go :variables ?)

  ;; ;; bind keys for the github.com/google/coverage code coverage for go in buffer package
  ;; (spacemacs/set-leader-keys
  ;;   "cg" 'cov-gen ;; runs compilation that generates coverage profile for the
  ;;   ;;           file. Compilation hooks refresh all affected buffers.)
  ;;   "cs" 'cov-show
  ;;   "ch" 'cov-hide
  ;;   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Evil fix, for key consistency
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-key evil-normal-state-map (kbd "H") (kbd "^")) ; H goes to beginning of the line
  (define-key evil-visual-state-map (kbd "H") (kbd "^")) ;; same goes for v-mode
  (define-key evil-normal-state-map (kbd "L") (kbd "$")) ; L Goes the end of the line
  (define-key evil-visual-state-map (kbd "L") (kbd "$")) ;
  (define-key evil-motion-state-map (kbd "L") (kbd "$")) ; Keep motions consistent
  (define-key evil-motion-state-map (kbd "H") (kbd "^"))

  ;; Shell-script mode basic offset ;;

  ;; (setq sh-basic-offset 2)


  ;; ;; TODO - now opens go-guru in laptop display, also add functionality for all compilation buffers
  ;; (defun open-go-guru-laptop-display (buffer alist)
  ;;   (if  (string-equal (cdaar (display-monitor-attributes-list)) "eDP-1")
  ;;       (progn (message "Found the laptop display")
  ;;              (message "%s" (cadar (cdr (cdddar (display-monitor-attributes-list)))))
  ;;              (window--display-buffer buffer (frame-root-window (cadar (cdr (cdddar (display-monitor-attributes-list))))) 'reuse))
  ;;     (message "%s" (cdaar (display-monitor-attributes-list)))))

  ;; display-buffer-alist

  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*go-guru-output\\*"
  ;;    open-go-guru-laptop-display
  ;;    display-buffer-reuse-window))

  ;; TODO - Instead of having a dedicated window, use a popwin
  ;; (add-to-list 'popwin:special-display-config '("*go-guru-output*" :tail t :dedicated t :position bottom :stick t))

  ;; TODO - implement a checker function, so that if in the *go-guru-output* buffer, open
  ;; buffer in last used window
  ;; TODO - also make the font larger by default on the laptop display


  ;; Bind clang-format-buffer to tab on the c++-mode only:
  ;; (add-hook 'c-c++-mode-hook 'clang-format-bindings ;; NOTE Not using as we format on save atm
  ;;           (defun clang-format-bindings ()
  ;;             (define-key c-mode-map [tab] 'clang-format-buffer)))

  ;; C-C++ format buffer binding for c-c++-mode
  ;; (spacemacs/set-leader-keys-for-major-mode 'c-c++-mode
  ;;   "pc" 'clang-format-region)
  ;; ;; (spacemacs/declare-prefix-for-mode 'c-c++-mode "c" "port/paste")
  ;; (spacemacs/declare-prefix-for-mode 'c-c++-mode "cr" "clang-format-region")

  ;; Global only run flycheck when prompted
  (spacemacs/declare-prefix
    "eb" "flycheck-buffer")
  (spacemacs/set-leader-keys
    "eb" '(lambda ()
            (interactive)
            ;; Enable flycheck if it is not enabled
            (flycheck-mode 1)
            (call-interactively #'flycheck-buffer)))

  ;; Don't kill the emacs-server on exit
  (evil-leader/set-key
    "q q" 'spacemacs/frame-killer)

  ;; c-c++-mode hook for google-cpplint flycheck
  (eval-after-load 'flycheck
    '(progn
       (require 'flycheck-google-cpplint))
    )

  (add-hook 'c++-mode-hook
            (lambda ()
              (message "C++ inintialisation hook...")
              (flycheck-select-checker 'c/c++-googlelint)
              (flycheck-add-next-checker 'c/c++-googlelint 'c/c++-cppcheck)))
  ;; Disable lsp as the standard flycheck checker
  ;; (setq lsp-diagnostic-package :none)

  ;; Python lsp-pyright (Do not start it in all directories)
  (advice-add 'lsp
              :before
              (lambda (&rest _args)
                (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

  ;; https://github.com/syl20bnr/spacemacs/issues/13849#issuecomment-674560260
  (with-eval-after-load 'evil-iedit-state
    (fset 'iedit-cleanup 'iedit-lib-cleanup))

  ;; Avy
  (setq avy-case-fold-search nil) ;; make avy case sensitive

  ;; Org
  (with-eval-after-load 'org
    (setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "PR" "ONHOLD" "|" "DONE")))
    '(require 'ox-jira nil t)
    )

  ;; lsp no ui on hover
  (setq lsp-ui-doc-enable nil)

  ;; Add shell script highligting to bbclass files (Yocto)
  ;; (add-to-list 'auto-mode-alist '(("\\.bbclass.*\\'") . shell-script-mode))

  ;; Add Sagemath files as python mode file
  (add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))

  ;; Add bitbake.el as a syntax highlighter for bitbake files
  ;; TODO - fix regex!
  ;; (add-to-list 'auto-mode-alist '(".bbclass\\|.inc\\|.bb\\|.bbappend" . bitbake-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.scm\\'" . prettify-symbols-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.amb" . scheme-mode))

  ;; ("\\.el\\'" . emacs-lisp-mode)
  ;; (setq mmm-global-mode 'auto) -- NOTE : try to fix bitbake highlighting.

  ;;
  ;;; Org archive all tasks marked as DONE
  ;;; And add them to a key in org-mode
  ;;

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file))
  (define-key spacemacs-org-mode-map (kbd "A") 'org-archive-done-tasks)


  ;; ediff combine both diffs
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-B-to-ediff-mode-map () (define-key ediff-mode-map "B" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-B-to-ediff-mode-map)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((eval add-hook 'before-save-hook 'time-stamp)))
 '(org-agenda-files '("/home/olepor/Documents/journal/20230720"))
 '(package-selected-packages
   '(devicetree-ts-mode dts-mode ccls cmake-mode lsp-docker doom-modeline nerd-icons flycheck-google-cpplint magit transient helm-ls-git helm helm-core hl-todo logview consult lsp-origami lsp-mode treemacs markdown-mode org-modern org-projectile org-project-capture org-category-capture yasnippet-snippets evil yapfify ws-butler writeroom-mode winum which-key wfnames web-mode web-beautify volatile-highlights vim-powerline vi-tilde-fringe uuidgen unicode-fonts undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toc-org term-cursor tagedit symon symbol-overlay string-inflection string-edit-at-point sqlup-mode sql-indent sphinx-doc spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline space-doc smeargle slim-mode shrink-path shfmt scss-mode sass-mode rjsx-mode restart-emacs realgud react-snippets rainbow-delimiters quickrun pytest pylookup pyenv-mode pydoc py-isort pug-mode prettier-js popwin poetry pippel pipenv pip-requirements pcre2el password-generator paradox pandoc-mode ox-pandoc ox-hugo ox-gfm overseer origami orgit-forge org-superstar org-rich-yank org-present org-pomodoro org-mime org-journal org-download org-contrib org-cliplink open-junk-file ob-cfengine3 npm-mode nose nodejs-repl nameless mustache-mode multi-line mmm-mode markdown-toc magit-section macrostep lsp-ui lsp-python-ms lsp-pyright lsp-latex lorem-ipsum livid-mode live-py-mode link-hint ligature k8s-mode json-reformat json-navigator json-mode js2-refactor js-doc inspector insert-shebang info+ indent-guide importmagic import-js impatient-mode hybrid-mode hungry-delete holy-mode highlight-parentheses highlight-numbers highlight-indentation hide-comnt helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-git-grep helm-descbinds helm-ctest helm-css-scss helm-company helm-c-yasnippet helm-ag goto-chg google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link git-commit gh-md gendoxy fuzzy flyspell-correct-helm flycheck-ycmd flycheck-rtags flycheck-pos-tip flycheck-package flycheck-golangci-lint flycheck-elsa flycheck-bashate flx-ido fish-mode fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-tex evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view emr emmet-mode elisp-slime-nav elisp-def ein editorconfig dumb-jump drag-stuff dotenv-mode dockerfile-mode docker disaster dired-quick-sort diminish devdocs define-word datetime dap-mode cython-mode cpp-auto-include company-ycmd company-web company-shell company-rtags company-reftex company-math company-go company-c-headers company-auctex company-anaconda column-enforce-mode code-cells clean-aindent-mode cfrs centered-cursor-mode blacken auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile all-the-icons aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))
 '(safe-local-variable-values
   '((flycheck-googlelint-filter "-whitespace,+whitespace/braces")
     (flycheck-checker . c/c++-googlelint)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
