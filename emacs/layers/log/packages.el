;;; packages.el --- log layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author:  <olepor@olepor>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `log-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `log/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `log/pre-init-PACKAGE' and/or
;;   `log/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst log-packages
  '((logview :location (recipe
                        :fetcher github
                        :repo "doublep/logview")))
  "The list of Lisp packages required by the log layer.

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


;;
;;; TODO - Unix log does not pass our levels... Fix!
;;

;; (defvar logview-unix-with-level
;;   '("MENDER" . ((format . "TIMESTAMP THREAD NAME: TIMESTAMP level=LEVEL MESSAGE")
;;                 (levels . "RFC 5424 lowercase"))))



;;
;;; TODO - The UX should be so that all filter creation and edits is under one key
;;;        as opposed to now, that it's under a,t,m etc
;;

(defvar logview-unix-with-level
  '("MENDER" . ((format . "TIMESTAMP THREAD NAME: IGNORED lvl=LEVEL")
                (levels . "RFC 5424 lowercase"))))

(defvar logview-mender-logs-format
  '("MENDER-LOGS" . ((format . "TIMESTAMP [LEVEL]: >> THREAD-NAME")
                     (levels . "RFC 5424"))
    ))


;; TODO - In progress = not tested
;; (defvar logview-unix-with-level
  ;; '("PYTEST" . ((format . "[THREAD] IGNORED NAME")
                ;; No levels set for the pytest module)))


;;(add-to-list 'logview-additional-submodes logview-unix-with-level)

;;
;;; TODO - Maybe add a logview transient mode for spacemacs (?)
;;

;;
;;; TODO - Should be able to switch between the evilified keymap and logviews keymap (!)
;;

(defun log/init-logview ()
  (use-package logview
    :defer t
    :config (progn

              ;; Add the custom submode for parsing our logs (mender)
              ;; (log/add-mender-log-submode)

              ;; TODO - Re-enable adding these...
              ;;(add-to-list 'logview-additional-submodes logview-unix-with-level t)

              ;; (add-to-list 'logview-additional-submodes logview-mender-logs-format t)

              ;; TODO - how to use this properly (?)
              ;;
            ;;; Should I add it to the major mode ?
              ;;
            ;;; Or custom keybindings
              ;;
            ;;; One option is to 'evilify' the logview-key map
              ;;
            ;;; Another one is: Manually overriding the map
            ;;; Using (evil-define-key)
              ;;

              ;; Use magit as inspiration
              ;;
              ;; (kbd "?") - should show all the keys

              (evil-set-initial-state 'logview-mode 'emacs)

              ;; (evilified-state-evilify-map logview-mode-map
              ;;   :mode logview-mode
              ;;   :bindings
              ;;   ;; "j" 'logview-next-entry
              ;;   ;; "k" 'logview-previous-entry
              ;;   ;; "l" nil ;; <- Don't want to override this key (and some others)
              ;;   ;; "L" nil
              ;;   )
              ;; Add the leader key to the logview-mode-map
              (with-eval-after-load 'logview
                (define-key logview-mode-map
                  (kbd dotspacemacs-leader-key) spacemacs-default-map))


              ;;; Define keymaps for evil emulation
              (with-eval-after-load 'logview
                (define-key logview-mode-map "/" 'evil-search-forward)
                (define-key logview-mode-map ":" 'evil-ex)
                ;; (define-key logview-mode-map "h" 'evil-backward-char)
                (define-key logview-mode-map "j" 'evil-next-visual-line)
                (define-key logview-mode-map "k" 'evil-previous-visual-line)
                ;; (define-key logview-mode-map "l" 'evil-forward-char)
                ;; (define-key logview-mode-map "n" 'evil-search-next)
                ;; (define-key logview-mode-map "N" 'evil-search-previous)
                ;; (define-key logview-mode-map "v" 'evil-visual-char)
                ;; (define-key logview-mode-map "V" 'evil-visual-line)
                ;; (define-key logview-mode-map "gg" 'evil-goto-first-line)
                (define-key logview-mode-map "G" 'evil-goto-line)
                (define-key logview-mode-map (kbd "C-f") 'evil-scroll-page-down)
                (define-key logview-mode-map (kbd "C-b") 'evil-scroll-page-up)
                (define-key logview-mode-map (kbd "C-e") 'evil-scroll-line-down)
                (define-key logview-mode-map (kbd "C-y") 'evil-scroll-line-up)
                (define-key logview-mode-map (kbd "C-d") 'evil-scroll-down)
                (define-key logview-mode-map (kbd "C-u") 'evil-scroll-up)
                (define-key logview-mode-map (kbd "C-o") 'evil-jump-backward)
                )


              (spacemacs/set-leader-keys-for-major-mode 'logview-mode
                "fe" 'logview-edit-filters
                "fn" 'logview-add-include-name-filter
                "fN" 'logview-add-exclude-name-filter
                "ft" 'logview-add-include-thread-filter
                "fT" 'logview-add-exclude-thread-filter
                "fr" 'logview-reset-all-filters-restrictions-and-hidings
                "l" 'logview-show-all-levels

                ;; Narrow
                "nt" 'logview-narrow-to-thread
                "t" 'logview-edit-threads
                "w" 'logview-widen

                ;; TODO - Views
                "vs" 'logview-save-filters-as-view-for-submode
                "ve" 'logview-edit-submode-views
                "vd" 'logview-delete-submode-views
                "vn" 'logview-set-navigation-view
                "vh" 'logview-highlight-view-entries

                )

              ;; (evil-define-key 'normal logview-mode-map (kbd "j") 'logview-next-entry)
              ;; (evil-define-key 'normal logview-mode-map (kbd "k") 'logview-previous-entry)
              )))
