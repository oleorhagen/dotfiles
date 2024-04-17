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
  '(

    ;; We only require logview-mode
    (logview :location (recipe
                        :fetcher github
                        :repo "doublep/logview"))

    ))


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
    :config

    (spacemacs/declare-prefix-for-mode 'logview-mode
      "mf" "filter")
    (spacemacs/set-leader-keys-for-major-mode 'logview-mode
      "fe" 'logview-edit-filters
      "fn" 'logview-add-include-name-filter
      "fN" 'logview-add-exclude-name-filter
      "ft" 'logview-add-include-thread-filter
      "fT" 'logview-add-exclude-thread-filter
      "fr" 'logview-reset-all-filters-restrictions-and-hidings)

    (spacemacs/declare-prefix-for-mode 'logview-mode
      "ml" "Show all levels")
    (spacemacs/set-leader-keys-for-major-mode 'logview-mode
      "l" 'logview-show-all-levels)

    (spacemacs/declare-prefix-for-mode 'logview-mode
      "mn" "narrow")
    (spacemacs/set-leader-keys-for-major-mode 'logview-mode
      ;; Narrow
      "nt" 'logview-narrow-to-thread
      "t" 'logview-edit-threads
      "w" 'logview-widen)

    (spacemacs/declare-prefix-for-mode 'logview-mode
      "mt" "threads")
    (spacemacs/declare-prefix-for-mode 'logview-mode
      "mt" "Edit threads")

    (spacemacs/declare-prefix-for-mode 'logview-mode
      "mv" "Views")
    (spacemacs/set-leader-keys-for-major-mode 'logview-mode
      ;; TODO - Views
      "vs" 'logview-save-filters-as-view-for-submode
      "ve" 'logview-edit-submode-views
      "vd" 'logview-delete-submode-views
      "vn" 'logview-set-navigation-view
      "vh" 'logview-highlight-view-entries)

    (spacemacs/set-leader-keys-for-major-mode 'logview-mode
      ;; TODO - Views
      "mvs" 'logview-save-filters-as-view-for-submode
      "mve" 'logview-edit-submode-views
      "mvd" 'logview-delete-submode-views
      "mvn" 'logview-set-navigation-view
      "mvh" 'logview-highlight-view-entries)

    (spacemacs/declare-prefix-for-mode 'logview-mode "mf" "guru")
    ;; (spacemacs/set-leader-keys-for-major-mode 'logview-mode
    ;;   "f<" 'go-guru-callers
    ;;   "f>" 'go-guru-callees
    ;;   "fc" 'go-guru-peers
    ;;   "fd" 'go-guru-describe
    ;;   "fe" 'go-guru-whicherrs
    ;;   "ff" 'go-guru-freevars
    ;;   "fi" 'go-guru-implements
    ;;   "fj" 'go-guru-definition
    ;;   "fo" 'go-guru-set-scope
    ;;   "fp" 'go-guru-pointsto
    ;;   "fr" 'go-guru-referrers
    ;;   "fs" 'go-guru-callstack)))
    ))
