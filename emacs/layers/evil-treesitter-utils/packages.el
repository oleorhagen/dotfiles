;;; packages.el --- evil-treesitter-utils layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author:  <olepor@t14-armata>
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
;; added to `evil-treesitter-utils-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `evil-treesitter-utils/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `evil-treesitter-utils/pre-init-PACKAGE' and/or
;;   `evil-treesitter-utils/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst evil-treesitter-utils-packages
  '(
    (evil-textobj-tree-sitter :ensure t)
    )
  "The list of Lisp packages required by the evil-treesitter-utils layer.

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


(defun evil-treesitter-utils/init-evil-textobj-tree-sitter ()
  (use-package evil-textobj-tree-sitter
    :ensure t
    :defer t
    :init

    ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
    (define-key evil-outer-text-objects-map
                "f"
                (evil-textobj-tree-sitter-get-textobj "function.outer"))

    ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
    (define-key evil-inner-text-objects-map
                "f"
                (evil-textobj-tree-sitter-get-textobj "function.inner"))

    ;; You can also bind multiple items and we will match the first one we can find
    (define-key evil-outer-text-objects-map
                "a"
                (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
    ;; Goto start of next function
    (define-key evil-normal-state-map
                (kbd "]f")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer")))

    ;; Goto start of previous function
    (define-key evil-normal-state-map
                (kbd "[f")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

    ;; Goto end of next function
    (define-key evil-normal-state-map
                (kbd "]F")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

    ;; Goto end of previous function
    (define-key evil-normal-state-map
                (kbd "[F")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))

    )

  )
