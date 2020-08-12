;;; packages.el --- djvu layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
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
;; added to `djvu-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `djvu/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `djvu/pre-init-PACKAGE' and/or
;;   `djvu/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst djvu-packages
  '(djvu
    ))

(defun djvu/init-djvu ()
  (use-package djvu
    :defer t
    :init 
    (progn
      ;; (add-hook 'djvu-read-mode-hook #'djvu-image-toggle)
      ;; these following function do not work as intended
    (defun djvu-scroll-up-or-next-page ()
      (interactive)
      (scroll-up-line 5)
      (when (= (window-vscroll) 0)
        (djvu-next-page 1)))

    (defun djvu-scroll-down-or-previous-page ()
      (interactive)
      (if (not (= (window-vscroll) 0))
          (scroll-down-line 5)
        (djvu-prev-page 1)
        (scroll-up-command))))

    :config
    (progn
      (evilified-state-evilify djvu-mode djvu-read-mode-map
        "j"         'djvu-scroll-up-or-next-page
        "k"         'djvu-scroll-down-or-previous-page
        "J"         'djvu-next-page
        "K"         'djvu-prev-page
        "g"         'djvu-goto-page
        )
      ;; (djvu-image-toggle)
      )))
      ;; (evil-define-key djvu-read-mode-map "j" 'djvu-scroll-up-or-next-page)
      ;; (evil-define-key djvu-read-mode-map "k" 'djvu-scroll-donw-or-previous-page))))


;;; packages.el ends here
