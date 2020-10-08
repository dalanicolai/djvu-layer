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
  ;; '(djvu)
  '((djvu :location (recipe
                     :fetcher github
                     :repo "dalanicolai/djvu2.el"))))

(defun djvu/init-djvu ()
  (use-package djvu
    :defer t
    :init 
    (progn
      (add-hook 'djvu-read-mode-hook (lambda () (setq imenu-create-index-function #'djvu-imenu-create-index)))
      (add-hook 'djvu-read-mode-hook (lambda () (setq imenu-default-goto-function (lambda (title page) (djvu-goto-page page djvu-doc)))))
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
      (add-to-list 'spacemacs-large-file-modes-list 'djvu-read-mode t)
      (advice-add 'djvu-find-file :after #'djvu-advise-image-toggle)
      (evilified-state-evilify djvu-read-mode djvu-read-mode-map
        "j"         'djvu-scroll-up-or-next-page
        "k"         'djvu-scroll-down-or-previous-page
        "J"         'djvu-next-page
        "K"         'djvu-prev-page
        "g"         'djvu-goto-page
        "/"         'djvu-fast-search
        "n"         'djvu-re-search-forward-continue
        (kbd "C-o") 'djvu-history-backward
        (kbd "C-i") 'djvu-history-forward
        (kbd "SPC f s") 'djvu-save
        )
      (spacemacs/set-leader-keys-for-major-mode 'djvu-read-mode "s" 'djvu-occur)

      (define-key djvu-read-mode-map [remap save-buffer] 'djvu-save)

      (evilified-state-evilify djvu-occur-mode djvu-occur-mode-map)
      )))

(defun djvu/init-djvu-annots ()
  (use-package djvu-annots
    :defer t
    ))

;;; packages.el ends here
