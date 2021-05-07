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
    (djvu3 :location (recipe
                      :fetcher github
                      :repo "dalanicolai/djvu3"))))

(defun djvu/init-djvu ()
  (use-package djvu
    :defer t
    :magic ("%DJVU" . djvu-read-mode)))

(defun djvu/init-djvu3 ()
  (use-package djvu3
    :after djvu
    :init
    (add-to-list 'spacemacs-large-file-modes-list 'djvu-read-mode t)
    (add-hook 'djvu-read-mode-hook (lambda () (setq imenu-create-index-function #'djvu-imenu-create-index)))
    (add-hook 'djvu-read-mode-hook (lambda () (setq imenu-default-goto-function (lambda (title page) (djvu-goto-page page djvu-doc)))))

    :config
    (advice-add 'djvu-find-file :after #'spacemacs/djvu-advise-image-toggle)

    (evilified-state-evilify-map djvu-read-mode-map
      :mode  djvu-read-mode
      :bindings
      "j" 'spacemacs/djvu-scroll-up-or-next-page
      "k" 'spacemacs/djvu-scroll-down-or-previous-page
      "J" 'djvu-next-page
      "K" 'djvu-prev-page
      "g" 'djvu-goto-page
      "/" 'spacemacs/djvu-fast-search
      "n" 'djvu-re-search-forward-continue
      "H" 'djvu-history-backward
      "L" 'djvu-history-forward
      "c" 'djvu-toggle-semi-continuous-scrolling
      "d" 'djvu-toggle-invert
      "q" 'djvu-kill-doc)

    (define-key djvu-image-mode-map "s" 'image-save)

    (spacemacs/declare-prefix-for-mode 'djvu-read-mode "mb" "buffers")
    (spacemacs/set-leader-keys-for-major-mode 'djvu-read-mode
      "s" 'djvu-occur
      "h" 'djvu-keyboard-annot
      "bs" 'djvu-switch-shared
      "bo" 'djvu-switch-outline
      "bt" 'djvu-switch-text
      "ba" 'djvu-switch-annot
      "bb" 'djvu-switch-bookmarks)

    ;; for some reason can not use dolist here
      (define-key djvu-read-mode-map [remap save-buffer] 'djvu-save)
      (define-key djvu-script-mode-map [remap save-buffer] 'djvu-save)
      (define-key djvu-outline-mode-map [remap save-buffer] 'djvu-save)

    (spacemacs/set-leader-keys-for-major-mode 'djvu-script-mode
      "r" 'djvu-switch-read
      "s" 'djvu-switch-shared
      "o" 'djvu-switch-outline
      "t" 'djvu-switch-text
      "a" 'djvu-switch-annot
      "b" 'djvu-switch-bookmarks)

    (evilified-state-evilify-map djvu-outline-mode-map
      :mode  djvu-outline-mode
      :bindings
      "q" 'djvu-quit-window)

    (evilified-state-evilify-map djvu-occur-mode-map
      :mode djvu-occur-mode
      :bindings
      (kbd "C-j") 'djvu-occur-next-entry-and-follow
      (kbd "C-k") 'djvu-occur-previous-entry-and-follow)))

;;; packages.el ends here
