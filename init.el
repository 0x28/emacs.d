;;; init.el -- initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (org-babel-load-file
   (expand-file-name "settings.org"
                     user-emacs-directory)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-argument-name ((t (:inherit (italic minibuffer-prompt)))))
 '(mode-line ((t (:overline "gainsboro" :background nil))))
 '(mode-line-inactive ((t (:overline "dim gray" :background nil))))
 '(org-block-begin-line ((t (:box -1))))
 '(org-block-end-line ((t (:box -1)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-at-pt-display-when-idle t nil (help-at-pt))
 '(org-src-fontify-natively t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))

(provide 'init)
;;; init.el ends here
