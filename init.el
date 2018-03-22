;;; init.el -- initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; (package-initialize)

(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (org-babel-load-file
   (expand-file-name "settings.org"
                     user-emacs-directory)))

(provide 'init)

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((t (:foreground "green" :weight bold))))
 '(mode-line ((t (:background "gray10" :foreground "#EBDBB2" :box (:line-width -1 :color "black")))))
 '(org-block-background ((t (:background "gray6"))))
 '(whitespace-trailing ((t (:background "orange red" :foreground "black")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-src-fontify-natively t))
