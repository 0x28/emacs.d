;;; init.el -- test
;;; Commentary:
;;; Code:


;; (package-initialize)

(let ((gc-cons-threshold most-positive-fixnum)
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
 '(org-block-background ((t (:background "gray6"))))
 '(whitespace-trailing ((t (:background "orange red" :foreground "black")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-src-fontify-natively t))
