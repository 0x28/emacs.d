;;; init.el -- test
;;; Commentary:
;;; Code:
(require 'org)

(let ((gc-cons-threshold most-positive-fixnum))
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
 '(org-block ((t (:background "gray9" :foreground "white smoke")))))
