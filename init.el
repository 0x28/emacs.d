;;; init.el -- initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (load (expand-file-name "settings.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
