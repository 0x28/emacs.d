(setq package-quickstart t)

;; disable scroll bar
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
;; disable menu bar
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;; disable tool bar
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;; start emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; use hack or consolas font
(add-to-list 'default-frame-alist
             (cons 'font
                   (cond ((eq system-type 'gnu/linux) "Hack-13")
                         ((eq system-type 'windows-nt) "Consolas-14"))))
;; don't flash a white screen on startup
(add-to-list 'default-frame-alist '(background-color . "#000000"))
(add-to-list 'default-frame-alist '(foreground-color . "#ffffff"))
;; resize frame per pixel
(setq frame-resize-pixelwise t)
