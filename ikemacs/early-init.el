;;; early-init.el --- Attempt to help resolve the flickery startup behavior

;; Prevent the window from jumping sizes
(add-to-list 'default-frame-alist '(width . 130))
(add-to-list 'default-frame-alist '(height . 45))

;; Prevent the scrollbars, toolbars, and menus from flashing
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Prevent the package manager from doing extra work before init
(setq package-enable-at-startup nil)

(add-to-list 'default-frame-alist '(visibility . nil))
