;;; ikemacs-packages.el --- Package management configuration -*- lexical-binding: t; -*-

(require 'package)

;; 1. Add MELPA to the list of repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; 2. Initialize the package system
(package-initialize)

;; 3. Refresh contents if we haven't yet (auto-bootstrap)
(unless package-archive-contents
  (package-refresh-contents))

;; 4. Install use-package if missing
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'ikemacs-packages)