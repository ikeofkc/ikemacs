;;; install-packages.el --- Package installation script -*- lexical-binding: t; -*-

;;; Commentary:
;; Run this script to install all required packages
;; Usage: emacs --batch -l install-packages.el

;;; Code:

(require 'package)

;; Add package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Refresh package contents
(message "Refreshing package archives...")
(package-refresh-contents)

;; List of packages to install
(defvar my-package-list
  '(use-package
    doom-themes
    all-the-icons
    which-key
    vertico
    marginalia
    orderless
    org-bullets
    visual-fill-column
    org-modern
    general
    crdt)
  "List of packages to install.")

;; Optional packages (install if available)
(defvar my-optional-packages
  '(doom-modeline
    treemacs
    treemacs-all-the-icons)
  "List of optional packages.")

;; Install required packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (message "Installing %s..." package)
    (condition-case err
        (package-install package)
      (error (message "Failed to install %s: %s" package err)))))

;; Try to install optional packages
(dolist (package my-optional-packages)
  (unless (package-installed-p package)
    (message "Trying to install optional package %s..." package)
    (condition-case err
        (package-install package)
      (error (message "Optional package %s not available: %s" package err)))))

;; Summary
(message "\n=== Installation Summary ===")
(message "Required packages:")
(dolist (package my-package-list)
  (message "  %s: %s"
           package
           (if (package-installed-p package) "✓ Installed" "✗ Failed")))

(message "\nOptional packages:")
(dolist (package my-optional-packages)
  (message "  %s: %s"
           package
           (if (package-installed-p package) "✓ Installed" "○ Not available")))

(message "\nInstallation complete!")
(message "Restart Emacs to use the new configuration.")

;;; install-packages.el ends here
