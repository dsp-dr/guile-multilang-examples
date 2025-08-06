;;; project-name.el --- Development environment for Guile Multilang Examples

;; Enhanced Scheme development environment with Geiser, Paredit, and project support

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Essential packages for Scheme development
(defvar required-packages 
  '(geiser-guile paredit company projectile magit org-mode))

;; Install missing packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package required-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Project configuration
(setq project-name "guile-multilang-examples")
(setq project-root (getenv "PROJECT_ROOT"))

;; Geiser configuration for Guile 3
(with-eval-after-load 'geiser-guile
  (setq geiser-guile-binary "guile3")
  (setq geiser-guile-load-path (list project-root 
                                     (concat project-root "/examples")
                                     (concat project-root "/elisp"))))

;; Paredit - structured editing for Lisp
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)

;; Company mode for completion
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.3))

;; Projectile for project management
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Org mode for documentation and notes
(require 'org)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (emacs-lisp . t)
   (shell . t)))

;; Convenient key bindings
(global-set-key (kbd "C-c g") 'geiser)
(global-set-key (kbd "C-c C-z") 'geiser-mode-switch-to-repl-and-enter)

;; Auto-mode associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

;; Custom functions for this project
(defun open-project-shell ()
  "Open a shell in the project root directory."
  (interactive)
  (let ((default-directory project-root))
    (shell (format "*shell-%s*" project-name))))

(defun compile-hoot-examples ()
  "Compile Hoot examples to WASM."
  (interactive)
  (let ((default-directory project-root))
    (compile "make hoot-example")))

(defun test-elisp-compilation ()
  "Test Elisp compilation examples."
  (interactive)
  (let ((default-directory project-root))
    (compile "make elisp-example")))

;; Project-specific menu
(define-key-after global-map [menu-bar project]
  (cons "Project" (make-sparse-keymap "Project")) 'tools)
(define-key global-map [menu-bar project shell]
  '("Project Shell" . open-project-shell))
(define-key global-map [menu-bar project hoot]
  '("Compile Hoot" . compile-hoot-examples))
(define-key global-map [menu-bar project elisp]
  '("Test Elisp" . test-elisp-compilation))

;; Welcome message
(message "Welcome to %s development environment!" project-name)
(message "Use C-c g to start Geiser, C-c p for Projectile commands")

;; Optional: Start Geiser automatically
;; (run-geiser 'guile)

;;; project-name.el ends here