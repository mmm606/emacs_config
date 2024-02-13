;;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(menu-bar-mode -1)

(set-fringe-mode 10)

(setq visible-bell t)

(global-display-line-numbers-mode t)
(column-number-mode)
;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq scroll-margin 8)
(setq hscroll-margin 8)
(setq scroll-conservatively 1000)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package doom-themes
  :init
  (load-theme 'doom-solarized-light t))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-d-scroll nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-fine-undo t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (setq forge-add-default-bindings nil))

(use-package nerd-icons)

(use-package vertico
  :init
  (vertico-mode)

  (setq vertico-scroll-margin 2)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :bind
  (([remap bookmark-jump]                 . consult-bookmark)
   ([remap evil-show-marks]               . consult-mark)
   ([remap evil-show-jumps]               . +vertico/jump-list)
   ([remap evil-show-registers]           . consult-register)
   ([remap goto-line]                     . consult-goto-line)
   ([remap imenu]                         . consult-imenu)
   ([remap Info-search]                   . consult-info)
   ([remap locate]                        . consult-locate)
   ([remap load-theme]                    . consult-theme)
   ([remap man]                           . consult-man)
   ([remap recentf-open-files]            . consult-recent-file)
   ([remap switch-to-buffer]              . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
   ([remap yank-pop]                      . consult-yank-pop)
   ([remap persp-switch-to-buffer]        . +vertico/switch-workspace-buffer))
  :config
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; runs default action on selection
   ("C-h B" . embark-bindings)
   ;; ([remap describe-bindings] . embark-bindings)
   )

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; (setq completion-styles '(substring orderless basic))
;; (keymap-set vertico-map "TAB" #'minibuffer-complete)

(keymap-set vertico-map "?" #'minibuffer-completion-help)

(setq completion-in-region-function
      (lambda (&rest args)
	(apply (if vertico-mode
		   #'consult-completion-in-region
		 #'completion--in-region)
	       args)))

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))

(use-package helpful
  :custom
  (council-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defun mish/personal-config-dir ()
  "This function will get the directory that the user's init file is in"
  (interactive)
  (if-let (
	   (is-bound (boundp 'chemacs-profile))
	   (its-assoc (assoc 'user-emacs-directory chemacs-profile))
	   )
      (cdr its-assoc)
    (file-name-directory user-init-file)
    )
  )

(defun mish/open-personal-config-dir ()
  "This function will open the directory of the user's init file"
  (interactive)
  (let ((default-directory (concat (mish/personal-config-dir) "/")))
    (call-interactively 'find-file)))

(defun mish/call-in-order (alist-of-func-keys-and-arg-values)
  "This function takes an alist of key values being a function and the values being a list of args. It will try to call each one in order if and only if it is bound. If it is bound and returns a truthy value then we stop and return that value"
  (cl-some
   (lambda (key-value-pair)
     (let
	 (
	  (func (car key-value-pair))
	  (args (cdr key-value-pair))
	  )
       (and
	(message "doing it")
	(fboundp func)
	(message "it was bound")
	(apply func args)
	)
       )
     )
   alist-of-func-keys-and-arg-values
   )
  )


(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (defvar-keymap rune/no-prefix-leader-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/toggle-keymap
    "." '(find-file :which-key "open file system")
    "/" '(consult-ripgrep))

  (defvar-keymap rune/toggle-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/toggle-keymap
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text"))

  (defvar-keymap rune/insert-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/toggle-keymap
    "s" '(org-insert-structure-template :which-key "insert source block"))

  (defvar-keymap rune/buffer-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/buffer-keymap
    "b" '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "switch to buffer")
    "bk" '(kill-buffer-and-window :which-key "kill buffer"))

  (defvar-keymap rune/window-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/window-keymap
    "w" '(:ignore t :which-key "window")
    "wk" '(evil-window-up :which-key "window up")
    "wj" '(evil-window-up :which-key "window down")
    "wh" '(evil-window-left :which-key "window left")
    "wl" '(evil-window-right :which-key "window right")
    "wc" '(evil-window-delete :which-key "close window")
    "wC" '(delete-other-windows :which-key "close all other windows"))

  (defvar-keymap rune/code-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/code-keymap
    "c" '(:ignore t :which-key "code")
    "cd" '(xref-find-definitions :which-key "jump to definition"))

  (defvar-keymap rune/file-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/file-keymap
    "f" '(:ignore t :which-key "file")
    "fp" '(mish/open-personal-config-dir :which-key "open personal config"))

  (defvar-keymap rune/popup-keymap)
  (rune/leader-keys
    ;; :keymaps 'rune/popup-keymap
    "p" '(:ignore t :which-key "pop up")
    "pm" '(view-echo-area-messages :which-key "view messages"))

  (setq mish/help-map (copy-keymap help-map))

  (rune/leader-keys "h" '(:keymap mish/help-map :which-key "help"))

  (general-define-key
   :states '(normal visual insert emacs)
   "C-f" '(consult-line :which-key "find in buffer")))

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/Projects/Code")
;;     (setq projectile-project-search-path '("~/Projects/Code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(evil-collection-magit-setup)

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))
;; https://magit.vc/manual/ghub/Storing-a-Token.html

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package org
  :config
  (setq org-ellipsis " ▾"
	;; org-hide-emphasis-markers t
	)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/.emacs.d/tasks.org"))
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-tempo)
(setq org-confirm-babel-evaluate nil)

(setq org-structure-template-alist (delq (assoc "e" org-structure-template-alist) org-structure-template-alist))

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun rune/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/config.org"))

    (let ((org-confirm-babel-eval nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rune/org-babel-tangle-config)))

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix " l")
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-headerline-breadcrumb-mode t)
  :config
  (lsp-enable-which-key-integration t)
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'at-point)
  )

(use-package dap-mode)

(use-package company
  ;; :after lsp-mode
  ;; :hook
  ;; (lsp-mode . company-mode)
  :init
  (global-company-mode) 
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-base-mode . (lambda ()
			      (require 'lsp-pyright)
			      (lsp-deferred))))
