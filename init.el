
;; Get rid of old looking defaults and set some basic visual stuff

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)


(global-display-line-numbers-mode t)
(column-number-mode)
;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make sure we have melpa and can use-package + set up use-package

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; get evil keybindings

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

;; make deletions go into the d register. To paste them do: "dp
;; (defun mish/evil-delete (orig-fn beg end &optional type _ &rest args)
;;   (apply orig-fn beg end type ?_ args))

;; (advice-add 'evil-delete :around 'mish/evil-delete)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treesit-auto evil-nerd-commenter lsp-ui company-box company lsp-pyright python-mode dap-python dap-mode lsp-ivy lsp-mode general helpful ivy-rich counsel ivy which-key doom-modeline nerd-icons rainbow-delimiters doom-themes evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Lets change the look of things

(use-package doom-themes)
(load-theme 'doom-solarized-light t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; NOTE: The first time you load your configuration on a new machine, you'll need to run the following command interactively so that the mode like icons display correctly:
;; M-x nerd-icons-install-fonts
(use-package nerd-icons)

;; make it easier to remember shortcuts with whichkey

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))

;; Ivy will help us with autocompletion

(use-package ivy
  :init
  (ivy-mode t)
  ;; (setq ivy-mode-map nil) ;; stops ivy from making its keybinds
  (setq ivy-use-selectable-prompt t)
  :bind
  (:map ivy-minibuffer-map
	("C-j" . ivy-next-line-or-history)
	("C-k" . ivy-previous-line-or-history)
	)
  )
;; Counsel will give us descriptions of functions, help search through other files, and use ivy's autocompletes

(use-package counsel
  :after
  ivy
  :init
  (counsel-mode t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; swiper will let us find within a file

(use-package swiper
  :after ivy
  :init)

;; helpful will give us prettier documentation
(use-package helpful
  :custom
  (council-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; general will help us do keymappings with whichkey integration

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  
  ;; This leader is for my cross buffer actions
  (general-create-definer mish/global-leader-definer
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"
    )

  (mish/global-leader-definer
    "b" '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "switch to buffer")
    "bk" '(kill-buffer-and-window :which-key "kill buffer")

    "w" '(:ignore t :which-key "window")
    "wk" '(evil-window-up :which-key "window up")
    "wj" '(evil-window-up :which-key "window down")
    "wh" '(evil-window-left :which-key "window left")
    "wl" '(evil-window-right :which-key "window right")
    "wc" '(evil-window-delete :which-key "close window")
    "wC" '(delete-other-windows :which-key "close all other windows")
    )
  )

;; need to increase garbage collector threshold for lsp
(setq gc-cons-threshold 100000000)
;; some of the language server responses are in 800k - 3M range
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; automatically install treesitter grammars
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

;; integrate lsp with ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-pyright
  :ensure t
  :hook (python-base-mode . (lambda ()
			      (require 'lsp-pyright)
			      (lsp-deferred))))

;; Nicer UI for lsp completions
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

;; make the company completion box look nicer
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'at-point)
  )

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))
