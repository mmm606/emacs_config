
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
	     :config
	     (evil-mode 1))

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
   '(general helpful ivy-rich counsel ivy which-key doom-modeline nerd-icons rainbow-delimiters doom-themes evil-collection evil use-package)))
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
  (setq ivy-use-selectable-prompt t))

;; Counsel will give us descriptions of functions, help search through other files, and use ivy's autocompletes

(use-package counsel
  :after
  ivy
  :init
  (counsel-mode t))

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
  :config
  (general-evil-setup t)

  ;; This leader is for my cross buffer actions
  (general-create-definer mish/global-leader :prefix "SPC")

  ;; lets define cross buffer keymaps
  (mish/global-leader
    :states '(normal visual emacs)
    :keymaps 'override
   "b" '(:ignore t :which-key "buffer")
   "bs" '(switch-to-buffer :which-key "switch to buffer")
   "bc" '(delete-window :which-key "close window")
   "." '(find-file :which-key "open filesystem")
   )

  ;; This one is for buffer/mode specific actions
  (general-create-definer mish/local-leader :prefix "C-SPC")

  ;; lets define keymaps for local things
  (mish/local-leader
    :states '(normal insert visual emacs)
    :keymaps '(override prog-mode)
   "l" '(:ignore t :which-key "lsp")
   )
  )
