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

(mish/personal-config-dir)
(org-babel-load-file (concat (mish/personal-config-dir) "/" "config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(projectile tree-sitter-langs treesit tree-sitter merlin 0blayout which-key vertico use-package treesit-auto rainbow-delimiters python-mode org-bullets orderless nerd-icons-completion marginalia lsp-ui lsp-pyright lsp-ivy ivy-rich helpful general forge evil-nerd-commenter evil-collection embark-consult doom-themes doom-modeline dap-mode counsel-projectile company-box)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
