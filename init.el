;; init.el of laerling
;; Copyright (C) 2020, laerling <laerling@posteo.de>

(progn ;; most important

  ;; define packages to install
  (setq package-list '(

		       ;; UI
		       magit
		       smart-window

		       ;; programming modes
		       go-mode
		       json-mode ;; dependency of nix-mode
		       markdown-mode
		       nix-mode
		       rust-mode

		       ;; themes
		       spacemacs-theme

		       ))

  ;; fix the UI
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  ;; make the scratch buffer more welcoming
  (setq initial-scratch-message "")
  (setq initial-major-mode 'text-mode)

  ;; disable the bell
  (setq ring-bell-function (lambda ()))
  )

(progn ;; packaging
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless package--initialized (package-initialize t))
  (unless package-archive-contents (package-refresh-contents))

  ;; install packages
  (dolist (package package-list)
    (package-install package))
  )

(progn ;; functions

  (defun find-init-file ()
    "Find the file this function was defined in."
    (interactive)
    (find-file (symbol-file 'find-init-file)))

  (defun switch-to-scratch-buffer ()
    "Switch to the scratch buffer, creating it if it doesn't exist already."
    (interactive)
    (let ((scratch-buffer (get-buffer "*scratch*")))
      (if scratch-buffer
	  (switch-to-buffer scratch-buffer)
	(switch-to-buffer (generate-new-buffer "*scratch*"))
	(set-buffer-major-mode (current-buffer))
	(insert initial-scratch-message))))

  (defun switch-to-theme (theme)
    "Disable all active themes and load THEME."
    (interactive
     (list (intern (completing-read "Switch to theme: "
				    (mapcar 'symbol-name
					    (custom-available-themes)
					    )))))
    (when (not (member theme (custom-available-themes)))
      (error "No such theme"))
    (dolist (active-theme custom-enabled-themes)
      (disable-theme active-theme))
    (load-theme theme 'NO-CONFIRM))
  )

(progn ;; handling (keybindings etc.

  ;; basic
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-x m") 'eshell)
  (global-set-key (kbd "C-x t") 'toggle-truncate-lines)
  (global-set-key (kbd "C-x s") 'switch-to-scratch-buffer)

  ;; buffers
  (global-set-key (kbd "M-n") 'next-buffer)
  (global-set-key (kbd "M-p") 'next-buffer)

  ;; windows
  (global-set-key (kbd "C-<up>"     ) 'windmove-up)
  (global-set-key (kbd "C-<down>"   ) 'windmove-down)
  (global-set-key (kbd "C-<left>"   ) 'windmove-left)
  (global-set-key (kbd "C-<right>"  ) 'windmove-right)
  (global-set-key (kbd "C-x <up>"   ) 'windmove-up)
  (global-set-key (kbd "C-x <down>" ) 'windmove-down)
  (global-set-key (kbd "C-x <left>" ) 'windmove-left)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x C-<up>"   ) (lambda () (interactive) (smart-window-move 'above)))
  (global-set-key (kbd "C-x C-<down>" ) (lambda () (interactive) (smart-window-move 'below)))
  (global-set-key (kbd "C-x C-<left>" ) (lambda () (interactive) (smart-window-move 'left)))
  (global-set-key (kbd "C-x C-<right>") (lambda () (interactive) (smart-window-move 'right)))

  ;; git
  (global-set-key (kbd "C-x g") 'magit-status)
  )

(progn ;; looks

  ;; set a theme
  (switch-to-theme 'spacemacs-dark)
  )


;; custom-set-variables beyond this point
