;; init.el of laerling
;; Copyright (C) 2020, laerling <laerling@posteo.de>

(progn ;; packaging

  ;; must stand at the beginning
  (package-initialize)

  ;; define packages to install
  (setq package-list '(

		       ;; UI
		       magit
		       rainbow-delimiters
		       smart-window
		       swiper
		       tron-legacy-theme
		       which-key
		       zygospore

		       ;; programming modes
		       go-mode
		       json-mode ;; dependency of nix-mode
		       markdown-mode
		       nix-mode
		       rust-mode
		       yaml-mode

		       ;; themes
		       acme-theme
		       gruvbox-theme
		       spacemacs-theme
		       ubuntu-theme
		       zenburn-theme

		       ))

  ;; prepare cache
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless package--initialized (package-initialize t))

  ;; install packages
  (dolist (package package-list)
    (condition-case nil
    	(package-install package)
      (error
       (message
    	"Installing package " (symbol-name package)
	"failed. Refreshing package list and retrying.")
       (package-refresh-contents)
       (package-install package))) )
  )

(progn ;; functions

  (defun find-init-file (&optional other-window)
    "Find the file this function was defined in.
If OTHER-WINDOW is non-nil, find init file in other window."
    (interactive "P")
    (let ((init-file-name (symbol-file 'find-init-file)))
      (if other-window
	  (find-file-other-window init-file-name)
	(find-file init-file-name))))

  (defun find-custom-file (&optional other-window)
    "Find the custom.el file.
If OTHER-WINDOW is non-nil, find custom.el file in other window."
    (interactive "P")
    (let ((custom-file-name
	   (concat (url-basepath (symbol-file 'find-custom-file)) "custom.el")))
      (if other-window
	  (find-file-other-window custom-file-name)
	(find-file custom-file-name))))

  (defun switch-to-scratch-buffer (&optional other-window)
    "Switch to the scratch buffer, creating it if it doesn't
exist already. If OTHER-WINDOW is non-nil, switch to the scratch
buffer in other window.  If the buffer didn't exist before, call
SET-BUFFER-MAJOR-MODE and insert INITIAL-SCRATCH-MESSAGE."
    (interactive "P")
    (let* ((switch-buffer-function
	    (if other-window
		'switch-to-buffer-other-window 'switch-to-buffer))
	   (existing-buffer (get-buffer "*scratch*"))
	   (buffer-to-switch-to (or existing-buffer (generate-new-buffer "*scratch*"))))
      (apply switch-buffer-function (list buffer-to-switch-to))
      (unless existing-buffer
	(set-buffer-major-mode (current-buffer))
	(insert initial-scratch-message))
      ))

  (defun disable-all-themes ()
    "Disable all active themes."
    (interactive)
    (dolist (active-theme custom-enabled-themes)
      (disable-theme active-theme)))

  (defun switch-to-theme (theme)
    "Disable all active themes and load THEME."
    (interactive
     (list (intern (completing-read "Switch to theme: "
				    (sort (mapcar 'symbol-name
						  (custom-available-themes)
						  ) 'string<)))))
    (when (not (member theme (custom-available-themes)))
      (error "No such theme: %S" theme))
    (disable-all-themes)
    (load-theme theme 'NO-CONFIRM))

  ;; kill-buffer-and-frame is just kill-buffer-and-window with s/window/frame/
  (defun kill-buffer-and-frame ()
    "Kill the current buffer and delete the selected frame."
    (interactive)
    (let ((frame-to-delete (selected-frame))
	  (buffer-to-kill (current-buffer))
	  (delete-frame-hook (lambda () (ignore-errors (delete-frame)))))
      (unwind-protect
	  (progn
	    (add-hook 'kill-buffer-hook delete-frame-hook t t)
	    (if (kill-buffer (current-buffer))
		;; If `delete-frame' failed before, we rerun it to regenerate
		;; the error so it can be seen in the echo area.
		(when (eq (selected-frame) frame-to-delete)
		  (delete-frame))))
	;; If the buffer is not dead for some reason (probably because
	;; of a `quit' signal), remove the hook again.
	(ignore-errors
	  (with-current-buffer buffer-to-kill
	    (remove-hook 'kill-buffer-hook delete-frame-hook t))))))
  )

(progn ;; handling (keybindings etc.)

  ;; don't be an Emacs newbie
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq disabled-command-function nil)

  ;; basic
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-x i") 'find-init-file)
  (global-set-key (kbd "C-x c") 'find-custom-file)
  (global-set-key (kbd "C-x m") 'eshell)
  (global-set-key (kbd "C-x t") 'toggle-truncate-lines)
  (global-set-key (kbd "C-x p") 'list-processes)

  ;; package basics
  (ivy-mode 1)
  (which-key-mode 1)
  (global-set-key (kbd "C-M-s") 'swiper)

  ;; buffers
  (global-set-key (kbd "C-x C-n") 'next-buffer)
  (global-set-key (kbd "C-x C-p") 'previous-buffer)
  (global-set-key (kbd "C-x s") 'switch-to-scratch-buffer)
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
  (global-set-key (kbd "C-x C-r") 'rename-buffer)
  (global-set-key (kbd "C-x 6 0") 'kill-buffer-and-frame)
  (global-set-key (kbd "C-x C-k") 'kill-current-buffer)
  (global-set-key (kbd "C-<tab>") 'other-window)

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

  ;; Set C-x 2 and C-x 3 back to original bindings.
  ;; If I want to open a specific buffer in another window, I'll just use C-x 4 b
  (require 'smart-window)
  (setq smart-window-remap-keys 0)
  (global-set-key (kbd "C-x 2") 'split-window-below)
  (global-set-key (kbd "C-x 3") 'split-window-right)

  ;; themes
  (global-set-key (kbd "C-x C-y") 'switch-to-theme)
  (global-set-key (kbd "C-x y") 'disable-all-themes)

  ;; git
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; grepping
  (setq grep-command "grep --color -n -r ") ;; -n is needed for proper rendering in the *grep* buffer
  )

(progn ;; looks and sound

  ;; fix the UI
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  ;; use rainbow delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

  ;; show the column we're in
  (column-number-mode 1)

  ;; make the scratch buffer more welcoming
  (setq initial-scratch-message "")
  (setq initial-major-mode 'text-mode)

  ;; don't display the Emacs main screen
  (setq inhibit-startup-screen t)

  ;; disable the bell
  (setq visible-bell nil)
  (setq ring-bell-function (lambda ()))

  ;; load custom script, if available
  (let ((custom-file (concat (file-name-directory (or load-file-name "")) "custom.el")))
    (when (file-exists-p custom-file)
      (load-file custom-file)))
  )

(progn ;; server
  (require 'server)
  (unless (server-running-p) (server-start))
  )

;; custom-set-variables beyond this point
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   '("387b487737860e18cbb92d83a42616a67c1edfd0664d521940e7fbf049c315ae" default))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(package-selected-packages
   '(tron-legacy-theme ubuntu-theme gruvbox-theme acme-theme yaml-mode which-key swiper rainbow-delimiters zenburn-theme zygospore spacemacs-theme rust-mode nix-mode markdown-mode json-mode go-mode smart-window magit))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140)))))
