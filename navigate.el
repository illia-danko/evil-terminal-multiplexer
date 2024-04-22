;;; navigate.el --- Seamlessly navigate between Emacs and WezTerm

;; Author:   Keith Smiley <keithbsmiley@gmail.com>
;;           Illia Danko <illia@danko.me>
;; Created:  December 17 2023
;; Version:  0.2.1
;; Keywords: wezterm, evil, vi, vim

;;; Commentary:

;; This package is inspired by evil-tmux-navigator.
;; It allows you to navigate splits in evil mode
;; Along with wezterm splits with the same commands
;; Include with:
;;
;;    (require 'navigate)
;;

;;; Code:

(require 'evil)

(defgroup navigate nil
  "seamlessly navigate between Emacs and wezterm"
  :prefix "navigate-"
  :group 'evil)

;; Without unsetting C-h this is useless
(global-unset-key (kbd "C-h"))

;; This requires windmove commands
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun wezterm-navigate (direction)
  (let
      ((cmd (concat "windmove-" direction)))
    (condition-case nil
        (funcall (read cmd))
      (error
       (wezterm-command direction)))))

(defun wezterm-command (direction)
  (let ((pane-id (shell-command-to-string
				  (concat "wezterm cli get-pane-direction "
						  direction))))
	(shell-command-to-string
	 (concat "wezterm cli activate-pane --pane-id "
			 pane-id))))

(define-key evil-normal-state-map
            (kbd "C-h")
            (lambda ()
              (interactive)
              (wezterm-navigate "left")))
(define-key evil-normal-state-map
            (kbd "C-j")
            (lambda ()
              (interactive)
              (wezterm-navigate "down")))
(define-key evil-normal-state-map
            (kbd "C-k")
            (lambda ()
              (interactive)
              (wezterm-navigate "up")))
(define-key evil-normal-state-map
            (kbd "C-l")
            (lambda ()
              (interactive)
              (wezterm-navigate "right")))

(provide 'navigate)

;;; navigate.el ends here
