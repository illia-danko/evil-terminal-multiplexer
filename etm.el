;;; etm.el --- Seamlessly navigate between Emacs and a terminal multiplexer

;; Author:   Keith Smiley <keithbsmiley@gmail.com>
;;           Illia Danko <illia@danko.me>
;; Created:  December 17 2023
;; Version:  0.2.1
;; Keywords: wezterm, tmux, evil, vi, vim

;;; Commentary:

;; This package is inspired by evil-tmux-navigator.
;; It allows you to navigate and change splits in
;; the evil mode. Supports tmux and wezterm.
;; Include with:
;;
;;    (require 'etm)
;;

;;; Code:

(require 'evil)

(defgroup etm nil
  "The Terminal Multiplexter goodies. Supports WezTerm and Tmux"
  :prefix "etm-"
  :group 'evil)

;; Without unsetting C-h this is useless
(global-unset-key (kbd "C-h"))

;; This requires windmove commands.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun tmux-cmd (direction)
  (let ((tmux-direction (upcase
						 (substring direction 0 1))))
	(concat "tmux select-pane -" tmux-direction)))

(defun wezterm-cmd (direction)
  (let ((pane-id (shell-command-to-string
				  (concat "wezterm cli get-pane-direction "
						  direction))))
	(concat "wezterm cli activate-pane --pane-id " pane-id)))

(defun etm-cmd (direction)
  (let ((provider (getenv "TERM_PROGRAM")))
	(cond
	 ((string-equal provider "tmux") (tmux-cmd direction))
	 ((string-equal provider "WezTerm") (wezterm-cmd direction)))))

(defun etm-exec (direction)
  (shell-command-to-string
   (etm-cmd direction)))

(defun etm-move-cursor (direction)
  (let
	  ((cmd (concat "windmove-" direction)))
    (condition-case nil
        (funcall (read cmd))
	  (error
	   (etm-exec direction)))))

(define-key evil-normal-state-map
            (kbd "C-h")
            (lambda ()
			  (interactive)
			  (etm-move-cursor "left")))
(define-key evil-normal-state-map
            (kbd "C-j")
            (lambda ()
			  (interactive)
			  (etm-move-cursor "down")))
(define-key evil-normal-state-map
            (kbd "C-k")
            (lambda ()
			  (interactive)
			  (etm-move-cursor "up")))
(define-key evil-normal-state-map
            (kbd "C-l")
            (lambda ()
			  (interactive)
			  (etm-move-cursor "right")))

(provide 'etm)

;;; etm.el ends here
