;;; anything-zsh-screen.el --- zsh completion system with `anything'

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: convenience, anything

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Zsh completion system with Anything interface.

;;; Installation:

;; Put the anything-zsh-screen.el to your load-path.
;; Put the anything-zsh-screen.zsh (the helper zsh script file) to the
;; location poited by `anything-zsh-screen-script-file-name'.
;; (defualt: ~/.emacs.d)

;; Start a screen session named by `anything-zsh-screen-session-name' and
;; within that session, run the start up commands.
;; Like below.
;; % screen -S anything-zsh-screen
;; % source ~/.emacs.d/anything-zsh-screen.zsh
;; % anything-zsh-screen-rc
;; (You can now detach this screen session.)

;; And the following to your ~/.emacs startup file.
;; (eval-after-load
;;  'shell
;;  '(define-key shell-mode-map "\e\t" 'anything-zsh-screen-simple-complete))

;;; Note:

;; This package requires several working executables listed below,
;; Perl, ruby1.9, zsh and screen.

;;; Code:

(require 'anything)
(require 'comint)
(require 'term)

(defvar anything-zsh-screen-script-file-name
  (expand-file-name "~/.emacs.d/anything-zsh-screen.zsh")
  "Location of the installed zsh script file.

Please install the zsh script file (`anything-zsh-screen.zsh' bundled with
this `anything-zsh-screen' distribution) to this location.")
(defvar anything-zsh-screen-session-name
  "anything-zsh-screen"
  "Session name of the screen to which this elisp program communicates.")
(defvar anything-zsh-screen-exchange-file-name
  (expand-file-name "~/tmp/anything-zsh-screen-exchange")
  "Location of which will be passed as screen's `hardcopy -h' argument.")
(defvar anything-zsh-screen-scrollback
  9999
  "Number of the lines to be passed as screen's history scrollback buffer.")
(defvar anything-zsh-screen-zle-line-source-name
  "command line"
  "Name of the anything-source which is supposed to be displayed at the zle.")

(define-anything-type-attribute 'anything-zsh-screen-read
  '((display-to-real . azs-display-to-real)
    (action . (("Insert" . azs-insert))))
  "Anything Zsh Screen Read.")

(defun azs-display-to-real (disp)
  (let ((target (thing-at-point 'symbol))
        (sname (assoc-default 'name (anything-get-current-source))))
    (cond ((and (string-match "option" sname)
                target
                (string-match "^-[^-]+" target)) ;; short option
           (substring disp 1))
          (t disp))))

(defun azs-simple-insert-candidate (cand)
  (let* ((len (azs-get-prefix-length))
         (del-len (if (< len 0) 0 len)))
    (delete-region (- (point) del-len) (point))
    (insert cand)))
(defun azs-term-insert-candidate (cand)
  (dotimes (_ (azs-get-prefix-length))
    (term-send-backspace))
  (term-send-raw-string cand))
(defun azs-insert (cand)
  (let ((insert-func (funcall anything-zsh-screen-get-insert-func)))
    (funcall insert-func cand)))
(defvar anything-zsh-screen-get-insert-func
  (lambda ()
    (if (eq major-mode 'term-mode)
      #'azs-term-insert-candidate
      #'azs-simple-insert-candidate)))

(defvar anything-zsh-screen-complete-target "")
(defun azs-get-prefix-length ()
  (let ((sname (assoc-default 'name (anything-get-current-source))))
    (cond ((string= anything-zsh-screen-zle-line-source-name sname)
           (length anything-zsh-screen-complete-target))
          ((string= "directory stack" sname) 1)
          ((string-match "\\(:?file\\|director\\(:?ies\\|y\\)\\)" sname)
           (let ((name-maybe
                  (file-name-nondirectory (thing-at-point 'filename))))
             (if (string= "" name-maybe)
               0
               (length name-maybe))))
          ((null (thing-at-point 'symbol)) 0)
          ((string-match "option" sname)
           (let ((s (thing-at-point 'symbol)))
             (cond ((string= "-" s) 1)
                   ((string= "--" s) 2)
                   (t 0))))
          (t (length (thing-at-point 'symbol))))))

(defun azs-get-hardcopy (arg chdir)
  (if (zerop
       (call-process "zsh" nil nil nil
                     "-c"
                     (concat
                      "source " anything-zsh-screen-script-file-name ";"
                      (concat
                       "anything-zsh-screen-run"
                       " " anything-zsh-screen-exchange-file-name
                       " " anything-zsh-screen-session-name
                       " " (number-to-string anything-zsh-screen-scrollback)
                       " '" arg "' "
                       " '" anything-zsh-screen-zle-line-source-name "'"
                       " 'stuff \"cd " chdir "^M\"' "
                       " 'stuff \"" arg "^I^X^X\"'"))))
    anything-zsh-screen-exchange-file-name
    (error "Error while communicating with the screen session")))

(defun azs-read-sources (arg &optional chdir)
  (anything-aif (azs-get-hardcopy arg (or chdir default-directory))
      (read (with-temp-buffer
              (insert-file-contents-literally it)
              (buffer-string)))))

(defun azs-complete (sources target)
  (let ((anything-sources sources)
        (anything-zsh-screen-complete-target target)
        (anything-execute-action-at-once-if-one t))
    (anything sources
              nil
              "pattern: "
              nil nil "*anything zsh screen*")))

(defun azs-want-zsh-expander (target)
  (string-match "{" target))
(defun azs-complete-cancel-anything ()
  '(message "Sorry for the inconvenience")
  (when (eq major-mode 'term-mode)
    (term-send-raw-string "\C-I")))

(defun anything-zsh-screen-complete (get-target &optional extend-sources)
  (let ((extend-sources (or extend-sources #'identity))
        (target (funcall get-target)))
    (if (azs-want-zsh-expander target)
      (azs-complete-cancel-anything)
      (azs-complete (funcall extend-sources (azs-read-sources target))
                    target))))

(defun anything-zsh-screen-simple-complete ()
  (interactive)
  (anything-zsh-screen-complete (lambda ()
                                  (buffer-substring-no-properties
                                   (comint-line-beginning-position)
                                   (point)))))

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-zsh-screen-complete
                                '(azs-get-prefix-length)))

(provide 'anything-zsh-screen)
;;; anything-zsh-screen.el ends here
