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
;;
;; Zsh completion system with Anything interface.

;;; Installation:
;;
;; Put the anything-zsh-screen.el to your load-path.
;; Put the anything-zsh-screen.zsh (the helper zsh script file) to the
;; location poited by `anything-zsh-screen-script-file-name'.
;; (defualt: ~/.emacs.d)
;;
;; Start a screen session named by `anything-zsh-screen-session-name' and
;; within that session, run the start up commands.
;; Like below.
;; % screen -S anything-zsh-screen
;; % source ~/.emacs.d/anything-zsh-screen.zsh
;; % anything-zsh-screen-rc
;; (You can now detach this screen session.)
;;
;; And the following to your ~/.emacs startup file.
;; (eval-after-load
;;  'shell
;;  '(define-key shell-mode-map "\e\t" 'anything-zsh-screen-simple-complete))

;;; Note:
;;
;; This package requires several working executables listed below,
;; Zsh and screen.

;;; Code:

(require 'anything)
(require 'comint)
(require 'term)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-zsh-screen-complete
                                '(anything-zsh-screen-get-prefix-length)))

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
(defvar anything-zsh-screen-run-awaits
  20
  "Number of the times to be passed as awaiting the screen's
exchange file generation.")

(define-anything-type-attribute 'anything-zsh-screen
  '((display-to-real . azs-display-to-real)
    (action . (("Insert" . azs-insert)))
    (volatile))
  "Anything Zsh Screen.")

(defun azs-display-to-real (disp)
  (let ((target (thing-at-point 'symbol))
        (sname (assoc-default 'name (anything-get-current-source))))
    (cond ((and (string-match "option" sname)
                target
                (string-match "^-[^-]+" target)) ;; short option
           (substring disp 1))
          (t disp))))

(defun azs-simple-insert-candidate (cand)
  (delete-region (- (point) (azs-get-prefix-length)) (point))
  (insert cand))
(defun azs-term-insert-candidate (cand)
  (dotimes (_ (azs-get-prefix-length))
    (term-send-backspace))
  (term-send-raw-string cand))
(defvar anything-zsh-screen-get-insert-func
  #'(lambda ()
      (if (eq major-mode 'term-mode)
        #'azs-term-insert-candidate
        #'azs-simple-insert-candidate)))
(defun azs-insert (cand)
  (let ((insert-func (funcall anything-zsh-screen-get-insert-func)))
    (funcall insert-func cand)))

(defvar anything-zsh-screen-complete-target "")
(defun azs-get-prefix-length ()
  (let ((target (thing-at-point 'symbol))
        (sname (assoc-default 'name (anything-get-current-source)))
        (length1 (lambda (target)
                   (if (and (string-match "^-[^-]" target)
                            (not (eq (char-before) ?\-)))
                     0
                     (length target)))))
    (cond ((string= anything-zsh-screen-zle-line-source-name sname)
           (length anything-zsh-screen-complete-target))
          ((string= "directory stack" sname) 1)
          ((string-match "\\(:?file\\|director\\(:?ies\\|y\\)\\)" sname)
           (let ((name-maybe
                  (file-name-nondirectory (thing-at-point 'filename))))
             (if (string= "" name-maybe)
               0
               (length name-maybe))))
          ((null target) 0)
          ((string-match "option" sname)
           (cond ((string= "-" target) 1)
                 ((string= "--" target) 2)
                 (t (funcall length1 target))))
          (t (funcall length1 target)))))

(defalias 'anything-zsh-screen-get-prefix-length 'azs-get-prefix-length)

(defun* azs-get-sources (arg &optional (chdir default-directory))
  (anything-aif (azs-get-hardcopy arg chdir)
      (with-temp-buffer
        (insert-file-contents-literally it)
        (azs-process-hardcopy (replace-regexp-in-string "[[:space:]]+$" "" arg)
                              chdir
                              anything-zsh-screen-zle-line-source-name
                              "^>"))))

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
                       " " (number-to-string anything-zsh-screen-run-awaits)
                       " 'stuff \"cd " chdir "^M\"' "
                       " 'stuff \"" arg "^I^X^X\"'"))))
    anything-zsh-screen-exchange-file-name
    (error "Error while communicating with the screen session")))

(defun azs-process-hardcopy (arg chdir zle-line-name start-regexp)
  (save-excursion
    (goto-char (point-min))
    (let* ((beg (re-search-forward start-regexp nil t))
           (end0 (or (re-search-forward "^$" nil t)
                     (progn
                       (goto-char (point-max))
                       (forward-line)
                       (point))))
           (end (progn (goto-char end0)
                       (insert "* dummy\n")
                       (point))))
      (goto-char beg)
      (forward-line)
      (azs-current-buffer-reduce
       #'(lambda (x desc buf cands acc)
           (cond ((string-match "^>[[:space:]]+\\(.+\\)" x)
                  (list desc (match-string-no-properties 1 x) cands acc))
                 ((string-match "^corrections (errors:" x)
                  (list x buf cands acc))
                 ((string-match "^\\*[[:space:]]+\\(.+\\)" x)
                  (let ((desc1 (match-string-no-properties 1 x))
                        (source (if (null cands)
                                  nil
                                  `(((name . ,desc)
                                     (candidates . ,cands))))))
                    (if (and (not (string-equal buf ""))
                             (not (string-equal buf arg)))
                      (list desc1 "" nil (nconc acc
                                                `(((name . ,zle-line-name)
                                                   (candidates . (,buf))))
                                                source))
                      (list desc1 buf nil (nconc acc source)))))
                 ((string-match " -- " x)
                  (list desc
                        buf
                        (nconc
                         cands
                         `((,x
                            . ,(if (string-equal "directory stack" desc)
                                 (car (last (split-string x " -- ")))
                                 (car (split-string x "[[:space:]]+"))))))
                        acc))
                 (t (if (string-equal desc "")
                      (list desc (concat buf x) cands acc)
                      (list desc
                            buf
                            (nconc cands 
                                   (mapcar #'(lambda (x) `(,x . ,x))
                                           (split-string x "[[:space:]]+")))
                            acc)))))
       '("" "" nil nil)
       #'(lambda (&rest acc) (car (reverse acc)))
       #'(lambda ()
           (= end (point)))))))

(defun* azs-current-buffer-reduce (f init &optional (final #'identity) (until #'eobp))
  (loop until (funcall until)
        for val = (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))
        for acc = (apply f val acc)
        initially (setq acc init)
        do (forward-line)
        finally (return (apply final acc))))

(defun azs-complete (sources target)
  (let ((anything-sources sources)
        (anything-execute-action-at-once-if-one t)
        (anything-zsh-screen-complete-target target))
    (anything-other-buffer sources "*anything zsh screen*")))

(defun azs-want-zsh-expander (target)
  (string-match "{" target))
(defun azs-complete-cancel-anything ()
  '(message "Sorry for the inconvenience")
  (when (eq major-mode 'term-mode)
    (term-send-raw-string "\C-I")))

(defun anything-zsh-screen-extend-sources (sources)
  (mapcar #'(lambda (x) (append x '((type . anything-zsh-screen))))
          sources))

(defun* anything-zsh-screen-complete (get-target &optional (extend-sources #'anything-zsh-screen-extend-sources))
  (let ((target (funcall get-target)))
    (if (azs-want-zsh-expander target)
      (azs-complete-cancel-anything)
      (azs-complete (funcall extend-sources (azs-get-sources target))
                    target))))

(defun anything-zsh-screen-simple-complete ()
  (interactive)
  (anything-zsh-screen-complete #'(lambda ()
                                    (buffer-substring-no-properties
                                     (comint-line-beginning-position)
                                     (point)))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "azs-process-hardcopy")
      (expect '(((name . "desc") (candidates ("--foo" . "--foo"))))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   '("> _"
                     "> ls -"
                     "* desc"
                     "--foo"
                     "")
                   "\n"))
          (azs-process-hardcopy "ls -" "" ">" "^>")))
      (expect '(((name . "name>") (candidates "foo foo")))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   '("> _"
                     "> foo foo"
                     "")
                   "\n"))
          (azs-process-hardcopy "ls " "" "name>" "^>")))
      (expect '(((name . ">")
                 (candidates "ls abc"))
                ((name . "corrections (errors: 1)")
                 (candidates ("abc" . "abc"))))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   '("> _"
                     "> ls abc"
                     "* corrections (errors: 1)"
                     "abc"
                     "")
                   "\n"))
          (azs-process-hardcopy "ls abcd" "" ">" "^>")))
      (expect '(((name . "directory stack")
                 (candidates ("1 -- /tmp" . "/tmp")
                             ("2 -- /tmp/tmp" . "/tmp/tmp"))))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   '("> _"
                     "> cd -"
                     "* directory stack"
                     "1 -- /tmp"
                     "2 -- /tmp/tmp"
                     "")
                   "\n"))
          (azs-process-hardcopy "cd -" "" ">" "^>")))
      (expect '(((name . "desc")
                 (candidates ("--option -- option and desc" . "--option"))))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   '("> _"
                     "> foo"
                     "* desc"
                     "--option -- option and desc"
                     "")
                   "\n"))
          (azs-process-hardcopy "foo" "" ">" "^>")))
      )))

(provide 'anything-zsh-screen)
;;; anything-zsh-screen.el ends here
