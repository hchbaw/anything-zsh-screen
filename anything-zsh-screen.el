(require 'anything)

(defvar anything-zsh-screen.zsh
  (expand-file-name "~/.elisp/anything-zsh-screen.zsh"))
(defvar anything-zsh-screen-exchange-file-name
  (expand-file-name "~/tmp/anything-zsh-screen-exchange"))
(defvar anything-zsh-screen-target nil)

(defun aczs-get-hardcopy (arg)
  (when (= (call-process "zsh" nil nil nil
                         "-c"
                         (concat
                          "source " anything-zsh-screen.zsh ";"
                          "anything-zsh-screen-run \"" arg "\"")) 0)
    anything-zsh-screen-exchange-file-name))

(defvar anything-zsh-screen-source
  '((name . "zsh screen")
    (init
     . (lambda ()
         (with-current-buffer (anything-candidate-buffer 'local)
           (anything-aif (aczs-get-hardcopy anything-zsh-screen-target)
               (insert-file-contents it)))))
    (candidates-in-buffer)
    (action . identity)))

(defun anything-zsh-screen ()
  (interactive)
  (let ((anything-zsh-screen-target anything-pattern))
    (let ((anything-candidate-number-limit 9999))
      (anything-nest '(anything-zsh-screen-source)
                     nil nil nil nil "*anything zsh screen*"))))

(defun aczs-inits (list)
  (loop for i upto (length list)
        nconc (list (subseq list 0 i))))
(defun aczs-tails (list)
  (loop with size = (length list)
        for i upto size
        nconc (list (subseq list i size))))

(defvar anything-c-current-line-source
  '((name . "current line")
    (candidate
     . (lambda ()
         (with-current-buffer anything-current-buffer
           (let ((line0 (buffer-substring-no-properties (point-at-bol)
                                                        (point))))
             line0))))))
             
             
