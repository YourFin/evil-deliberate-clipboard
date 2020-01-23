;; heretic-evil-clipboard.el
;; This file contains all of the special binds I have that make killing things in
;; emacs behave better.
(require 'evil)

(defvar heretic-evil-clipboard/mode-line-name " heretic-clip"
  "The mode-line name for `heretic-evil-clipboard-mode'.
Should start with a space")
(defvar heretic-evil-clipboard/ignored-modes (list 'magit 'magit-diff-mode 'git-rebase-mode)
  "Modes that `heretic-evil-clipboard-mode' will not bind in.
Must be changed before `heretic-evil-clipboard-mode.el' is loaded.")

;; This advice does most of the heavy lifting for heretic-evil-clipboard-mode.
(defun heretic-evil-clipboard/evil-delete-advice (proc &rest arg-passthrough)
  "Advice evil functions that kill as a non-primary directive"
  (cond
   ((bound-and-true-p evil-mc-cursor-list)
    (let ((interprogram-cut-function nil))
      (apply proc arg-passthrough)))
   (kill-ring ;; Kill ring has something in it
    (let ((real-kill-ring kill-ring)
          (kill-ring (cdr kill-ring))
          (interprogram-cut-function nil))
      (apply proc arg-passthrough)
      (setcdr real-kill-ring kill-ring)
      (setq kill-ring-yank-pointer real-kill-ring)))
   ;;Handle empty kill advice
   (t
    (let ((interprogram-cut-function nil))
      (apply proc arg-passthrough)))))

(defun heretic-evil-clipboard/without-evil-delete-advice (proc &rest args)
  "Call PROC with ARGS and `heretic-evil-clipboard/evil-delete-advice' disabled."
  (if (not heretic-evil-clipboard-mode)
      (apply proc args)
    (advice-remove 'evil-delete #'heretic-evil-clipboard/evil-delete-advice)
    (unwind-protect
        (apply proc args)
      (advice-add 'evil-delete :around #'heretic-evil-clipboard/evil-delete-advice))))

(defun hetetic-evil-clipboard--add-ignore-hooks ()
  "Internal use: adds hooks for the modes that are ignored
by `heretic-evil-clipboard-mode' as defined in `heretic-evil-clipboard/ignored-modes'"
  (mapcar
   (lambda (mode)
     (let ((mode-hook
            (intern (concat (symbol-name mode) "-mode-hook"))))
       (if mode-hook
           (add-hook mode-hook 'heretic-evil-clipboard-mode-off)
         (error (concat "heretic-evil-clip: mode hook does not exist: "
                        (symbol-name mode))))))
   heretic-evil-clipboard/ignored-modes))

(defun heretic-evil-clipboard--bind (key def)
  "Binds KEY to DEF with evil-define key
Alias for (`evil-define-key' (visual and normal)
                             `heretic-evil-cilpboard-mode-map' KEY DEF)"
  (evil-define-minor-mode-key 'visual 'heretic-evil-clipboard-mode key def)
  (evil-define-minor-mode-key 'normal 'heretic-evil-clipboard-mode key def))

(defun heretic-evil-clipboard- ()
  "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents.

Idea ripped from doom emacs: https://github.com/hlissner/doom-emacs"
  (interactive)
  (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
    (call-interactively #'evil-paste-after)))

;;; functions
(evil-define-command heretic-evil-clipboard-alt-paste-before
  (count &optional register yank-handler)
  "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents. To swap this so that `evil-paste-before' pastes without
replacing the clipboard's contents, you add the following to your init file:

`(setq evil-kill-on-visual-paste nil)'

Idea ripped from doom emacs: https://github.com/hlissner/doom-emacs"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
        (call-interactively #'evil-paste-after register yank-handler))
    (evil-paste-before count register yank-handler)))
(heretic-evil-clipboard--bind "P" #'heretic-evil-clipboard-alt-paste-before)

(evil-define-operator heretic-evil-clipboard-m (beg end type register yank-handler)
  "Evil-delete that sets custom clipboard along with standard kill"
  (interactive "<R><x><y>")
  (heretic-evil-clipboard/without-evil-delete-advice
   #'evil-delete beg end type register yank-handler))
(heretic-evil-clipboard--bind "m" #'heretic-evil-clipboard-m)

(evil-define-operator heretic-evil-clipboard-M (beg end type register yank-handler)
  "Evil-delete-line that sets custom clipboard along with standard kill"
  :motion evil-end-of-line
  (interactive "<R><x>")
  (heretic-evil-clipboard/without-evil-delete-advice
   #'evil-delete-line beg end type register))
(heretic-evil-clipboard--bind "M" #'heretic-evil-clipboard-M)

(hetetic-evil-clipboard--add-ignore-hooks)
(define-minor-mode heretic-evil-clipboard-mode
  "Makes evil mode do sane things with the default register and kill ring,
causing deletion commands to add to the second spot in the kill ring
so as to keep them accessible, and adds a dedicated deletion key
(m by default) that interacts with the top of the kill ring and system
clipboard.

This mode currently has built in support for `evil-cleverparens-mode',
but theoretically any minor mode could be added in a similar manner

To further motivate usage of your new and improved `kill-ring',
`heretic-evil-clipboard-mode' provides heretic-evil-helm-kill-ring"
  :lighter heretic-evil-clipboard/mode-line-name
  :keymap (make-sparse-keymap)
  ;; Keymap switching handled by evil mode
  :global nil
  ;;(hetetic-evil-clipboard--add-ignore-hooks)
  (if heretic-evil-clipboard-mode
      (progn
        (advice-add 'evil-visual-paste :around #'heretic-evil-clipboard/without-evil-delete-advice)
        (advice-add 'evil-delete :around #'heretic-evil-clipboard/evil-delete-advice))
    (advice-remove 'evil-visual-paste #'heretic-evil-clipboard/without-evil-delete-advice)
    (advice-remove 'evil-delete #'heretic-evil-clipboard/evil-delete-advice)))

;;;###autoload
(defun heretic-evil-clipboard-mode-on ()
  "Turn on `heretic-evil-clipboard-mode'."
  (interactive)
  (heretic-evil-clipboard-mode t))


;;;###autoload
(define-globalized-minor-mode global-heretic-evil-clipboard-mode
  heretic-evil-clipboard-mode  heretic-evil-clipboard-mode-on)

;;;###autoload
(defun heretic-evil-clipboard-mode-off ()
  "Turn off `heretic-evil-clipboard-mode'"
  (interactive)
  (heretic-evil-clipboard-mode -1))

;; evil helm kill ring
;; (when (boundp 'helm-mode)
;;   (require 'helm-ring)
;;   (defcustom heretic-evil-helm-kill-ring--actions
;;     '(("Paste after (no override in visual)" .
;;        (lambda (_str)
;;          (let ((marked (helm-marked-candidates))
;;                (sep (if (equal helm-current-prefix-arg '(16))
;;                         (read-string "Separator: ")
;;                       helm-kill-ring-separator))
;;                (old-kill-ring kill-ring)
;;                (to-paste
;;                 ;; Taken from `helm-kill-ring-action-yank'
;;                 (cl-loop for c in (butlast marked)
;;                          concat (concat c sep) into str
;;                          finally return (concat str (car (last marked))))))
;;            ;; Mask off the old kill ring, and don't
;;            ;; paste anywhere
;;            (kill-new
;;             (heretic-evil-clipboard-p 1)))))
;;       ("Paste before (override in visual)" .
;;        (lambda (_str)
;;          (let ((marked (helm-marked-candidates))
;;                (sep (if (equal helm-current-prefix-arg '(16))
;;                         (read-string "Separator: ")
;;                       helm-kill-ring-separator))
;;                kill-ring
;;                interprogram-cut-function
;;                interprogram-paste-function)
;;            ;; Mask off the old kill ring, and don't
;;            ;; paste anywhere
;;            (kill-new
;;             ;; Taken from `helm-kill-ring-action-yank'
;;             (cl-loop for c in (butlast marked)
;;                      concat (concat c sep) into str
;;                      finally return (concat str (car (last marked)))))
;;            (heretic-evil-clipboard-P 1)))))
;;     "List of actions for heretic kill ring source"
;;     ;; From `helm-kill-ring-actions'
;;     :group 'helm-ring
;;     :type '(alist :key-type string :value-type function))
;;
;;   (defvar heretic-evil-helm-kill-ring--source
;;     (helm-build-sync-source "Kill Ring"
;;       :init (lambda ()
;;               (helm-attrset 'last-command last-command)
;;               (helm-attrset 'multiline helm-kill-ring-max-offset))
;;       :candidates #'helm-kill-ring-candidates
;;       :filtered-candidate-transformer #'helm-kill-ring-transformer
;;       :action 'heretic-evil-helm-kill-ring--actions
;;       :persistent-action 'ignore
;;       :help-message 'helm-kill-ring-help-message
;;       :persistent-help "DoNothing"
;;       :keymap helm-kill-ring-map
;;       :migemo t
;;       :multiline 'helm-kill-ring-max-offset
;;       :group 'helm-ring)
;;     "Helm source for `heretic-evil-helm-kill-ring'")
;;
;;   (defun heretic-evil-helm-kill-ring ()
;;     (interactive)
;;     (let ((enable-recursive-minibuffers t))
;;       (helm :sources heretic-evil-helm-kill-ring--source
;;             :buffer "*helm evil kill ring"
;;             :resume 'noresume))))
;;
;; (provide 'heretic-evil-clipboard-mode)
;;; heretic-evil-clipboard-mode.el ends here
