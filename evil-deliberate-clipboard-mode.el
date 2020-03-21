;; evil-deliberate-clipboard.el
;; This file contains all of the special binds I have that make killing things in
;; emacs behave better.
(require 'evil)

;;; Code:

(defvar-local evil-deliberate-clipboard//evil-del-advice-enabled t
  "Internal use only.
Buffer local variable that is set by
`evil-deliberate-clipboard-mode/without-evil-delete-advice'
to determine whether or not to load kills into the system
clipboard and first space in the kill ring.")

(defvar evil-deliberate-clipboard/mode-line-name " delib-clip"
  "The mode-line name for `evil-deliberate-clipboard-mode'.
Should start with a space")
(defvar evil-deliberate-clipboard/ignored-modes (list 'magit 'magit-diff-mode 'git-rebase-mode)
  "Modes that `evil-deliberate-clipboard-mode' will not bind in.
Must be changed before `evil-deliberate-clipboard-mode.el' is loaded.")

;; This advice does most of the heavy lifting for evil-deliberate-clipboard-mode.
(defun evil-deliberate-clipboard/evil-delete-advice (proc &rest arg-passthrough)
  "Advise evil functions that kill as a non-primary directive.
When `evil-deliberate-clipboard-mode' is on, this advice prevents
`kill-new' from accessing the system clipboard, and has it set the second
item in the `kill-ring' instead of the first. In the case that the
`kill-ring' is empty,"
  (cond
   ((or (not evil-deliberate-clipboard-mode) (not evil-deliberate-clipboard//evil-del-advice-enabled))
    ;; If the mode isn't on, or we should kill to the clipboard, just pass
    ;; through like nothing happened.
    (apply proc arg-passthrough))
   ((bound-and-true-p evil-mc-cursor-list)
    (let ((select-enable-clipboard nil))
      (apply proc arg-passthrough)))
   (kill-ring ;; Kill ring has something in it
    (let ((original-kill-ring kill-ring)
          (kill-ring (cdr kill-ring)) ;; Set "system kill ring"
          (select-enable-clipboard nil))
      (unwind-protect
          (apply proc arg-passthrough)
        (setcdr original-kill-ring kill-ring)
        (setq kill-ring-yank-pointer original-kill-ring))))
   ;;Handle empty kill advice
   (t
    (let ((select-enable-clipboard nil))
      (apply proc arg-passthrough)))))
(advice-add 'evil-delete :around #'evil-deliberate-clipboard/evil-delete-advice)

(defun evil-deliberate-clipboard/without-evil-delete-advice (proc &rest args)
  "Call PROC with ARGS and `evil-deliberate-clipboard/evil-delete-advice' disabled."
  (if (not evil-deliberate-clipboard-mode)
      (apply proc args)
    (setq evil-deliberate-clipboard//evil-del-advice-enabled nil)
    (unwind-protect
        (apply proc args)
      (setq evil-deliberate-clipboard//evil-del-advice-enabled t))))

(defun hetetic-evil-clipboard--add-ignore-hooks ()
  "Internal use: adds hooks for the modes that are ignored
by `evil-deliberate-clipboard-mode' as defined in
`evil-deliberate-clipboard/ignored-modes'"
  (mapcar
   (lambda (mode)
     (let ((mode-hook
            (intern (concat (symbol-name mode) "-mode-hook"))))
       (if mode-hook
           (add-hook mode-hook 'evil-deliberate-clipboard-mode-off)
         (error (concat "evil-deliberate-clip: mode hook does not exist: "
                        (symbol-name mode))))))
   evil-deliberate-clipboard/ignored-modes))

(defun evil-deliberate-clipboard--bind (key def)
  "Binds KEY to DEF with evil-define key
Alias for (`evil-define-key' (visual and normal)
                             `evil-deliberate-cilpboard-mode-map' KEY DEF)"
  (evil-define-minor-mode-key 'visual 'evil-deliberate-clipboard-mode key def)
  (evil-define-minor-mode-key 'normal 'evil-deliberate-clipboard-mode key def))

;;; functions
(evil-define-command evil-deliberate-clipboard-alt-paste-before
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
(evil-deliberate-clipboard--bind "P" #'evil-deliberate-clipboard-alt-paste-before)

(evil-define-operator evil-deliberate-clipboard-cut (beg end type register yank-handler)
  "Evil-delete that sets custom clipboard along with standard kill"
  (interactive "<R><x><y>")
  (evil-deliberate-clipboard/without-evil-delete-advice
   #'evil-delete beg end type register yank-handler))
(evil-deliberate-clipboard--bind "m" #'evil-deliberate-clipboard-cut)

(evil-define-operator evil-deliberate-clipboard-cut-eol (beg end type register yank-handler)
  "Evil-delete-line that sets custom clipboard along with standard kill"
  :motion evil-end-of-line
  (interactive "<R><x>")
  (evil-deliberate-clipboard/without-evil-delete-advice
   #'evil-delete-line beg end type register))
(evil-deliberate-clipboard--bind "M" #'evil-deliberate-clipboard-cut-eol)

(hetetic-evil-clipboard--add-ignore-hooks)
(define-minor-mode evil-deliberate-clipboard-mode
  "Makes evil mode do sane things with the default register and kill ring,
causing deletion commands to add to the second spot in the kill ring
so as to keep them accessible, and adds a dedicated deletion key
(m by default) that interacts with the top of the kill ring and system
clipboard.

This mode currently has built in support for `evil-cleverparens-mode',
but theoretically any minor mode could be added in a similar manner

To further motivate usage of your new and improved `kill-ring',
`evil-deliberate-clipboard-mode' provides heretic-evil-helm-kill-ring"
  :lighter evil-deliberate-clipboard/mode-line-name
  :keymap (make-sparse-keymap)
  ;; Keymap switching handled by evil mode
  :global nil)

;;;###autoload
(defun evil-deliberate-clipboard-mode-on ()
  "Turn on `evil-deliberate-clipboard-mode'."
  (interactive)
  (evil-deliberate-clipboard-mode t))


;;;###autoload
(define-globalized-minor-mode global-evil-deliberate-clipboard-mode
  evil-deliberate-clipboard-mode evil-deliberate-clipboard-mode-on)

;;;###autoload
(defun evil-deliberate-clipboard-mode-off ()
  "Turn off `evil-deliberate-clipboard-mode'"
  (interactive)
  (evil-deliberate-clipboard-mode -1))

;; evil helm kill ring
;; (when (boundp 'helm-mode)
;;   (require 'helm-ring)
;;   (defcustom evil-deliberate-helm-kill-ring--actions
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
;;             (evil-deliberate-clipboard-p 1)))))
;;       ("Paste before (override in visual)" .
;;        (lambda (_str)
;;          (let ((marked (helm-marked-candidates))
;;                (sep (if (equal helm-current-prefix-arg '(16))
;;                         (read-string "Separator: ")
;;                       helm-kill-ring-separator))
;;                kill-ring
;;                select-enable-clipboard
;;                interprogram-paste-function)
;;            ;; Mask off the old kill ring, and don't
;;            ;; paste anywhere
;;            (kill-new
;;             ;; Taken from `helm-kill-ring-action-yank'
;;             (cl-loop for c in (butlast marked)
;;                      concat (concat c sep) into str
;;                      finally return (concat str (car (last marked)))))
;;            (evil-deliberate-clipboard-P 1)))))
;;     "List of actions for heretic kill ring source"
;;     ;; From `helm-kill-ring-actions'
;;     :group 'helm-ring
;;     :type '(alist :key-type string :value-type function))
;;
;;   (defvar evil-deliberate-helm-kill-ring--source
;;     (helm-build-sync-source "Kill Ring"
;;       :init (lambda ()
;;               (helm-attrset 'last-command last-command)
;;               (helm-attrset 'multiline helm-kill-ring-max-offset))
;;       :candidates #'helm-kill-ring-candidates
;;       :filtered-candidate-transformer #'helm-kill-ring-transformer
;;       :action 'evil-deliberate-helm-kill-ring--actions
;;       :persistent-action 'ignore
;;       :help-message 'helm-kill-ring-help-message
;;       :persistent-help "DoNothing"
;;       :keymap helm-kill-ring-map
;;       :migemo t
;;       :multiline 'helm-kill-ring-max-offset
;;       :group 'helm-ring)
;;     "Helm source for `evil-deliberate-helm-kill-ring'")
;;
;;   (defun evil-deliberate-helm-kill-ring ()
;;     (interactive)
;;     (let ((enable-recursive-minibuffers t))
;;       (helm :sources evil-deliberate-helm-kill-ring--source
;;             :buffer "*helm evil kill ring"
;;             :resume 'noresume))))
;;
;; (provide 'evil-deliberate-clipboard-mode)
;;; evil-deliberate-clipboard-mode.el ends here
