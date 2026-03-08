;;; ui/activites.el -*- lexical-binding: t; -*-

(defcustom +activities-save-all-skip '()
  "List of functions to skip `activities-save-all'."
  :type '(set (function :tag "functions")))

(defcustom +activities-default-directory nil
  "Default directory for `activities'."
  :type 'directory)

(use-package activities :after project
  :custom
  (activities-name-prefix "@")
  (activities-always-persist t)
  (activities-anti-save-predicates
   '(active-minibuffer-window
     activities--backtrace-visible-p))

  (activities-window-persistent-parameters
   (list (cons 'header-line-format 'writable)
         (cons 'mode-line-format 'writable)
         (cons 'tab-line-format 'writable)
         (cons 'no-other-window 'writable)
         (cons 'no-delete-other-windows 'writable)
         (cons 'window-preserved-size 'writable)
         (cons 'window-side 'writable)
         (cons 'window-slot 'writable)
         (cons 'window-popup 'writable)
         (cons 'window-purpose 'writable)))

  (activities-mode-idle-frequency (if init-file-debug most-positive-fixnum 30))
  (+activities-save-all-skip
   '((lambda() (when (fboundp 'treemacs-is-treemacs-window-selected?) (treemacs-is-treemacs-window-selected?)))))
  (activities-bookmark-store nil)
  :init
  (when init-file-debug
    (advice-add #'activities-save-all :override #'ignore)
    (advice-add #'activities-save :override #'ignore)
    (advice-add #'+activities-save-all-around :override #'ignore)
    (advice-add #'activities-save-activity-workspaces :override #'ignore))

  (unless init-file-debug
    (defun +activities-save-all-around (fn &rest args)
      (unless (run-hook-with-args-until-success '+activities-save-all-skip)
        (apply fn args)))

    (advice-add #'activities-save-all :around #'+activities-save-all-around))

  (defun +activities-suspend-eglot (activity)
    (activities-with activity
      (let* ((project-name (activities--project-name)))
        (+eglot--shutdown-project project-name))))

  (advice-add #'activities-suspend :before #'+activities-suspend-eglot)

  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  (defun +activities-new-project ()
    "Create new activity with project."
    (interactive)
    (tab-bar-new-tab)
    (dashboard-open)
    (condition-case err
        (let ((default-directory +activities-default-directory)
              (activity nil))
          (call-interactively #'project-switch-project)
          (tab-line-close-other-tabs)
          (setq activity (call-interactively #'activities-define))
          (treemacs--init)
          (setq activity (activities-define (activities-activity-name activity) :forcep t))
          (activities-revert activity))
      ((error quit)
       (tab-bar-close-tab))))

  (defun +activities-discard-override (activity)
    "Discard ACTIVITY and its state.
It will not be recoverable."
    (interactive
     (list (activities-completing-read :prompt "Discard activity")))
    (when (yes-or-no-p (format "Discard activity %S permanently?" (activities-activity-name activity)))
      (ignore-errors
        (when (activities-activity-active-p activity)
          (activities-close activity))
        )
      (setf activities-activities (map-delete activities-activities (activities-activity-name activity)))
      (let* ((workspace-name (treemacs-scope->current-scope-name
                              (treemacs-current-scope-type) (activities-name-for activity))))
        (treemacs-do-remove-workspace workspace-name nil))
      ))

  (advice-add #'activities-discard :override #'+activities-discard-override)
  :hook
  (after-init      . activities-mode)
  (activities-mode . activities-tabs-mode))

(with-eval-after-load 'activities
  (cl-defstruct activities-workspace-state
    "Workspace state."
    (window-state :documentation "Window state `window-state-get'."))

  (cl-defstruct activities-workspaces
    "Workspace container for `activities-activities'."
    (name nil :documention "Activity name.")
    (last nil :documentation "Last workspace used.")
    (workspaces nil :documentation "List of workspace states for activity."))

  (with-demoted-errors "Variable `activities-activity-workspaces' failed to load persisted data: %S"
    (persist-defvar activities-activity-workspaces nil "Additional window states defined for `activities-activity'."))

  (cl-defun activities-current-workspace ()
    (map-elt activities-activity-workspaces (activities-activity-name (activities-current))))

  (cl-defun activities-workspaces-completing-read (activity)
    (mapcar #'car (activities-workspaces-workspaces (map-elt activities-activity-workspaces activity))))

  (cl-defun activities--save-activity-workspace (workspace name frame)
    (when name
      (setf (map-elt (activities-workspaces-workspaces workspace) name)
            (activities--window-state frame))
      (persist-save 'activities-activity-workspaces)))

  (cl-defun activities--ensure-activity-workspace (activity)
    (unless (map-elt activities-activity-workspaces activity)
      (setf (map-elt activities-activity-workspaces activity) (make-activities-workspaces :name activity))))

  (cl-defun activities--define-activity-workspace (workspace name frame)
    (activities--save-activity-workspace workspace name frame)
    (setf (activities-workspaces-last workspace) name))

  (defmacro with-current-activity (&rest body)
    `(if-let* ((activities-current (activities-current))
               (activity (activities-activity-name activities-current)))
         ,@body
       (user-error "Not in activity.")))

  (cl-defun activities-quit-activity-workspace (workspace)
    (interactive
     (with-current-activity
      (list (map-elt activities-activity-workspaces activity))))

    (when (yes-or-no-p (format "Revert activity and workspace?"))
      (let* ((last (activities-workspaces-last workspace)))
        (when last
          (activities--save-activity-workspace workspace last (selected-frame))
          (setf (activities-workspaces-last workspace) nil)
          (call-interactively #'activities-revert))
        (activities-save (activities-current)))))

  (cl-defun activities-remove-activity-workspace (name activity)
    (interactive
     (with-current-activity
      (let* ((default (activities-workspaces-last (or (map-elt activities-activity-workspaces activity)
                                                      (activities--ensure-activity-workspace activity)))))
        (list (completing-read "Discard workspace: " (activities-workspaces-completing-read activity) nil t) activity))))

    (when (yes-or-no-p (format "Discard workspace %S permanently?" name))
      (let* ((activity-workspaces (map-elt activities-activity-workspaces activity))
             (last (activities-workspaces-last activity-workspaces)))
        (setf (activities-workspaces-workspaces activity-workspaces)
              (map-delete (activities-workspaces-workspaces activity-workspaces) name))
        (when (string= last name)
          (setf (activities-workspaces-last activity-workspaces) nil)
          (call-interactively #'activities-revert))
        (activities-save (activities-current)))))

  (cl-defun activities-switch-activity-workspace (name activity &key ignorep)
    (interactive
     (with-current-activity
      (let* ((default (activities-workspaces-last (or (map-elt activities-activity-workspaces activity)
                                                      (activities--ensure-activity-workspace activity)))))
        (list (completing-read "Switch workspace: " (activities-workspaces-completing-read activity) nil t) activity
              :ignorep current-prefix-arg))))

    (when (or (string= name "") (null name)) (user-error "Invalid workspace name"))

    (let* ((activity-workspaces (map-elt activities-activity-workspaces activity))
           (last (activities-workspaces-last activity-workspaces)))
      (unless (string= last name)
        (unless ignorep
          (activities--save-activity-workspace activity-workspaces last (selected-frame)))
        (if (null (map-elt (activities-workspaces-workspaces activity-workspaces) name))
            (activities--define-activity-workspace activity-workspaces name (selected-frame))
          (activities--windows-set (map-elt (activities-workspaces-workspaces activity-workspaces) name))
          (setf (activities-workspaces-last activity-workspaces) name)))
      (activities-save (activities-current))))

  (cl-defun activities-define-activity-workspace (name activity &key ignorep)
    (interactive
     (with-current-activity
      (let* ((default (activities-workspaces-last (or (map-elt activities-activity-workspaces activity)
                                                      (activities--ensure-activity-workspace activity)))))
        (list (read-string (format-prompt "Define workspace" default) nil nil default) activity
              :ignorep current-prefix-arg))))

    (when (or (string= name "") (null name)) (user-error "Invalid workspace name"))

    (let* ((activity-workspaces (map-elt activities-activity-workspaces activity))
           (last (activities-workspaces-last activity-workspaces)))
      (unless ignorep
        (activities--save-activity-workspace activity-workspaces last (selected-frame)))
      (unless (string= name last)
        (activities--define-activity-workspace activity-workspaces name (selected-frame)))
      (activities-save (activities-current))))

  (cl-defun activities-save-activity-workspaces (activity &key defaultp lastp persistp)
    "Save workspace states of ACTIVITY."
    (activities-with activity
      (let* ((name (activities-activity-name activity)))
        (activities--ensure-activity-workspace name)
        (when-let ((workspace (map-elt activities-activity-workspaces name))
                   (last (activities-workspaces-last workspace)))
          (activities--save-activity-workspace workspace last (selected-frame))))
      ))

  (advice-add #'activities-save :after #'activities-save-activity-workspaces))
