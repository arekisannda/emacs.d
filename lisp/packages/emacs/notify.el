;;; emacs/project.el -*- lexical-binding: t; -*-

(use-package notifications
  :config
  (defun notifications-notify-override (&rest params)
    (with-demoted-errors "Notification error: %S"
      (let ((bus (or (plist-get params :bus) :session))
            (title (plist-get params :title))
            (body (plist-get params :body))
            (app-name (plist-get params :app-name))
            (replaces-id (plist-get params :replaces-id))
            (app-icon (plist-get params :app-icon))
            (actions (plist-get params :actions))
            (timeout (plist-get params :timeout))
            ;; Hints
            (hints '())
            (urgency (plist-get params :urgency))
            (category (plist-get params :category))
            (desktop-entry (plist-get params :desktop-entry))
            (image-data (plist-get params :image-data))
            (image-path (plist-get params :image-path))
            (action-items (plist-get params :action-items))
            (sound-file (plist-get params :sound-file))
            (sound-name (plist-get params :sound-name))
            (suppress-sound (plist-get params :suppress-sound))
            (resident (plist-get params :resident))
            (transient (plist-get params :transient))
            (x (plist-get params :x))
            (y (plist-get params :y))
            (extra-hints (plist-get params :hints))
            id)
        ;; Build hints array
        (when urgency
          (push `(:dict-entry
                  "urgency"
                  (:variant :byte ,(pcase urgency
                                     ('low 0)
                                     ('critical 2)
                                     (_ 1))))
                hints))
        (when category
          (push `(:dict-entry
                  "category"
                  (:variant :string ,category))
                hints))
        (when desktop-entry
          (push `(:dict-entry
                  "desktop-entry"
                  (:variant :string ,desktop-entry))
                hints))
        (when image-data
          (push `(:dict-entry
                  "image-data"
                  (:variant :struct ,image-data))
                hints))
        (when image-path
          (push `(:dict-entry
                  "image-path"
                  (:variant :string ,image-path))
                hints))
        (when action-items
          (push `(:dict-entry
                  "action-items"
                  (:variant :boolean ,action-items))
                hints))
        (when sound-file
          (push `(:dict-entry
                  "sound-file"
                  (:variant :string ,sound-file))
                hints))
        (when sound-name
          (push `(:dict-entry
                  "sound-name"
                  (:variant :string ,sound-name))
                hints))
        (when suppress-sound
          (push `(:dict-entry
                  "suppress-sound"
                  (:variant :boolean ,suppress-sound))
                hints))
        (when resident
          (push `(:dict-entry
                  "resident"
                  (:variant :boolean ,resident))
                hints))
        (when transient
          (push `(:dict-entry
                  "transient"
                  (:variant :boolean ,transient))
                hints))
        (when x
          (push `(:dict-entry "x" (:variant :int32 ,x)) hints))
        (when y
          (push `(:dict-entry "y" (:variant :int32 ,y)) hints))

        (dolist (h extra-hints)
          (pcase-let ((`(,name ,type ,value) h))
            (push `(:dict-entry ,name (:variant ,type ,value)) hints)))

        (setq hints (nreverse hints))

        ;; Call Notify method.
        (setq id
              (dbus-call-method bus
                                notifications-service
                                notifications-path
                                notifications-interface
                                notifications-notify-method
                                :string (or app-name
                                            notifications-application-name)
                                :uint32 (or replaces-id 0)
                                :string (if app-icon
                                            (if (stringp app-icon)
                                                (expand-file-name app-icon)
                                              ;; Convert symbol to string
                                              (symbol-name app-icon))
                                          ;; If app-icon is nil because user
                                          ;; requested it to be so, send the
                                          ;; empty string
                                          (if (plist-member params :app-icon)
                                              ""
                                            ;; Otherwise send the
                                            ;; default icon path
                                            notifications-application-icon))
                                :string (or title "")
                                :string (or body "")
                                `(:array ,@actions)
                                (or hints '(:array :signature "{sv}"))
                                :int32 (or timeout -1)))

        ;; Register close/action callback function.  We must also
        ;; remember the daemon's unique name, because the daemon could
        ;; have restarted.
        (let ((on-action (plist-get params :on-action))
              (on-close (plist-get params :on-close))
              (unique-name (dbus-get-name-owner bus notifications-service)))
          (when on-action
            (push (list (list bus unique-name id) on-action)
                  notifications-on-action-map)
            (unless notifications-on-action-object
              (setq notifications-on-action-object
                    (dbus-register-signal
                     bus
                     nil
                     notifications-path
                     notifications-interface
                     notifications-action-signal
                     'notifications-on-action-signal))))

          (when on-close
            (push (list (list bus unique-name id) on-close)
                  notifications-on-close-map)
            (unless notifications-on-close-object
              (setq notifications-on-close-object
                    (dbus-register-signal
                     bus
                     nil
                     notifications-path
                     notifications-interface
                     notifications-closed-signal
                     'notifications-on-closed-signal)))))

        ;; Return notification id
        id)))

  (advice-add #'notifications-notify :override #'notifications-notify-override))
