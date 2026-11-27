;;; insert-ts.el --- Insert timestamps in layouts via transient  -*- lexical-binding: t; -*-

;; Requires: ts (https://github.com/alphapapa/ts.el), transient (built in since Emacs 28)

;;; Commentary:
;;
;; M-x +insert-timestamp  opens a transient menu.  Each entry shows a live
;; preview of the current time in that layout; press its key to insert it.
;; The inserted timestamp is always `ts-now' at the moment the key is pressed.
;;
;; Options in the menu:
;;   -u  toggle UTC (default: local time zone)
;;   -p  toggle prompting for the time to insert (default: now)

;;; Code:

(require 'ts)
(require 'transient)
(require 'subr-x)

;;;; Options

(defvar insert-ts-utc nil
  "When non-nil, format timestamps in UTC instead of local time.")

(defvar insert-ts-prompt nil
  "When non-nil, prompt for the time to insert instead of using now.")

;;;; Formatting helpers

(defun insert-ts--fmt (fmt ts &optional zone)
  "Format TS with `format-time-string' FMT.
ZONE overrides the zone chosen by `insert-ts-utc'.  Uses the C locale so
day/month names are always English."
  (let ((system-time-locale "C"))
    (format-time-string fmt (ts-unix ts) (or zone (and insert-ts-utc t)))))

(defun insert-ts--iso-zone (ts)
  "Return \"Z\" for UTC, else the offset as \"+07:00\"."
  (if (equal (insert-ts--fmt "%z" ts) "+0000")
      "Z"
    (insert-ts--fmt "%:z" ts)))

(defun insert-ts--frac-trim (ts)
  "Nanosecond fraction with trailing zeros removed.
Returns \"\" when the fraction is zero."
  (let ((digits (string-trim-right (insert-ts--fmt "%N" ts) "0+")))
    (if (string-empty-p digits) "" (concat "." digits))))

(defconst insert-ts-layouts
  `((unix        "Unix epoch"   "%s")
    (ansic       "ANSIC"        "%a %b %e %H:%M:%S %Y")
    (unix-date   "UnixDate"     "%a %b %e %H:%M:%S %Z %Y")
    (ruby-date   "RubyDate"     "%a %b %d %H:%M:%S %z %Y")
    (rfc822      "RFC822"       "%d %b %y %H:%M %Z")
    (rfc822z     "RFC822Z"      "%d %b %y %H:%M %z")
    (rfc850      "RFC850"       "%A, %d-%b-%y %H:%M:%S %Z")
    (rfc1123     "RFC1123"      "%a, %d %b %Y %H:%M:%S %Z")
    (rfc1123z    "RFC1123Z"     "%a, %d %b %Y %H:%M:%S %z")
    (rfc3339     "RFC3339"
                 ,(lambda (ts) (concat (insert-ts--fmt "%Y-%m-%dT%H:%M:%S" ts)
                                       (insert-ts--iso-zone ts))))
    (rfc3339nano "RFC3339Nano"
                 ,(lambda (ts) (concat (insert-ts--fmt "%Y-%m-%dT%H:%M:%S" ts)
                                       (insert-ts--frac-trim ts)
                                       (insert-ts--iso-zone ts))))
    ;; Always UTC, because of the literal "Z" suffix.
    (iso8601     "ISO8601"
                 ,(lambda (ts) (insert-ts--fmt "%Y%m%dT%H%M%SZ" ts t)))
    (kitchen     "Kitchen"      "%-I:%M%p")
    (stamp       "Stamp"        "%b %e %H:%M:%S")
    (stamp-milli "StampMilli"   "%b %e %H:%M:%S.%3N")
    (stamp-micro "StampMicro"   "%b %e %H:%M:%S.%6N")
    (stamp-nano  "StampNano"    "%b %e %H:%M:%S.%N")
    (date-time   "DateTime"     "%Y-%m-%d %H:%M:%S")
    (date-only   "DateOnly"     "%Y-%m-%d")
    (time-only   "TimeOnly"     "%H:%M:%S"))
  "Supported timestamp layouts.")

(defun insert-ts--read-time ()
  "Prompt for a date/time and return it as a ts struct."
  (require 'org)
  (make-ts :unix (float-time (org-read-date t t nil "Time: "))))

(defun insert-ts-format-layout (id &optional ts)
  "Return TS (default: now) formatted with layout ID from `insert-ts-layouts'."
  (let* ((ts (or ts (ts-now)))
         (spec (nth 2 (or (assq id insert-ts-layouts)
                          (user-error "Unknown timestamp layout: %s" id)))))
    (if (functionp spec)
        (funcall spec ts)
      (insert-ts--fmt spec ts))))

;;;; Commands

(defun insert-ts-insert (id)
  "Insert a timestamp in layout ID at point.
Uses the current time, or a prompted time when `insert-ts-prompt' is set."
  (insert (insert-ts-format-layout
           id (and insert-ts-prompt (insert-ts--read-time)))))

(defun insert-ts-unix ()
  "Insert the current Unix timestamp."
  (interactive)
  (insert-ts-insert 'unix))

;;;; Transient menu

(defun insert-ts--describe (id)
  "Menu label for layout ID: its name plus a live preview."
  (format "%-12s %s"
          (nth 1 (assq id insert-ts-layouts))
          (propertize (insert-ts-format-layout id) 'face 'transient-value)))

(defun insert-ts--describe-toggle (label on)
  (format "%s %s" label
          (propertize (if on "[on]" "[off]")
                      'face (if on 'transient-value 'transient-inactive-value))))

(transient-define-suffix insert-ts-toggle-utc ()
  :transient t
  :description (lambda () (insert-ts--describe-toggle "UTC" insert-ts-utc))
  (interactive)
  (setq insert-ts-utc (not insert-ts-utc)))

(transient-define-suffix insert-ts-toggle-prompt ()
  :transient t
  :description (lambda () (insert-ts--describe-toggle "Prompt for time" insert-ts-prompt))
  (interactive)
  (setq insert-ts-prompt (not insert-ts-prompt)))

;;;###autoload (autoload '+insert-timestamp "insert-timestamp" nil t)
(transient-define-prefix insert-ts ()
  "Insert a timestamp."
  ["Options"
   ("-u" insert-ts-toggle-utc)
   ("-p" insert-ts-toggle-prompt)]
  [["Unix"
    ("u" (lambda () (interactive) (insert-ts-insert 'unix))
     :description (lambda () (insert-ts--describe 'unix)))
    ("a" (lambda () (interactive) (insert-ts-insert 'ansic))
     :description (lambda () (insert-ts--describe 'ansic)))
    ("x" (lambda () (interactive) (insert-ts-insert 'unix-date))
     :description (lambda () (insert-ts--describe 'unix-date)))
    ("r" (lambda () (interactive) (insert-ts-insert 'ruby-date))
     :description (lambda () (insert-ts--describe 'ruby-date)))
    ("k" (lambda () (interactive) (insert-ts-insert 'kitchen))
     :description (lambda () (insert-ts--describe 'kitchen)))]
   ["RFC / ISO"
    ("2" (lambda () (interactive) (insert-ts-insert 'rfc822))
     :description (lambda () (insert-ts--describe 'rfc822)))
    ("@" (lambda () (interactive) (insert-ts-insert 'rfc822z))
     :description (lambda () (insert-ts--describe 'rfc822z)))
    ("8" (lambda () (interactive) (insert-ts-insert 'rfc850))
     :description (lambda () (insert-ts--describe 'rfc850)))
    ("1" (lambda () (interactive) (insert-ts-insert 'rfc1123))
     :description (lambda () (insert-ts--describe 'rfc1123)))
    ("!" (lambda () (interactive) (insert-ts-insert 'rfc1123z))
     :description (lambda () (insert-ts--describe 'rfc1123z)))
    ("3" (lambda () (interactive) (insert-ts-insert 'rfc3339))
     :description (lambda () (insert-ts--describe 'rfc3339)))
    ("#" (lambda () (interactive) (insert-ts-insert 'rfc3339nano))
     :description (lambda () (insert-ts--describe 'rfc3339nano)))
    ("i" (lambda () (interactive) (insert-ts-insert 'iso8601))
     :description (lambda () (insert-ts--describe 'iso8601)))]]
  [["Stamp"
    ("s" (lambda () (interactive) (insert-ts-insert 'stamp))
     :description (lambda () (insert-ts--describe 'stamp)))
    ("m" (lambda () (interactive) (insert-ts-insert 'stamp-milli))
     :description (lambda () (insert-ts--describe 'stamp-milli)))
    ("M" (lambda () (interactive) (insert-ts-insert 'stamp-micro))
     :description (lambda () (insert-ts--describe 'stamp-micro)))
    ("n" (lambda () (interactive) (insert-ts-insert 'stamp-nano))
     :description (lambda () (insert-ts--describe 'stamp-nano)))]
   ["Date / time"
    ("d" (lambda () (interactive) (insert-ts-insert 'date-time))
     :description (lambda () (insert-ts--describe 'date-time)))
    ("D" (lambda () (interactive) (insert-ts-insert 'date-only))
     :description (lambda () (insert-ts--describe 'date-only)))
    ("t" (lambda () (interactive) (insert-ts-insert 'time-only))
     :description (lambda () (insert-ts--describe 'time-only)))]])

(provide 'insert-ts)
;;; insert-ts.el ends here
