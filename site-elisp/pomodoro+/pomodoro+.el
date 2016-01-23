;;; pomodoro+.el --- Pomodoro Technique for Emacs

;; Author: Ivan Kanis, Distopico Vegan <distopico@riseup.net>
;; Maintainer: Distopico Vegan <distopico@riseup.net>
;; Keywords: pomodoro, time
;; Version: 1.1

;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This package is a fork of pomodoro.el created by Ivan Kanis
;; The technique is described in http://www.pomodorotechnique.com
;;
;; to start the pomodoro you issue the following command:
;;
;; M-x pomodoro
;;
;; in the modeline you will see the following indicator Work~1/25min. This
;; means that you are working on set 1 and that you have 25 minutes
;; remaining. The counter will decrease each minutes. When it reaches
;; 0 you will get a message in a buffer that it's time to take a
;; break. The modeline will display Short Brak~1/5min, that is you have a break of
;; 5 minutes. When the count reaches 0 you will another message to get
;; back to work and the set number will increase. At the end of the
;; 4th set you will get a long break. The modeline will display Long Break~
;;
;; When you don't need the pomodoro anymore you do:
;;
;; M-x pomodoro-stop
;;
;; I you got interrupted and you want to rewind the pomodoro on the
;; current set just do:
;;
;; M-x pomodoro-rewind
;;
;; Calling M-x pomodoro again will reset it to the first working set
;;

;;; THANKS:

;; Ivan Kanis for created the initial code
;; Obviously Francesco Cirillo for creating the technique. I was
;; inspired by a pomodoro timer for Windows but I can't find out who
;; wrote it...
;; Richard Riley for pointing out I forgot provide
;; Manan Tuli for fixing the modeline and adding a hook

;;; BUGS:

;; Are you kidding me? This software is perfect ;)

;;; INSTALLATION:

;; (require 'pomodoro+)
;;

;;; Code:

(require 'org)
(require 'org-clock)
(eval-when-compile
  (require 'cl))

;;; Custom Variables
(defgroup pomodoro nil
  "Org pomodoro customization."
  :tag "Pomodoro"
  :group 'tools)

(defcustom pomodoro-work-time 25
  "Time in minutes of work."
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-short-break 5
  "Time in minute of short break."
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-long-break 15
  "Time in minute of long break."
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-set-number 4
  "Number of sets until a long break."
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-buffer-name "*pomodoro*"
  "Name of the pomodoro buffer."
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-notify 'buffer
  "Notification when start braek Options: buffer, message."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-raise-frame nil
  "Ir pomodoro-notify is buffer when t raise frame on pomodoro notification."
  :group 'pomodoro
  :type 'boolean)

(defcustom pomodoro-time-format "%.2m:%.2s"
  "Defines the format of the time representation in the modeline."
  :group 'org-pomodoro
  :type 'string)

(defcustom pomodoro-work-format "Work~%d/%dmin"
  "String displayed in the modeline when working."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-break-format "Short Break~%d/%dmin"
  "String displayed in the modeline for a break."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-long-break-format "Long Break~%d"
  "String displayed in the modeline for a long break."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-short-start-msg "Break time!"
  "String displayed in notification work start."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-long-start-msg "Time for a longer break!"
  "String displayed in notification work start."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-short-end-msg "Short break finished, Back to work!"
  "String displayed in notification work start."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-long-end-msg "Long break finished, Back to work!"
  "String displayed in notification work start."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-play-sounds t
  "Determines whether sounds are played or not."
  :group 'pomodoro
  :type 'boolean)

(defcustom pomodoro-audio-player (or (executable-find "aplay")
                                     (executable-find "afplay"))
  "Music player used to play sounds."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-sound (when load-file-name
                            (concat (file-name-directory load-file-name) "resources/tick.wav"))
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'pomodoro
  :type 'file)

(defcustom pomodoro-sound-args nil
  "The volume for the pomodoro sound."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-killed-sound nil
  "The path to a sound file, that´s to be played when a pomodoro is killed."
  :group 'pomodoro
  :type 'file)

(defcustom pomodoro-killed-sound-args nil
  "The args for the pomodoro sound."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-short-break-sound (when load-file-name
                                        (concat (file-name-directory load-file-name)
                                                "resources/bell.wav"))
  "The path to a sound file that´s to be played when a break was finished."
  :group 'pomodoro
  :type 'file)

(defcustom pomodoro-short-break-sound-args nil
  "The args for the short-break sound."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-long-break-sound (when load-file-name
                                       (concat (file-name-directory load-file-name)
                                               "resources/bell_multiple.wav"))
  "The path to a sound file that´s to be played when a long break is finished."
  :group 'pomodoro
  :type 'file)

(defcustom pomodoro-long-break-sound-args nil
  "The args for the long-break sound."
  :group 'pomodoro
  :type 'string)

;; Hooks
(defvar pomodoro-message-hook nil
  "Hook run on pomodoro notification.
The function take one argument that is the message to be
diplayed")
(defvar pomodoro-started-hook nil
  "Hooks run when a pomodoro is started.")
(defvar pomodoro-rewind-hook nil
  "Hooks run when a pomodoro is rewind.")
(defvar pomodoro-out-hook nil
  "Hooks run when a pomodoro is out.")
(defvar pomodoro-stop-hook nil
  "Hooks run when a pomodoro is stop.")
(defvar pomodoro-finished-hook nil
  "Hooks run when a pomodoro is finished.")
(defvar pomodoro-break-finished-hook nil
  "Hook run after any break has finished.
Run before a break's specific hook.")
(defvar pomodoro-long-break-finished-hook nil
  "Hooks run when a long break is finished.")
(defvar pomodoro-short-break-finished-hook nil
  "Hooks run when short break is finished.")

;; Faces
(defface pomodoro-mode-line-face
  '((t (:foreground "tomato1")))
  "Face of a pomodoro in the modeline."
  :group 'faces)
(defface pomodoro-mode-line-break-face
  '((t (:foreground "#2aa198")))
  "Face of a pomodoro break in the modeline ."
  :group 'faces)

;; Control Variables
(defvar pomodoro-display-string "")
(put 'pomodoro-display-string 'risky-local-variable t)
(defvar pomodoro-minute)
(defvar pomodoro-set)
(defvar pomodoro-timer nil)
(defvar pomodoro-state 'none)

;;;###autoload
(defun pomodoro ()
  "Start pomodoro, also rewind pomodoro to first set."
  (interactive)
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'pomodoro-display-string global-mode-string)
      (setq global-mode-string
            (append global-mode-string '(pomodoro-display-string))))
  (if (eq pomodoro-state 'out)
      (setq pomodoro-minute pomodoro-work-time
            pomodoro-state 'work
            pomodoro-timer (run-at-time t 60 'pomodoro-timer))
    (setq pomodoro-minute pomodoro-work-time
          pomodoro-set 1
          pomodoro-state 'work
          pomodoro-timer (run-at-time t 60 'pomodoro-timer)))
  (pomodoro-play-sound 'work)
  (pomodoro-update-modeline)
  (run-hooks 'pomodoro-started-hook))

(defun pomodoro-rewind ()
  "Rewind pomodoro, keep current set."
  (interactive)
  (setq pomodoro-minute pomodoro-work-time
        pomodoro-state 'work)
  (pomodoro-play-sound 'work)
  (pomodoro-update-modeline)
  (run-hooks 'pomodoro-rewind-hook))

(defun pomodoro-out ()
  "Stop time but keep current set."
  (interactive)
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (setq pomodoro-display-string ""
        pomodoro-state 'out)
  (pomodoro-play-sound 'killed)
  (when (get-buffer pomodoro-buffer-name)
    (kill-buffer "*pomodoro*"))
  (run-hooks 'pomodoro-out-hook))

(defun pomodoro-stop ()
  "Stop pomodoro."
  (interactive)
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (setq pomodoro-display-string "")
  (setq pomodoro-state 'none)
  (pomodoro-play-sound 'killed)
  (when (get-buffer pomodoro-buffer-name)
    (kill-buffer "*pomodoro*"))
  (run-hooks 'pomodoro-stop-hook))

(defun pomodoro-timer ()
  "Function called every minute.
It takes care of updating the modeline as well a message buffer"
  (setq pomodoro-minute (- pomodoro-minute 1))
  (when (<= pomodoro-minute 0)
    (cond ((eq pomodoro-state 'long-break)
           (pomodoro-long-break-finished)
           )
          ((eq pomodoro-state 'short-break)
           (pomodoro-short-break-finished)
           )
          ((eq pomodoro-state 'work)
           (pomodoro-work-finished)
           )))
  (pomodoro-update-modeline))

(defun pomodoro-toggle ()
  "Toggle start or stop pomodoro."
  (interactive)
  (if (eq pomodoro-state 'none)
      (pomodoro)
    (if (y-or-n-p "Already a running pomodoro.  Would you like to stop it? ")
        (progn
          (pomodoro-stop))
      (message "Keep up the work!"))))

(defun pomodoro-action ()
  "If pomodoro not has started, run one but if running ask for action."
  (interactive)
  (if (eq pomodoro-state 'none)
      (pomodoro)
    (progn
      (message "[S] Stop  [O] Out  [R] Rewind  [P] Re-start [q] quit")
      (let ((pomoaction (pcase (read-char)
                          (?S "stop")
                          (?O "out")
                          (?R "rewind")
                          (?P "restart")
                          (?q "quit"))))
        (if pomoaction
            (pcase pomoaction
              ("stop" (pomodoro-stop))
              ("out" (pomodoro-out))
              ("rewind" (pomodoro-rewind))
              ("restart" (pomodoro))
              ("quit" (message "Quit..")))
          (message "Nothing to do.."))))))

(defun pomodoro-org ()
  "Minor laucnh a new pomodoro or stop the current one for `org-mode`.
Need add hook for work properly.
Example
---------
 (add-hook 'org-clock-in-hook
            (lambda ()
              (pomodoro)
              (add-hook 'pomodoro-break-finished-hook 'org-clock-in-last)
              (add-hook 'pomodoro-finished-hook 'org-clock-out)) 'append)
 (add-hook 'org-clock-cancel-hook
          (lambda ()
            (pomodoro-stop)
            (remove-hook 'pomodoro-break-finished-hook 'org-clock-in-last)
            (remove-hook 'pomodoro-finished-hook 'org-clock-out)) 'append)"
  (interactive)
  (if (eq pomodoro-state 'none)
      (progn
        (cond
         ((eq major-mode 'org-mode)
          (call-interactively 'org-clock-in))
         ((eq major-mode 'org-agenda-mode)
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (call-interactively 'org-clock-in)))
         (t (let ((current-prefix-arg '(4)))
              (call-interactively 'org-clock-in))))
        (pomodoro))
    (if (y-or-n-p "Already a running pomodoro.  Would you like to stop it? ")
        (progn
          (pomodoro-stop)
          (org-clock-cancel))
      (message "Keep up the work!"))))

(defun pomodoro-work-finished ()
  "Is invoked when a pomodoro was finished successfully.
This may send a notification, rise frame, play a sound and start a pomodoro break."
  (if (>= pomodoro-set pomodoro-set-number)
      (progn
        (setq pomodoro-minute pomodoro-long-break
              pomodoro-state 'long-break
              pomodoro-set 1)
        (pomodoro-message pomodoro-long-start-msg)
        (pomodoro-play-sound 'long-break))
    (setq pomodoro-minute pomodoro-short-break
          pomodoro-state 'short-break)
    (pomodoro-message pomodoro-short-start-msg)
    (pomodoro-play-sound 'short-break))
  (run-hooks 'pomodoro-finished-hook))


(defun pomodoro-short-break-finished ()
  "Is invoked when a long break is finished.
This may send a notification, rise frame and play a sound."
  (setq pomodoro-state 'work
        pomodoro-minute pomodoro-work-time)
  (setq pomodoro-set (+ pomodoro-set 1))
  (pomodoro-message pomodoro-short-end-msg)
  (pomodoro-play-sound 'work)
  (run-hooks 'pomodoro-break-finished-hook 'pomodoro-short-break-finished-hook))

(defun pomodoro-long-break-finished ()
  "Is invoked when a break is finished.
This may send a notification, rise frame and play a sound."
  (setq pomodoro-state 'work
        pomodoro-minute pomodoro-work-time)
  (pomodoro-message pomodoro-long-end-msg)
  (pomodoro-play-sound 'work)
  (run-hooks 'pomodoro-break-finished-hook 'pomodoro-long-break-finished-hook))

(defun pomodoro-active-p ()
  "Retrieve whether org-pomodoro is active or not."
  (not (eq pomodoro-state 'none)))

(defun pomodoro-format-seconds ()
  "Format the countdown with the format specified in org-pomodoro-time-format."
  (format-seconds pomodoro-time-format pomodoro-minute))

(defun pomodoro-update-modeline ()
  "Update the modeline."
  (let ((s (cl-case pomodoro-state
             ('work
              (propertize pomodoro-work-format 'face 'pomodoro-mode-line-face))
             ('short-break
              (propertize pomodoro-break-format
                          'face 'pomodoro-mode-line-break-face))
             ('long-break
              (propertize pomodoro-long-break-format
                          'face 'pomodoro-mode-line-break-face)))))
    (setq pomodoro-display-string
          (if (pomodoro-active-p)
              (list " ["(format s pomodoro-set pomodoro-minute)"] ")
            nil)))
  (force-mode-line-update))

(defun pomodoro-play-sound (type)
  "Play an audio file specified by TYPE (work, killed, short-break, long-break).
take from org-pomodoro.el"
  (let ((sound (cl-case type
                 ('work pomodoro-sound)
                 ('killed pomodoro-killed-sound)
                 ('short-break pomodoro-short-break-sound)
                 ('long-break pomodoro-long-break-sound)
                 (t (error "Unknown pomodoro sound: %S" type))))
        (args (cl-case type
                ('work pomodoro-sound-args)
                ('killed pomodoro-killed-sound-args)
                ('short-break pomodoro-short-break-sound-args)
                ('long-break pomodoro-long-break-sound-args)
                (t (error "Unknown pomodoro sound: %S" type))))
        )
    (when (and pomodoro-play-sounds sound pomodoro-audio-player)
      (apply 'call-process `(,pomodoro-audio-player nil 0 nil ,@(delq nil (list sound args)))))))

(defun pomodoro-message (pomodoro-msg)
  "Display a message POMODORO-MSG in a buffer and maybe raise Emacs frame."
  (cond  ((eq pomodoro-notify 'buffer)
          (when pomodoro-raise-frame
            (raise-frame (selected-frame)))
          (let ((this-window (selected-window)))
            (with-current-buffer (get-buffer-create pomodoro-buffer-name)
              (erase-buffer)
              (insert pomodoro-msg))
            (window-configuration-to-register :pomodoro-buffer-notify)
            (pop-to-buffer pomodoro-buffer-name)
            (funcall 'Info-mode)
            (fit-window-to-buffer)
            (run-at-time "5 sec" nil (lambda()
                                       (kill-buffer pomodoro-buffer-name)
                                       (jump-to-register :pomodoro-buffer-notify)))
            (select-window this-window)))
         ((eq pomodoro-notify 'message)
          (message pomodoro-msg)))
  (run-hook-with-args 'pomodoro-message-hook pomodoro-msg))

(provide 'pomodoro+)

;;; pomodoro+.el ends here
