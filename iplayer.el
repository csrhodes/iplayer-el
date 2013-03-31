;;; iplayer.el --- Browse and download BBC TV/radio shows

;; Copyright (C) 2012-2013  Christophe Rhodes

;; Author: Christophe Rhodes <csr21@cantab.net>
;; Version: 0.1
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Requires and uses the 'get-iplayer' script to provide a
;; convenient interface to BBC iPlayer.

;;; Code:
(defvar iplayer-updating-cache-process nil)
(defvar iplayer-updating-cache-sentinel-info nil)
(defvar iplayer-updating-cache-sentinel-executing nil)

(defun iplayer-updating-cache-sentinel (process event)
  ;; FIXME: assumes that all went well
  (let* ((iplayer-updating-cache-sentinel-executing t)
         (info (reverse iplayer-updating-cache-sentinel-info)))
    (setq iplayer-updating-cache-process nil
          iplayer-updating-cache-sentinel-info nil)
    (dolist (info info)
      (let ((iplayer-command-frame (car info))
            (iplayer-command-window (cadr info))
            (iplayer-command-buffer (caddr info))
            (keys (car (cdddr info))))
        (when (and (frame-live-p iplayer-command-frame)
                   (window-live-p iplayer-command-window)
                   (buffer-live-p iplayer-command-buffer))
          (let ((old-frame (selected-frame))
                (old-window (selected-window))
                (old-buffer (current-buffer)))
            (let ((pre-command-hook
                   (lambda ()
                     (select-frame iplayer-command-frame)
                     (select-window iplayer-command-window)
                     (set-buffer iplayer-command-buffer)
                     (setq pre-command-hook nil))))
              ;; KLUDGE: execute-kbd-macro executes a normal
              ;; command-loop, whose first action is to select the
              ;; current frame and window, which is why we contort
              ;; things to select the frame/window/buffer we actually
              ;; want in pre-command-hook.  I'm actually surprised
              ;; that it works, but mine is not too much to reason
              ;; why; lots of other ways to try to achieve this didn't
              ;; in fact work.
              (execute-kbd-macro keys)
              ;; KLUDGE: and then we restore old state
              (select-window old-window)
              (select-frame old-frame)
              (set-buffer old-buffer))))))
    (message "Done updating iPlayer cache")))

(defmacro define-iplayer-command (name arglist &rest body)
  (let (docstring interactive)
    (when (stringp (car body))
      (setq docstring (car body) body (cdr body)))
    (when (and (consp (car body)) (eql (caar body) 'interactive))
      (setq interactive (car body) body (cdr body)))
    `(defun ,name ,arglist
       ,@(when docstring (list docstring))
       ,@(when interactive (list interactive))
       (unless iplayer-updating-cache-process
         (setq iplayer-updating-cache-process
               (start-process "updating-iplayer" " *updating-iplayer*"
                              "get-iplayer" "--type" "radio,tv" "-q"))
         (set-process-sentinel iplayer-updating-cache-process
                               'iplayer-updating-cache-sentinel)
         (message "Updating iPlayer cache"))
       (if iplayer-updating-cache-sentinel-executing
           (progn ,@body)
         (push (list (selected-frame) (selected-window) (current-buffer) (this-command-keys-vector))
               iplayer-updating-cache-sentinel-info)))))

(defun get-iplayer-tree (&rest args)
  (with-temp-buffer
    (apply #'call-process "get-iplayer" nil t nil "--nocopyright" "--type" "radio,tv" "--tree" "--terse" args)
    (goto-char (point-min))
    (let (result program episodes)
      (while (< (point) (point-max))
        (cond
         ((looking-at "^\\w")
          (when (and program episodes)
            (push (cons program (reverse episodes)) result))
          (setf program (buffer-substring (point) (progn (end-of-line) (point))))
          (when (string-match "^\\(tv\\|radio\\), " program)
            (setq program (substring program (match-end 0))))
          (setf episodes nil)
          (unless (= (point) (point-max))
            (forward-char)))
         ((looking-at "^  \\([0-9]+\\):\\s-\\(.*\\)$")
          (let ((episode
                 (cons (buffer-substring (match-beginning 1) (match-end 1))
                       (buffer-substring (match-beginning 2) (match-end 2)))))
            (when (string-match "^\\(tv\\|radio\\), " (cdr  episode))
              (rplacd episode (substring (cdr episode) (match-end 0))))
            (push episode episodes))
          (forward-line))
         (t (forward-line))))
      (reverse result))))

(defun display-iplayer-tree (tree)
  (with-current-buffer (get-buffer-create "*iplayer*")
    (delete-region (point-min) (point-max))
    (iplayer-mode)
    (orgstruct-mode 1)
    (dolist (entry tree)
      (let ((program (car entry))
            (episodes (cdr entry)))
        (insert (propertize (format "* %s\n" program) 'face 'outline-1))
        (dolist (episode episodes)
          (insert (propertize (format "** %s\n" (cdr episode))
                              'face 'outline-2 'iplayer-id (car episode))))))
    (org-overview)
    (goto-char (point-min)))
  (switch-to-buffer (get-buffer-create "*iplayer*")))

(defvar iplayer-presets
  '(("1" . "BBC One")
    ("2" . "BBC Two")
    ("3" . "BBC Three")
    ("4" . "BBC Four")

    ("!" . "BBC Radio 1")
    ("\"" . "BBC Radio 2")
    ("£" . "BBC Radio 3")
    ("$" . "BBC Radio 4")
    ("%" . "BBC Radio 5 live")
    ("^" . "BBC 6 Music")
    ("&" . "BBC 7")
    ("*" . "BBC Radio 4 Extra"))
  "Alist mapping keys to iPlayer channels.

Used in the `iplayer-preset' command.")

(define-iplayer-command iplayer-preset (&optional prefix)
  "Switch display to a preset channel.

The presets are defined in the variable `iplayer-presets'."
  (interactive "p")
  (let ((keys (this-command-keys))
        (presets (mapcar (lambda (x) (cons (read-kbd-macro (car x)) (cdr x))) iplayer-presets)))
    (cond
     ((= (length keys) 1)
      (let ((channel (cdr (assoc keys presets))))
        (if channel
            (progn
              (setq mode-line-process (format "[%s]" channel))
              (iplayer-channel (format "^%s$" channel)))
          (error "no preset for key %s" keys)))))))

(defun iplayer-channel (channel)
  (display-iplayer-tree (get-iplayer-tree "--channel" channel)))

(defun iplayer-download ()
  (interactive)
  (let ((id (get-text-property (point) 'iplayer-id)))
    (if id
        (let ((default-directory "~/iPlayer/"))
          ;; should probably use a process filter instead to give us a
          ;; progress bar
          (message "downloading id %s" id)
          (start-process "get-iplayer" " *get-iplayer*" "get-iplayer" "--get" (format "%s" id)))
      (message "no id at point"))))

(defun iplayer-previous ()
  (interactive)
  (save-match-data
    (outline-previous-heading)
    (while (and (= (funcall outline-level) 1) (not (bobp)))
      (outline-previous-heading)))
  (hide-other)
  (unless (bobp)
    (save-excursion
      (outline-up-heading 1 t)
      (show-children))))

(defun iplayer-next ()
  (interactive)
  (save-match-data
    (outline-next-heading)
    (while (and (= (funcall outline-level) 1) (not (eobp)))
      (outline-next-heading)))
  (hide-other)
  (save-excursion
    (outline-up-heading 1 t)
    (show-children)))

(defconst iplayer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "0") 'iplayer)
    (let ((presets "123456789!\"£$%^&*()"))
      (dotimes (i (length presets))
        (define-key map (read-kbd-macro (substring presets i (1+ i)))
          'iplayer-preset)))
    (define-key map (kbd "RET") 'iplayer-download)
    (define-key map (kbd "j") 'iplayer-next)
    (define-key map (kbd "k") 'iplayer-previous)
    map
    ))

(defun iplayer-mode ()
  "A major mode for the BBC's iPlayer.
\\{iplayer-mode-map}"
  (interactive)
  (use-local-map iplayer-mode-map)
  (setq major-mode 'iplayer-mode mode-name "iPlayer"))

(define-iplayer-command iplayer ()
  "Start the emacs iPlayer interface."
  (interactive)
  (setq mode-line-process nil)
  (display-iplayer-tree (get-iplayer-tree)))

;;;###autoload
(autoload 'iplayer "iplayer" "Start the emacs iPlayer interface." t)


(provide 'iplayer)
;;; iplayer.el ends here
