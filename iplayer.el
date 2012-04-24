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
    ("*" . "BBC Radio 4 Extra")))

(defun iplayer-preset (&optional prefix)
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

(defconst iplayer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "0") 'iplayer)
    (let ((presets "123456789!\"£$%^&*()"))
      (dotimes (i (length presets))
        (define-key map (read-kbd-macro (substring presets i (1+ i)))
          'iplayer-preset)))
    (define-key map (kbd "RET") 'iplayer-download)
    map
    ))

(defun iplayer-mode ()
  "A major mode for the BBC's iPlayer.
\\{iplayer-mode-map}"
  (interactive)
  (use-local-map iplayer-mode-map)
  (setq major-mode 'iplayer-mode mode-name "iPlayer"))

(defun iplayer ()
  (interactive)
  (setq mode-line-process nil)
  (display-iplayer-tree (get-iplayer-tree)))

(provide 'iplayer)
