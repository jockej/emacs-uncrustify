;; lexical-binding: t

;; Copyright (C) 2015  Joakim Jalap

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'find-lisp)

(defgroup uncrustify nil
  "Customization group for the uncrustify package."
  :version "24.5"
  :package-version '(uncrustify . "0.1"))


(defcustom uncrustify-program "uncrustify"
  "The uncrustify program. If `nil' use the value (executable-find
  \"uncrustify\")."
  :type '(string))


(defcustom uncrustify-process-output-buffer "*uncrustify*"
  "The name of the buffer where uncrustify should print its output."
  :type '(string))


(defcustom uncrustify-check-buffer "*uncrustify-check*"
  "The name of the buffer where uncrustify should print the result of checks."
  :type '(string))


(defcustom uncrustify-pop-to-check-buffer t
  "Pop to the check buffer when running a check."
  :type '(boolean))


(defcustom uncrustify-only-show-failures nil
  "Only show failed files in the check buffer."
  :type '(boolean))


(defcustom uncrustify-clear-buffer-each-run t
  "Clear the check buffer before each new run."
  :type '(boolean))


(defvar uncrustify-config-file nil
  "The config file to be passed to uncrustify. Must be a full path, as
  uncrustify seems to do no expansion. So for example \"~/uncrustify.cfg\" will
  _not_ work. Instead use `expand-file-name'.")


(defvar uncrustify-files-regexp nil
  "Regexp to determine which files uncrustify will run on. A common example
  would be: \".*\\\\.[ch]\".")


(defmacro uncrustify--with-check-buffer (&rest body)
  "Run BODY in `uncrustify-check-buffer', create it if it doesn't exist."
  nil
  `(with-current-buffer (get-buffer-create uncrustify-check-buffer)
     (unless (eq major-mode 'uncrustify-check-mode)
       (uncrustify-check-mode))
     (goto-char (point-max))
     (beginning-of-line)
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)))


(defun uncrustify--button-action (button)
  "Action to take when a button has been pressed in the uncrustify-check-buffer.
  Opens the file in the other window."
  (let* ((start (button-start button))
         (end (button-end button))
         (fname (buffer-substring-no-properties start end)))
    (find-file-other-window fname)))


(defun uncrustify--sentinel (proc event)
  "Inserts the result of an uncrustify run in the check buffer."
     (insert (format "%s: " (process-name proc)))
     (cond ((= (process-exit-status proc) 0)
            (unless uncrustify-only-show-failures
              (insert "PASS\n")))
           ((= (process-exit-status proc) 1)
            (let* ((beg (point))
                   (eof (- beg 2)))
              (insert "FAIL")
              (put-text-property beg (point) 'face 'warning)
              (make-button bol eof
                           'action 'uncrustify--button-action
                           'follow-link t
                           'help-echo "mouse-2, RET: Visit file")
              (end-of-line)
              (insert "\n")))
           (t (insert "ERROR\n"))))))


(defun uncrustify--file-internal-async (file only-check)
  "Run uncrustify on FILE. With optional argument ONLY-CHECK run uncrustify with
  argument --check."
  (unless uncrustify-config-file (error "uncrustify-config-file not set"))
  (let ((proc (if only-check
                  (start-process file uncrustify-process-output-buffer
                                 uncrustify-program
                                 "-c" uncrustify-config-file "--check" "-f" file)
                (start-process file uncrustify-process-output-buffer
                               uncrustify-program
                               "-c" uncrustify-config-file "--no-backup" file))))
    (when only-check
      (set-process-sentinel proc 'uncrustify--sentinel))))


(defun uncrustify-file (file &optional only-check)
  "Uncrustify a single file.

With non-nil prefix argument, only check, do not overwrite."
  (interactive "ffile: \nP")
  (unless uncrustify-config-file (error "uncrustify-config-file not set"))
  (if only-check
      (let ((exit-status
             (call-process uncrustify-program nil nil nil
                           "-c" uncrustify-config-file "--check" "-f" file)))
        (cond ((= exit-status 0) (message "PASS"))
              ((= exit-status 1) (message "FAIL"))
              (t (error "Unknown exit status %d" exit-status))))
    (call-process uncrustify-program nil nil nil                               
                  "-c" uncrustify-config-file "--no-backup" file)))


(defun uncrustify-region (start end &optional only-check)
  "Run uncrustify on the current region.

With non-nil prefix argument, only check, do not overwrite."
  (interactive "r\nP")
  (unless uncrustify-config-file (error "uncrustify-config-file not set"))  
  (let ((exit-status
         (call-process-region start end uncrustify-program (not only-check)
                              (if only-check uncrustify-process-output-buffer
                                t) nil
                                (if only-check "--check" "-q")
                                "-c" uncrustify-config-file)))
    (when only-check
      (cond ((= exit-status 0) (message "PASS"))
            ((= exit-status 1) (message "FAIL"))
            (t (error "Unknown exit status %d" exit-status))))))


(defun uncrustify-defun (&optional only-check)
  "Uncrustify the current defun.

With non-nil prefix argument, only check, do not overwrite."
  (interactive "P")
  (save-excursion
    (let ((start (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (uncrustify-region start end only-check))))


(defvar uncrustify--ununcrustified-files 0
  "The nbumber of files which have not yet been uncrustified by
  `uncrustify-directory'. If this is > 0 we may not start a new
  `uncrustify-directory' run.")


(defun uncrustify-directory (dir &optional only-check)
  "Uncrustify all files matching `uncrustify-files-regexp' recursively, starting
  from DIR.

With non-nil prefix argument, only check, do not overwrite."
  (interactive "DDirectory: \nP")
  (unless uncrustify-config-file (error "uncrustify-config-file not set"))
  (unless uncrustify-files-regexp (error "uncrustify-files-regexp not set"))
  (let ((files
         (find-lisp-find-files dir uncrustify-files-regexp)))
    (when only-check (setq uncrustify--ununcrustified-files (length files)))
    (mapc (lambda (file) (uncrustify--file-internal file only-check)) files))
  (when (and uncrustify-pop-to-check-buffer only-check)
    (pop-to-buffer uncrustify-check-buffer)))


(defun uncrustify-project (&optional only-check)
  "Uncrustify all files matching `uncrustify-files-regexp' recursively starting
  from `projectile-project-root'.

With non-nil prefix argument, only check, do not overwrite."
  (interactive "P")
  (unless (fboundp 'projectile-mode)
    (error "This function requires projectile to be active"))
  (let ((root
         (projectile-project-root)))
    (uncrustify-directory root only-check)))


(defmacro uncrustify--with-unlocked-buffer (&rest body)
  "Run BODY in the current buffer (which is assumed to be the uncrustify check
  buffer) with read only mode disabled. Puts point at `point-max'."
  nil
  `(progn
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)
     (goto-char (point-max))))


(defun uncrustify-xpunge-passed ()
  "Remove all files that passed."
  (interactive)
  (uncrustify--with-unlocked-buffer
   (goto-char (point-min))
   (let ((search-upper-case t))
     (delete-matching-lines ".*: PASS$"))))


(defun uncrustify-rerun-file ()
  "Re-run uncrustify on the file at point, temporarily binding
`uncrustify-clear-buffer-each-run to nil and with ONLY-CHECK non-nil."
  (interactive)
  (let ((uncrustify-clear-buffer-each-run nil)
        ;; Have to remove the final ':'
        (file (substring (thing-at-point 'filename t) 0 -1)))
    (if file (uncrustify-file file t)
      (user-error "No filename at point"))))


(define-derived-mode uncrustify-check-mode tabulated-list-mode
  "Uncrustify-Check"
  "A major mode for the `uncrustify-check-buffer'."
  :group 'uncrustify

  (setq tabulated-list-format
        [("File" 25 nil . nil) ("Status" 7 nil . nil)])
  (tabulated-list-init-header)
  (setq tabulated-list-revert-hook 'uncrustify-revert-buffer)
  
  (define-key uncrustify-check-mode-map "r" 'uncrustify--rerun-file)
  (define-key uncrustify-check-mode-map "x" 'uncrustify--xpunge-passed))

;; uncrustify.el ends here... just kidding, it actually ends here
