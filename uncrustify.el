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

(defconst uncrustify-program nil
  "The uncrustify program. If `nil' use the value (executable-find
  \"uncrustify\").")


(defconst uncrustify-process-output-buffer "*uncrustify*"
  "The name of the buffer where uncrustify should print its output.")


(defconst uncrustify-check-buffer "*uncrustify-check*"
  "The name of the buffer where uncrustify should print the result of checks.")


(defconst uncrustify-pop-to-check-buffer t
  "Pop to the check buffer when running a check.")


(defvar uncrustify-config-file nil
  "The config file to be passed to uncrustify.")


(defvar uncrustify-files-regexp nil
  "Regexp to determine which files uncrustify will run on. A common example
  would be: \".*\\\\.[ch]\".")


(defun uncrustify--button-action (button)
  "Action to take when a button has been pressed in the uncrustify-check-buffer.
  Opens the file in the other window."
  (let* ((start (button-start button))
         (end (button-end button))
         (fname (buffer-substring-no-properties start end)))
    (find-file fname)))


(defun uncrustify--sentinel (proc event)
  ""
  (with-current-buffer (get-buffer-create uncrustify-check-buffer)
    (beginning-of-line)
    (let ((bol (point)))
      (insert (format "%s: " (process-name proc)))
      (cond ((= (process-exit-status proc) 0)
             (insert "PASS\n"))
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


(defun uncrustify--file-internal (file only-check)
  "Run uncrustify on FILE. With optional argument ONLY-CHECK run uncrustify with
  argument --check."
  (unless uncrustify-config-file (error "uncrustify-config-file not set"))
  (let* ((prog (or uncrustify-program (executable-find "uncrustify")
                   (error "Uncrustify not found")))
         (proc (if only-check
                   (start-process file uncrustify-process-output-buffer
                                  prog "-c" uncrustify-config-file "--check" "-f" file)
                 (start-process file uncrustify-process-output-buffer
                                prog "-c" uncrustify-config-file "--no-backup" file))))
    (when only-check
      (set-process-sentinel proc 'uncrustify--sentinel))))


(defun uncrustify-region (start end &optional only-check)
  (interactive "r\nP")
  (let* ((prog (or uncrustify-program (executable-find "uncrustify")
                   (error "Uncrustify not found")))
         (exit-status
          (call-process-region start end prog (not only-check)
                               (if only-check uncrustify-process-output-buffer
                                 t) nil
                                 (if only-check "--check" "-q")
                                 "-c" uncrustify-config-file)))
    (when only-check
      (cond ((= exit-status 0) (message "PASS"))
            ((= exit-status 1) (message "FAIL"))
            (t (error "Unknown exit status %d" exit-status))))))


(defun uncrustify-defun (&optional only-check)
  "Uncrustify the current defun."
  (interactive "P")
  (let ((start (beginning-of-defun))
        (end (end-of-defun)))
    (uncrustify-region start end only-check)))


(defmacro uncrustify--check-header (place)
  " " nil
  `(with-current-buffer (get-buffer-create uncrustify-check-buffer)
     (insert (format "\n\n[%s] Uncrustifying %s\n"
                     (substring (current-time-string) 4 -5) ,place))))

(defun uncrustify-directory (dir &optional only-check)
  "Uncrustify all files matching `uncrustify-files-regexp' recursively, starting
  from DIR."
  (interactive "DDirectory: \nP")
  (unless uncrustify-config-file (error "uncrustify-config-file not set"))
  (unless uncrustify-files-regexp (error "uncrustify-files-regexp not set"))
  (when only-check (uncrustify--check-header dir))
  (let ((files
         (find-lisp-find-files dir uncrustify-files-regexp)))
    (mapc (lambda (file) (uncrustify--file-internal file only-check)) files))
  (when (and uncrustify-pop-to-check-buffer only-check)
    (pop-to-buffer uncrustify-check-buffer)))


(defun uncrustify-project (&optional only-check)
  "Uncrustify all files matching `uncrustify-files-regexp' recursively starting
  from `vc-root-dir'."
  (interactive "P")
  (unless (fboundp 'projectile-mode)
    (error "This function requires projectile to be active"))
  (let ((root
         (projectile-project-root)))
    (uncrustify-directory root only-check)))


(defun uncrustify-file (file &optional only-check)
  "Uncrustify a single file."
  (interactive "ffile: \nP")
  (when only-check (uncrustify--check-header file))
  (uncrustify--file-internal file only-check)
  (when (and uncrustify-pop-to-check-buffer only-check)
    (pop-to-buffer uncrustify-check-buffer)))
