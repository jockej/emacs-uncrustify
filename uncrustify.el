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

(defconst uncrustify--prefix "uncrustify:")

(defvar uncrustify-config-file nil
  "The config file to be passed to uncrustify.")

(defvar uncrustify-files-regexp nil
  "Regexp to determine which files uncrustify will run on. A common example
  would be: \"*.[ch]\".")

(defun uncrustify--sentinel (proc event)
  ""
  (when (string= event "finished\n")
    (let ((buf (buffer-name (process-buffer proc))))
      (when buf
        (with-current-buffer buf
          (insert (format "%s: " (process-name proc)))
          (if (= (process-exit-status proc) 0)
              (insert "PASS\n")
            (let ((beg (point)))
              (insert "FAIL")
              (put-text-property beg (point) 'face 'warning)
              (insert "\n"))))))))
            

(defun uncrustify--uncrustify-file (file &optional only-check)
  "Run uncrustify on FILE. With optional argument ONLY-CHECK run uncrustify with
  argument --check."
  (let* ((procname (format "%s%s" uncrustify--prefix
                          file))
        (prog (or uncrustify-program (executable-find "uncrustify")))
        (proc (start-process procname uncrustify-process-output-buffer
                             prog "-c" uncrustify-config-file
                             (if only-check "--check" "--no-backup")
                             "-f" file)))
    (when only-check
      (set-process-sentinel proc uncrustify--sentinel))))

(defun uncrustify-region (start end &optional only-check)
  (interactive "r\nP")
  (let ((cmd
         (format "%s -c %s %s"
                 uncrustify-program
                 uncrustify-config-file
                 (if only-check "--check" "--no-backup"))))
    (shell-command-on-region start end cmd
                             uncrustify-process-output-buffer
                             only-check)))


(defun uncrustify-defun (&optional only-check)
  "Uncrustify the current defun."
  (interactive "P")

  )

(defun uncrustify-directory (dir &optional only-check)
  "Uncrustify all files matching `uncrustify-files-regexp' recursively, starting
  from DIR."
  (interactive "DDirectory: \nP")
  (let ((files
         (find-lisp-find-files dir uncrustify-files-regexp))
        (subdirs
         (find-lisp-find-dired-subdirectories dir)))
    (mapc
     (lambda (file) (uncrustify--uncrustify-file file only-check)) files)
    (mapc
     (lambda (subdir) uncrustify-directory subdir only-check) subdirs)))


(defun uncrustify-project (&optional only-check)
  "Uncrustify all files matching `uncrustify-files-regexp' recursively starting
  from `vc-root-dir'."
  (interactive "P")
  
  )


(defun uncrustify-file (file &optional only-check)
  "Uncrustify a single file."
  (interactive "ffile: \nP")

  )
  
