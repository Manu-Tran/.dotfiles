;;; ../my-config/doom.d/exercism.el -*- lexical-binding: t; -*-

;; Directory containing all the exercism problems
(defvar exercism-root-dir "~/Exercism")
(defvar exercism-current-problem nil)
(defvar exercism-current-track "rust")

(defvar exercism-mode-map (make-sparse-keymap))
(defvar exercism-mode-hook nil)

(map! :localleader
      :map exercism-mode-map
      :prefix "E"
      "p"  #'exercism/choose-problem
      "S"  #'exercism/submit-current-problem
      "t"  #'exercism/choose-track)

(defun exercism/to-file-name(file &optional test)
  (if (string-equal exercism-current-track "go")
      (subst-char-in-string ?- ?_ (concat file (if test "-test.go" ".go")))
    (if (string-equal exercism-current-track "emacs-lisp")
        (concat file (if test "-test.el" ".el"))
        (if (string-equal exercism-current-track "rust")
            (if test (concat "tests" "/" file ".rs") "src/lib.rs")
        file))))


(defun exercism/setup-workspace()
  (delete-other-windows)
  ;; If exervism current problem is set, open and setup the right layout
  (when exercism-current-problem
    (let ((exercism-current-dir (exercism/get-current-dir)))
      ;; left pane
      (find-file (concat exercism-current-dir "/" (exercism/to-file-name exercism-current-problem t)))
      (find-file (concat exercism-current-dir "/" (exercism/to-file-name exercism-current-problem)))
      (+evil/window-vsplit-and-follow)
      ;; right pane
      (find-file (concat exercism-current-dir "/README.md"))
      )
    )
  )

(defun exercism/choose-problem-action(file)
  (setq exercism-current-problem file)
  )

(defun exercism/get-current-dir(&optional dir)
  (if dir
      (concat exercism-root-dir "/" exercism-current-track "/" dir)
    (concat exercism-root-dir "/" exercism-current-track "/" exercism-current-problem))
  )

(defun exercism/mark-problem-as-done()
  (interactive)
  (dired-create-empty-file (concat (exercism/get-current-dir) "/.completed"))
  )

(defun exercism/choose-track-action(file)
  (setq exercism-current-track file)
  )

(defun exercism/list-track()
  (cl-remove-if (lambda(string) (if (string-prefix-p "." string) t nil))
                (directory-files (concat exercism-root-dir "/"))
                ))

(defun exercism/change-track()
  (interactive)
  (ivy-read (concat "[" exercism-current-track "] " "Switch to track: ")
            (exercism/list-track)
            :sort t
            :action #'exercism/choose-track-action
            )
  )

(defun exercism/list-problems(include-completed)
  (cl-remove-if (lambda(dir) (or include-completed
                                 (file-exists-p
                                  (concat (exercism/get-current-dir dir) "/.completed"))))
                (cl-remove-if (lambda(string) (if (string-prefix-p "." string) t nil))
                              (directory-files (concat exercism-root-dir "/" exercism-current-track "/"))
                              )))

(defun exercism/choose-problem(&optional include-completed)
  (interactive)
  (ivy-read (concat "[" exercism-current-problem "] " "Switch to problem: ")
            (exercism/list-problems include-completed)
            :require-match t
            :sort t
            :action #'exercism/choose-problem-action
            )
  (exercism/setup-workspace)
  )

(defun exercism/submit-current-problem-action()
  (interactive)
    (progn (shell-command (concat "cd " (exercism/get-current-dir) "; exercism submit"))))

(defun exercism/open-webpage()
  (interactive)
  (browse-url
   (concat "https://exercism.org/tracks/"
           exercism-current-track
           "/exercises/"
           exercism-current-problem))
)

(defun exercism/submit-current-problem()
  "Submit a solution for the current problem using aocd"
  (interactive)
  (when (y-or-n-p (concat "Confirm sending : " ))
  (run-with-timer 1 nil #'exercism/submit-current-problem-action)))

(define-minor-mode exercism-mode
  "For solving exercism issues"
  :lighter " exercism"
  :keymap exercism-mode-map)
