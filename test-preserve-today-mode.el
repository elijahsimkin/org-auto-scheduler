;;; test-preserve-today-mode.el --- Tests for clocked-only preserve mode -*- lexical-binding: t -*-

(ignore-errors (package-initialize))
(require 'org)
(require 'cl-lib)
(load-file "org-auto-scheduler.el")

(defvar test-failures 0)

(defun assert-equal (actual expected desc)
  (if (equal actual expected)
      (message "PASS: %s" desc)
    (setq test-failures (1+ test-failures))
    (message "FAIL: %s\n  Expected: %S\n  Actual:   %S" desc expected actual)))

(defun assert-true (val desc)
  (if val
      (message "PASS: %s" desc)
    (setq test-failures (1+ test-failures))
    (message "FAIL: %s (expected non-nil, got %S)" desc val)))

(defun test-get-slot (offset-minutes length-minutes)
  "Return an Org range timestamp starting OFFSET-MINUTES from now."
  (let* ((start (time-add (current-time) (* 60 offset-minutes)))
         (end (time-add start (* 60 length-minutes))))
    (format "<%s-%s>"
            (format-time-string "%Y-%m-%d %a %H:%M" start)
            (format-time-string "%H:%M" end))))

(defun test-get-task (heading id slot &optional extra-properties)
  "Return Org text for an AUTOSCH task HEADING with ID scheduled at SLOT."
  (format "** TODO %s :AUTOSCH:\nSCHEDULED: %s\n:PROPERTIES:\n:Effort: 0:30\n:ID: %s\n%s:END:\n"
          heading slot id (or extra-properties "")))

(defun test-get-scheduled (file id)
  "Return the SCHEDULED timestamp of the task with ID in FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (re-search-forward (format ":ID:[ \t]*%s$" (regexp-quote id)))
    (org-entry-get nil "SCHEDULED")))

(defun test-get-start (file id)
  "Return the start time of the task with ID in FILE."
  (org-time-string-to-time (test-get-scheduled file id)))

(defmacro test-with-org-file (var contents &rest body)
  "Write CONTENTS to a temp Org file bound to VAR and run BODY with scheduler settings."
  (declare (indent 2))
  `(let* ((temp-dir (make-temp-file "org-preserve-mode-test-" t))
          (,var (expand-file-name "tasks.org" temp-dir))
          (org-auto-scheduler-sync-caldav nil)
          (org-auto-scheduler-silent-mode t)
          (org-auto-scheduler-preserve-today-scheduled t)
          (org-auto-scheduler-start-time "00:00")
          (org-auto-scheduler-end-time "23:59")
          (org-auto-scheduler-task-gap 0)
          (org-auto-scheduler-lapse-grace-minutes 10)
          (org-auto-scheduler--last-task-scores nil))
     (with-temp-file ,var
       (insert "* Tasks\n:PROPERTIES:\n:ORDERED: nil\n:END:\n" ,contents))
     (setq org-agenda-files (list ,var))
     (unwind-protect
         (progn ,@body)
       (kill-buffer (find-file-noselect ,var))
       (delete-directory temp-dir t))))

(defun test-run-background ()
  "Run the scheduler the way the background daemon does."
  (let ((org-auto-scheduler--current-run-type 'background-sync))
    (org-auto-scheduler-schedule-tasks)))

;; ============================================================================
;; TEST 1: Background runs replan lapsed tasks by priority in clocked-only mode
;; ============================================================================
(message "\n--- TEST 1: Lapsed high-priority task is replanned ahead of lower priority ---")

(test-with-org-file file
    (concat (test-get-task "[#A] Lapsed important" "lapsed-a" (test-get-slot -30 30))
            (test-get-task "[#C] Upcoming minor" "upcoming-c" (test-get-slot 60 30)))
  (let ((org-auto-scheduler-preserve-today-mode 'clocked-only))
    (test-run-background)
    (let ((lapsed-start (test-get-start file "lapsed-a"))
          (minor-start (test-get-start file "upcoming-c")))
      (assert-true (time-less-p (current-time) lapsed-start)
                   "Test 1: Lapsed [#A] task moved to the future")
      (assert-true (time-less-p lapsed-start minor-start)
                   "Test 1: Lapsed [#A] task placed before upcoming [#C] task"))))

;; ============================================================================
;; TEST 2: Tasks that started within the grace period stay put
;; ============================================================================
(message "\n--- TEST 2: Task within grace period is not replanned ---")

(test-with-org-file file
    (test-get-task "[#A] Just started" "grace-a" (test-get-slot -5 30))
  (let ((org-auto-scheduler-preserve-today-mode 'clocked-only)
        (original-slot (test-get-slot -5 30)))
    (test-run-background)
    (assert-equal (test-get-scheduled file "grace-a") original-slot
                  "Test 2: Task 5 minutes into its slot keeps its slot")))

;; ============================================================================
;; TEST 3: Default mode keeps lapsed tasks on background runs
;; ============================================================================
(message "\n--- TEST 3: Default mode leaves lapsed tasks alone on background runs ---")

(test-with-org-file file
    (test-get-task "[#A] Lapsed important" "default-a" (test-get-slot -30 30))
  (let ((org-auto-scheduler-preserve-today-mode 'all)
        (original-slot (test-get-slot -30 30)))
    (test-run-background)
    (assert-equal (test-get-scheduled file "default-a") original-slot
                  "Test 3: Lapsed task keeps its slot in `all' mode")))

;; ============================================================================
;; TEST 4: Pinned lapsed tasks stay put on background runs
;; ============================================================================
(message "\n--- TEST 4: Pinned lapsed task is not replanned ---")

(test-with-org-file file
    (test-get-task "[#A] Pinned lapsed" "pinned-a" (test-get-slot -30 30) ":PINNED: t\n")
  (let ((org-auto-scheduler-preserve-today-mode 'clocked-only)
        (original-slot (test-get-slot -30 30)))
    (test-run-background)
    (assert-equal (test-get-scheduled file "pinned-a") original-slot
                  "Test 4: Pinned lapsed task keeps its slot")))

;; ============================================================================
;; TEST 5: Idle background runs with no changes don't move anything
;; ============================================================================
(message "\n--- TEST 5: Repeated idle runs make no changes ---")

(test-with-org-file file
    (concat (test-get-task "[#A] First" "idle-a" (test-get-slot 60 30))
            (test-get-task "[#C] Second" "idle-c" (test-get-slot 120 30)))
  (let ((org-auto-scheduler-preserve-today-mode 'clocked-only)
        (first-slot (test-get-slot 60 30))
        (second-slot (test-get-slot 120 30)))
    (test-run-background)
    (test-run-background)
    (assert-equal (test-get-scheduled file "idle-a") first-slot
                  "Test 5: First task keeps its slot across idle runs")
    (assert-equal (test-get-scheduled file "idle-c") second-slot
                  "Test 5: Second task keeps its slot across idle runs")))

;; ============================================================================
;; TEST 6: A priority change reorders today's upcoming tasks
;; ============================================================================
(message "\n--- TEST 6: Score change triggers a priority reorder ---")

(defun test-set-priority (file id priority)
  "Set the priority of the task with ID in FILE to PRIORITY and save."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (re-search-forward (format ":ID:[ \t]*%s$" (regexp-quote id)))
    (org-back-to-heading t)
    (org-priority priority)
    (save-buffer)))

(defun test-run-priority-swap (preserve-mode)
  "Swap two tasks' priorities between background runs in PRESERVE-MODE.
Return non-nil if the newly raised task now starts first."
  (test-with-org-file file
      (concat (test-get-task "[#B] Early" "swap-early" (test-get-slot 60 30))
              (test-get-task "[#B] Late" "swap-late" (test-get-slot 120 30)))
    (let ((org-auto-scheduler-preserve-today-mode preserve-mode))
      (test-run-background)
      (test-set-priority file "swap-late" ?A)
      (test-set-priority file "swap-early" ?C)
      (test-run-background)
      (time-less-p (test-get-start file "swap-late") (test-get-start file "swap-early")))))

(assert-true (test-run-priority-swap 'clocked-only)
             "Test 6.1: Raised task moves ahead in `clocked-only' mode")
(assert-true (not (test-run-priority-swap 'all))
             "Test 6.2: Raised task keeps its later slot in `all' mode")

;; ============================================================================
;; TEST 7: Lapsed-kind classification
;; ============================================================================
(message "\n--- TEST 7: Lapsed-kind classification ---")

(let ((org-auto-scheduler-lapse-grace-minutes 10)
      (now (current-time))
      (marker (with-temp-buffer (point-marker))))
  (cl-letf (((symbol-function 'org-auto-scheduler--task-clocked-p) (lambda (_) t))
            ((symbol-function 'org-auto-scheduler-task-pinned-p) (lambda (_ _) nil)))
    (assert-equal (org-auto-scheduler--get-lapsed-kind marker "id" (time-subtract now 1800) now nil t)
                  nil
                  "Test 7.1: Background run never replans the clocked task")
    (assert-equal (org-auto-scheduler--get-lapsed-kind marker "id" (time-subtract now 1800) now t nil)
                  'clocked
                  "Test 7.2: Manual run still replans the clocked task"))
  (cl-letf (((symbol-function 'org-auto-scheduler--task-clocked-p) (lambda (_) nil))
            ((symbol-function 'org-auto-scheduler-task-pinned-p) (lambda (_ _) nil)))
    (assert-equal (org-auto-scheduler--get-lapsed-kind marker "id" (time-subtract now 1800) now nil t)
                  'lapsed
                  "Test 7.3: Background run replans a task 30 minutes past its start")
    (assert-equal (org-auto-scheduler--get-lapsed-kind marker "id" (time-subtract now 300) now nil t)
                  nil
                  "Test 7.4: Background run waits out the grace period")
    (assert-equal (org-auto-scheduler--get-lapsed-kind marker "id" (time-subtract now 300) now t nil)
                  'lapsed
                  "Test 7.5: Manual run ignores the grace period")))

(if (> test-failures 0)
    (progn (message "\nFAILURES DETECTED: %d" test-failures)
           (kill-emacs 1))
  (message "\nALL TESTS PASSED"))
