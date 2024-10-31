;;; calc-cal.el --- Calculate totals for dated entries -*- lexical-binding: t; -*-

;;; Licensed under MIT License

;; Author: agent314
;; Version: 1.0.9
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/pwnednetwork/nomnom

;;; ╔══════════════════════════════════════╗
;;; ║    < RUNTIME::INIT_SEQUENCE >        ║
;;; ╚══════════════════════════════════════╝
;;;
;;; NOTICE: Software generated with Claude 3.5 Sonnet AI assistance and human review.
;;;
;;; PROVIDED "AS IS" WITHOUT WARRANTY. AUTHORS NOT LIABLE FOR ANY DAMAGES OR CLAIMS
;;; ARISING FROM USE OR IMPLEMENTATION.

;;; Revision History:


;; Version 1.0.9 (2024-10-30):
;;   - Added food calorie mapping system
;;   - Added support for multiple measurement units per food
;; Version 1.0.8 (2024-10-30):
;;   - Pulled out problematic keymap calls out of the file; keymap is now not our problem
;; Version 1.0.7 (2024-10-27):
;;   - Fixed day name formatting in calc-cal-insert-date
;; Version 1.0.6 (2024-10-27):
;;   - Added calc-cal-insert-date function
;;   - Added keybinding "SPC c i" for date insertion
;;   - Added date insertion to minor mode keymap
;; Version 1.0.5 (2024-10-25):
;;   - Added MIT License declaration
;;   - Added AI assistance disclosure
;; Version 1.0.4 (2024-10-18):
;;   - Added support for basic arithmetic (multiplication and addition)
;;   - Fixed string-to-number error in calc-cal-evaluate-expression
;; Version 1.0.3 (2024-10-16):
;;   - Added revision history section
;;   - Updated keybinding to "SPC c a t"
;; Version 1.0.2 (2024-10-16):
;;   - Changed keybinding from "SPC c c" to "SPC c a t"
;; Version 1.0.1 (2024-10-16):
;;   - Implemented total removal and recalculation to prevent duplicate totals
;; Version 1.0.0 (2024-10-15):
;;   - Initial release
;;   - Basic
;;; Code:

(eval-when-compile
  (require 'evil nil t))

(defvar calc-cal-expression-regex
  (concat "^\\("                     ; start of line and capture start
          "\\([0-9]+\\)"            ; first number
          "\\(?:"                   ; optional non-capturing group
          "\\([x+]\\)"             ; operator
          "\\([0-9]+\\)"           ; second number
          "\\)?"                    ; end of optional group
          "\\)")                    ; end main capture
  "Regex based on EBNF grammar for food entries.
Captures: full expr, first number, operator (if any), second number (if any)")

(defvar calc-cal-date-regex
  "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-\\w+\\)>"
  "Regex for matching date entries in the format <YYYY-MM-DD Day>.")

(defvar calc-cal-total-regex
  "\\s-*\\([0-9]+\\)\\s-*$"
  "Regex for matching totals at the end of date lines.")

(defun calc-cal-evaluate-expression (expr)
  "Evaluate arithmetic expression in EXPR.
Returns a number based on:
- Plain number -> return as is
- NxN -> multiply numbers
- N+N -> add numbers"
  (if (string-match calc-cal-expression-regex expr)
      (let* ((first-num (string-to-number (match-string 2 expr)))
             (operator (match-string 3 expr))
             (second-num (when operator
                           (string-to-number (match-string 4 expr)))))
        (cond
         ;; No operator - return first number
         ((null operator) first-num)
         ;; Multiplication
         ((string= operator "x") (* first-num second-num))
         ;; Addition
         ((string= operator "+") (+ first-num second-num))
         ;; Default case - shouldn't happen with our grammar
         (t first-num)))
    ;; If no match, return 0 or handle error as needed
    0))

(defun calc-cal-insert-date ()
  "Insert current date in the format <YYYY-MM-DD Day> at point.
If point is not at the beginning of a line, a newline will be inserted first."
  (interactive)
  (let ((date-string (format-time-string "<%Y-%m-%d %a>" (current-time))))
    (unless (bolp) (insert "\n"))
    (insert date-string "\n")))

(defun calc-cal ()
  "Process the current buffer, calculating totals for food entries.
Handles basic arithmetic expressions (multiplication and addition)
in the format: NUMBER [OPERATOR NUMBER] DESCRIPTION
where OPERATOR can be 'x' or '+'."
  (interactive)
  (save-excursion
    (message "Entering cal-cal")
    (goto-char (point-min))
    (let (current-date
          current-total)
      ;; First pass: Remove existing totals
      (while (re-search-forward calc-cal-date-regex nil t)
        (when (looking-at calc-cal-total-regex)
          (delete-region (match-beginning 0) (match-end 0))))
      (message "First pass done")
      ;; Second pass: Calculate and insert new totals
      (goto-char (point-min))
      (while (re-search-forward calc-cal-date-regex nil t)
        (setq current-date (match-string 1))
        (setq current-total 0)
        (forward-line)
        (while (and (not (looking-at calc-cal-date-regex))
                    (not (eobp)))
          (when (looking-at calc-cal-expression-regex)
            (setq current-total
                  (+ current-total
                     (calc-cal-evaluate-expression (match-string 0)))))
          (forward-line))
        (message "Third pass done")
        (save-excursion
          (search-backward current-date)
          (end-of-line)
          (insert (format " %d" current-total)))))))

;; minor-mode setup
(define-minor-mode calc-cal-fud-mode
  "Minor mode for .fud files with automatic calorie calculation on save."
  :lighter " FUD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'calc-cal)
            (define-key map (kbd "C-c C-d") #'calc-cal-insert-date)
            map)
  (if calc-cal-fud-mode
      (add-hook 'after-save-hook #'calc-cal nil t)
    (remove-hook 'after-save-hook #'calc-cal t)))

(add-to-list 'auto-mode-alist '("\\.fud\\'" . calc-cal-fud-mode))

(defun calc-cal-fud-mode-maybe ()
  "Enable calc-cal-fud-mode if the file extension is .fud."
  (when (and (stringp buffer-file-name)
             (string-match "\\.fud\\'" buffer-file-name))
    (calc-cal-fud-mode 1)))

(add-hook 'find-file-hook #'calc-cal-fud-mode-maybe)

;; Only set up Doom keybindings if we're in Doom Emacs
;; (with-eval-after-load 'evil
;;   (when (fboundp 'map!)
;;     (map! :leader
;;           :desc "Calculate calorie totals"
;;           "c t" #'calc-cal)
;;     (map! :leader
;;           :desc "Insert current date"
;;           "c i" #'calc-cal-insert-date)))


(provide 'calc-cal)
;;; calc-cal.el ends here
