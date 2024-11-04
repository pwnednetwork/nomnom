;;; nomnom.el --- Track and calculate meal entries -*- lexical-binding: t; -*-

;;; Licensed under MIT License

;; Author: agent314
;; Version: 0.0.11
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/x-agent314/nomnom

;;; ╔══════════════════════════════════════╗
;;; ║    < RUNTIME::INIT_SEQUENCE >        ║
;;; ╚══════════════════════════════════════╝
;;;
;;; NOTICE: Software generated with Claude 3.5 Sonnet AI assistance and human review.
;;;
;;; PROVIDED "AS IS" WITHOUT WARRANTY. AUTHORS NOT LIABLE FOR ANY DAMAGES OR CLAIMS
;;; ARISING FROM USE OR IMPLEMENTATION.

;;; Commentary:
;;; NomNom is a powerful package for tracking and analyzing meal entries.
;;; It processes text files containing dated entries with numerical values,
;;; making it perfect for calorie tracking, meal planning, and food journaling.

;;; Revision History:

;; Version 0.0.11 (2024-11-04):
;;   - Changed versions
;; Version 0.0.10 (2024-11-02):
;;   - Complete rebranding from calc-cal to nomnom
;;   - Renamed all functions and variables for consistency
;;   - Changed file extension from .fud to .nom
;;   - Updated mode name and configuration
;; Version 0.0.9 (2024-10-30):
;;   - Added food calorie mapping system
;;   - Added support for multiple measurement units per food
;; Version 0.0.8 (2024-10-30):
;;   - Pulled out problematic keymap calls out of the file
;; Version 0.0.7 (2024-10-27):
;;   - Fixed day name formatting in date insertion
;; Version 0.0.6 (2024-10-27):
;;   - Added date insertion function
;;   - Added keybinding for date insertion
;; Version 0.0.5 (2024-10-25):
;;   - Added MIT License declaration
;;   - Added AI assistance disclosure
;; Version 0.0.4 (2024-10-18):
;;   - Added support for basic arithmetic
;; Version 0.0.3 (2024-10-16):
;;   - Added revision history section
;; Version 0.0.2 (2024-10-16):
;;   - Updated keybindings
;; Version 0.0.1 (2024-10-16):
;;   - Implemented total removal and recalculation
;; Version 0.0.0 (2024-10-15):
;;   - Initial release

;;; Code:

(eval-when-compile
  (require 'evil nil t))

(defvar nomnom-entry-regex
  (concat "^\\("                     ; start of line and capture start
          "\\([0-9]+\\)"            ; first number
          "\\(?:"                   ; optional non-capturing group
          "\\([x+]\\)"             ; operator
          "\\([0-9]+\\)"           ; second number
          "\\)?"                    ; end of optional group
          "\\)")                    ; end main capture
  "Regex based on EBNF grammar for meal entries.
Captures: full expr, first number, operator (if any), second number (if any)")

(defvar nomnom-date-regex
  "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-\\w+\\)>"
  "Regex for matching date entries in the format <YYYY-MM-DD Day>.")

(defvar nomnom-total-regex
  "\\s-*\\([0-9]+\\)\\s-*$"
  "Regex for matching totals at the end of date lines.")

(defun nomnom-evaluate-meal (expr)
  "Evaluate meal entry expression in EXPR.
Returns a number based on:
- Plain number -> return as is
- NxN -> multiply numbers
- N+N -> add numbers"
  (if (string-match nomnom-entry-regex expr)
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

(defun nomnom-insert-date ()
  "Insert current date in the format <YYYY-MM-DD Day> at point.
If point is not at the beginning of a line, a newline will be inserted first."
  (interactive)
  (let ((date-string (format-time-string "<%Y-%m-%d %a>" (current-time))))
    (unless (bolp) (insert "\n"))
    (insert date-string "\n")))

(defun nomnom-calculate ()
  "Process the current buffer, calculating totals for meal entries.
Handles basic arithmetic expressions (multiplication and addition)
in the format: NUMBER [OPERATOR NUMBER] DESCRIPTION
where OPERATOR can be 'x' or '+'."
  (interactive)
  (save-excursion
    (message "Processing meal entries...")
    (goto-char (point-min))
    (let (current-date
          current-total)
      ;; First pass: Remove existing totals
      (while (re-search-forward nomnom-date-regex nil t)
        (when (looking-at nomnom-total-regex)
          (delete-region (match-beginning 0) (match-end 0))))
      (message "Existing totals cleared")
      
      ;; Second pass: Calculate and insert new totals
      (goto-char (point-min))
      (while (re-search-forward nomnom-date-regex nil t)
        (setq current-date (match-string 1))
        (setq current-total 0)
        (forward-line)
        (while (and (not (looking-at nomnom-date-regex))
                    (not (eobp)))
          (when (looking-at nomnom-entry-regex)
            (setq current-total
                  (+ current-total
                     (nomnom-evaluate-meal (match-string 0)))))
          (forward-line))
        (message "Processing entries for %s" current-date)
        (save-excursion
          (search-backward current-date)
          (end-of-line)
          (insert (format " %d" current-total)))))))

;; Define the minor mode
(define-minor-mode nomnom-mode
  "Minor mode for .nom files with automatic meal calculation on save."
  :lighter " NomNom"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'nomnom-calculate)
            (define-key map (kbd "C-c C-d") #'nomnom-insert-date)
            map)
  (if nomnom-mode
      (add-hook 'after-save-hook #'nomnom-calculate nil t)
    (remove-hook 'after-save-hook #'nomnom-calculate t)))

(add-to-list 'auto-mode-alist '("\\.nom\\'" . nomnom-mode))
(add-to-list 'auto-mode-alist '("\\.fud\\'" . nomnom-mode))

(defun nomnom-mode-maybe ()
  "Enable nomnom-mode if the file extension is .nom or .fud."
  (when (and (stringp buffer-file-name)
             (or (string-match "\\.nom\\'" buffer-file-name)
                 (string-match "\\.fud\\'" buffer-file-name)))
    (nomnom-mode 1)))






(add-hook 'find-file-hook #'nomnom-mode-maybe)

;; Note about keybindings:
;; To set up Doom Emacs keybindings, add to your config.el:
;;
;; (map! :leader
;;       :desc "Calculate meal totals"
;;       "c t" #'nomnom-calculate)
;;
;; (map! :leader
;;       :desc "Insert current date"
;;       "c i" #'nomnom-insert-date)

(provide 'nomnom)
;;; nomnom.el ends here
