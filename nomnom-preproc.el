;;; nomnom-preproc.el --- Preprocessor for food entry interpretation -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Preprocessor for nomnom that handles complex calorie specifications
;; including units like grams, liters, etc.

;;; Code:

(defvar nomnom-preproc-unit-regex
  "^\\([0-9]+\\)\\([[:alpha:]]+\\)\\s-"
  "Regex for matching entries starting with number+unit format.")

(defun nomnom-preproc-parse-first-token (str)
  "Parse first token of STR, return nil if not a pure number."
  (when (string-match "^\\([0-9]+\\)\\s-" str)
    (string-to-number (match-string 1 str))))

(defun nomnom-preproc-mark-interpretable ()
  "Mark lines that need interpretation with [I].
Marks lines where:
1. First token is a number less than 20 (assumed to be count of items)
2. First token matches number+unit format (e.g., '100g', '200ml')"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
             (first-num (nomnom-preproc-parse-first-token line)))

        ;; Skip date lines and empty lines
        (when (and (not (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" line))
                   (not (string= "" (string-trim line))))

          (cond
           ;; Case 1: Number less than 20 (assumed to be item count)
           ((and first-num (< first-num 20))
            (beginning-of-line)
            (insert "[I] "))

           ;; Case 2: Number+unit format
           ((string-match-p nomnom-preproc-unit-regex line)
            (beginning-of-line)
            (insert "[I] "))

           ;; Default case: do nothing
           (t nil)))

        (forward-line 1)))))

(provide 'nomnom-preproc)
;;; nomnom-preproc.el ends here
