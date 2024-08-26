;;; hl-sentence-long-lines.el --- highlights sentences are long
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/captainflasmr/hl-sentence-long-lines
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This Emacs package provides highlighting for the current sentence
;; and optionally highlights sentences that exceed a customizable
;; word limit and is heavily based on =hl-sentence= but with a few
;; additional features.

;;; Code:

(require 'transient)

(defgroup hl-sentence-long-lines nil
  "Highlight the current sentence."
  :group 'convenience)

;;;###autoload
(defface hl-sentence-long-lines '((t :inherit highlight))
  "The face used to highlight the current sentence."
  :group 'hl-sentence-long-lines)

(defface hl-sentence-long-lines-face '((t :inherit font-lock-warning-face))
  "Face used to highlight sentences that exceed the word limit."
  :group 'hl-sentence-long-lines)

(defcustom hl-sentence-long-lines-word-limit 20
  "The word limit for highlighting long sentences."
  :type 'integer
  :group 'hl-sentence-long-lines)

(defcustom hl-sentence-long-lines-word-count-toggle t
  "When non-nil, the `hl-sentence-long-lines-word-count-toggle` is active."
  :type 'boolean
  :group 'hl-sentence-long-lines)

(defcustom hl-sentence-long-lines-highlighting-toggle t
  "When non-nil, the `hl-sentence-long-lines-highlighting-toggle` is active."
  :type 'boolean
  :group 'hl-sentence-long-lines)

(defcustom hl-sentence-long-lines-sentence-toggle t
  "When non-nil, the `hl-sentence-long-lines-sentence-toggle` is active."
  :type 'boolean
  :group 'hl-sentence-long-lines)

(defun hl-sentence-long-lines-begin-pos ()
  "Return the point of the beginning of a sentence."
  (save-excursion
    (unless (= (point) (point-max))
      (forward-char))
    (backward-sentence)
    (point)))

(defun hl-sentence-long-lines-end-pos ()
  "Return the point of the end of a sentence."
  (save-excursion
    (unless (= (point) (point-max))
      (forward-char))
    (backward-sentence)
    (forward-sentence)
    (point)))

(defun hl-sentence-long-lines-word-count ()
  "Return the number of words in the current sentence."
  (let ((sentence (buffer-substring-no-properties
                   (hl-sentence-long-lines-begin-pos)
                   (hl-sentence-long-lines-end-pos))))
    (- (length (split-string sentence "\\W+")) 1)))

(defvar hl-sentence-long-lines-extent nil
  "The location of the hl-sentence-long-lines-mode overlay.")

(defvar-local hl-sentence-long-lines-word-count-str ""
  "String to display the current sentence word count in the mode line.")

(defun hl-sentence-long-lines-update-word-count ()
  "Update the modeline with the current sentence word count."
  (setq hl-sentence-long-lines-word-count-str
        (format " Words: %d" (hl-sentence-long-lines-word-count)))
  (force-mode-line-update))

(defun hl-sentence-long-lines-highlight-long-sentence ()
  "Highlight sentences in buffer `hl-sentence-long-lines-word-limit`."
  (remove-overlays nil nil 'highlight-long-sentences t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[.!?:\n]" nil t)
      (let ((my-end (point)))
        (backward-sentence)
        (let ((my-start (point)))
          (save-excursion
            (goto-char my-start)
            (let ((word-count 0))
              (while (and (< (point) my-end)
                          (re-search-forward "\\w+" my-end t))
                (setq word-count (1+ word-count)))
              (when (>= word-count hl-sentence-long-lines-word-limit)
                (highlight-region my-start my-end 'hl-sentence-long-lines-face)))))
        (goto-char my-end)))))

(defun highlight-region (my-start my-end face)
  "Highlight region from MY-START to MY-END with FACE. Tag the overlay."
  (let ((ov (make-overlay my-start my-end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'highlight-long-sentences t)))

 (defun hl-sentence-long-lines-toggle-word-count ()
    "Toggle word count on modeline."
    (interactive)
    (setq hl-sentence-long-lines-word-count-toggle
          (not hl-sentence-long-lines-word-count-toggle))
    (force-mode-line-update))

 (defun hl-sentence-long-lines-toggle-highlighting ()
    "Toggle sentence long highlighting."
    (interactive)
    (setq hl-sentence-long-lines-highlighting-toggle
          (not hl-sentence-long-lines-highlighting-toggle)))

 (defun hl-sentence-long-lines-toggle-sentence ()
    "Toggle the sentence highlighting."
    (interactive)
    (setq hl-sentence-long-lines-sentence-toggle
          (not hl-sentence-long-lines-sentence-toggle)))

;;;###autoload
(define-minor-mode hl-sentence-long-lines-mode
  "Enable highlighting of the current sentence."
  :init-value nil
  :lighter hl-sentence-long-lines-word-count-str
  (if hl-sentence-long-lines-mode
      (add-hook 'post-command-hook 'hl-sentence-long-lines-current nil t)
    (move-overlay hl-sentence-long-lines-extent 0 0 (current-buffer))
    (remove-overlays nil nil 'highlight-long-sentences t)
    (remove-hook 'post-command-hook 'hl-sentence-long-lines-current t)))

(defun hl-sentence-long-lines-current ()
  "Highlight the current sentence and update word count.
Also highlight if the sentence exceeds `hl-sentence-long-lines-word-limit`."
  (and hl-sentence-long-lines-mode (> (buffer-size) 0)
       (boundp 'hl-sentence-long-lines-extent)
       hl-sentence-long-lines-extent
       (let ((beg (hl-sentence-long-lines-begin-pos))
             (end (hl-sentence-long-lines-end-pos)))
         (if hl-sentence-long-lines-sentence-toggle
             (move-overlay hl-sentence-long-lines-extent beg end (current-buffer))
           (remove-overlays nil nil 'highlight-sentences t))
         (if hl-sentence-long-lines-word-count-toggle
             (hl-sentence-long-lines-update-word-count)
           (setq hl-sentence-long-lines-word-count-str ""))
         (if hl-sentence-long-lines-highlighting-toggle
             (hl-sentence-long-lines-highlight-long-sentence)
               (remove-overlays nil nil 'highlight-long-sentences t)))))

(setq hl-sentence-long-lines-extent (make-overlay 0 0))
(overlay-put hl-sentence-long-lines-extent 'face 'hl-sentence-long-lines)
(overlay-put hl-sentence-long-lines-extent 'highlight-sentences t)

(transient-define-prefix hl-sentence-long-lines-transient ()
  "Sentence Highlighting Transient Commands."
  ["Sentence Highlighting"
   ("w" "Toggle word count in modeline" hl-sentence-long-lines-toggle-word-count)
   ("l" "Toggle long sentence highlighting" hl-sentence-long-lines-toggle-highlighting)
   ("s" "Toggle sentence highlighting" hl-sentence-long-lines-toggle-sentence)])

(provide 'hl-sentence-long-lines)

;;; hl-sentence-long-lines.el ends here
